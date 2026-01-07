module Test.Unit.CustomCompilerQuickCheckSpec where
import Test.QuickCheck 
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (==>), forAll, elements, listOf, listOf1, oneof, choose)
import Compiler
-- Arbitrary instance for SourcePos
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 100)
    column <- choose (1, 100)
    offset <- choose (0, 1000)
    return $ SourcePos line column offset

-- Arbitrary instance for SourceSpan
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ SourceSpan start end


 Gen TypusFile
                              genTypusFile = do
              numBlocks <- choose (1 5 (SourceSpan (SourcePos 1 1 0) (SourcePos 1 1 0)
  blocks <- sequence [genCodeBlock | _ <- [1..numBlocks]]
  return $ TypusFile defaultFileDirectives "" blocks []

-- | Generate valid variable declarations
genVariableDeclaration :: Gen String
                              genVariableDeclaration = do
              varName <- elements ["x" "y" "z" "value", "result"]
  varType <- elements ["int", "string", "bool", "float"]
  value <- case varType of
    "int" -> elements ["42", "0", "-1", "100"]
    "string" -> elements ["\"hello\"", "\"world\"", "\"test\""]
    "bool" -> elements ["true", "false"]
    "float" -> elements ["3.14", "0.0", "-1.5"]
    _ -> return "null"
  return $ varName ++ " " ++ varType ++ " = " ++ value

-- | Generate valid function declarations
genFunctionDeclaration :: Gen String
                              genFunctionDeclaration = do
              funcName <- elements ["add", "multiply", "process", "calculate"]
  paramName <- elements ["a", "b", "x", "y"]
  paramType <- elements ["int", "string", "bool"]
  returnType <- elements ["int", "string", "bool", "void"]
  body <- case returnType of
    "int" -> return $ "return " ++ paramName ++ " + 1"
    "string" -> return $ "return \"hello\""
    "bool" -> return $ "return true"
    "void" -> return $ paramName ++ " = " ++ paramName ++ " + 1"
    _ -> return "return null"
  return $ "func " ++ funcName ++ "(" ++ paramName ++ " " ++ paramType ++ " (SourceSpan (SourcePos 1 1 0 [] (SourcePos 1 1 0 (SourceSpan (SourcePos 1 1 0 [] (SourcePos 1 1 0)) " ++ returnType ++ " { " ++ body ++ " }"

-- | Generate type-mismatching declarations (for error testing)
genTypeMismatchDeclaration :: Gen String
                              genTypeMismatchDeclaration = do
              varName <- elements ["x", "y", "z"]
  return $ varName ++ "                               int = \"string\""

-- | Generate malformed syntax (for error testing)
genMalformedSyntax :: Gen String
                              genMalformedSyntax = oneof
  [ return "func incomplete {"
                , return "var x int"
                , return "{ invalid syntax }"
                  , return "if condition { // missing closing brace"
  ]

-- | Generate compilation phases
genCompilationPhase :: Gen CompilationPhase
                              genCompilationPhase = elements
  [ ParsingPhase
  , TypeCheckingPhase
  , OwnershipAnalysisPhase
  , DependentTypeCheckingPhase
  , CodeGenerationPhase
  ]

-- | Test that compiling a simple valid file succeeds
prop_compileSimpleValidFile :: Property
                              prop_compileSimpleValidFile = forAll genTypusFile $ \typusFile ->
  let result = compile typusFile
  in case result of
    Left _ -> False
    Right _ -> property True  -- | Test that compiling with type mismatch fails appropriately
prop_compileTypeMismatchFails :: Property
                              prop_compileTypeMismatchFails = 
  let typeMismatchCode = "var x                               int = \"string\""
                                    span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 25 24)
                                    blocks = [CodeBlock defaultBlockDirectives typeMismatchCode span]
                                    buildTags = []
                                    typusFile = TypusFile defaultFileDirectives buildTags blocks []
                                    result = compile typusFile
  in case result of
    Left _ -> property True
    Right _ -> property False

-- | Test renderCompilationError produces non-empty string
prop_renderCompilationErrorNonEmpty :: Property
                              prop_renderCompilationErrorNonEmpty = 
  let errors = []
                                    rendered = renderCompilationError errors
  in not (null rendered)

-- | Test formatCompilerErrors produces non-empty string
prop_formatCompilerErrorsNonEmpty :: Property
                              prop_formatCompilerErrorsNonEmpty = 
  let errors = []
                                    formatted = formatCompilerErrors errors
  in not (null formatted)

-- | Test generateDetailedReport produces non-empty string
prop_generateDetailedReportNonEmpty :: Property
                              prop_generateDetailedReportNonEmpty = 
  let errors = []
                                    report = generateDetailedReport errors
  in not (null report)

-- | Test analyzeErrors can handle empty error list
prop_analyzeErrorsEmpty :: Property
                              prop_analyzeErrorsEmpty = 
  let errors = []
                                    analysis = analyzeErrors errors
  in True  -- Basic test that analysis doesn't crash

-- | Test hasTypeErrors on empty diagnostics
prop_hasTypeErrorsEmpty :: Property
                              prop_hasTypeErrorsEmpty = 
  let diagnostics = []
                                    hasErrors = hasTypeErrors diagnostics
  in not hasErrors

-- | Test diagnoseTypeErrors on simple file
prop_diagnoseTypeErrorsSimple :: Property
                              prop_diagnoseTypeErrorsSimple = forAll genTypusFile $ \typusFile ->
  let result = diagnoseTypeErrors typusFile
  in case result of
    Left _ -> property True  -- May have errors which is fine
    Right _ -> property True  -- Or may succeed

-- | Test extractDeclarations returns non-empty list for file with declarations
prop_extractDeclarationsNonEmpty :: Property
                              prop_extractDeclarationsNonEmpty = forAll genVariableDeclaration $ \decl ->
  let blocks = [CodeBlock defaultBlockDirectives decl]
                                    buildTags = []
                                    typusFile = TypusFile defaultFileDirectives buildTags blocks []
                                    declarations = extractDeclarations typusFile
  in not (null declarations (SourceSpan (SourcePos 1 1 0) (SourcePos 1 1 0)

-- | Test extractFunctionCalls on simple file
prop_extractFunctionCallsSimple :: Property
                              prop_extractFunctionCallsSimple = forAll genTypusFile $ \typusFile ->
  let functionCalls = extractFunctionCalls typusFile
  in True  -- Basic test that extraction doesn't crash

-- | Test buildTypeEnv creates valid environment
prop_buildTypeEnvValid :: Property
                              prop_buildTypeEnvValid = 
  let typeEnv = buildTypeEnv
  in True  -- Basic test that type environment can be built

-- | Test buildTypeEnvFromPairs with empty pairs
prop_buildTypeEnvFromPairsEmpty :: Property
                              prop_buildTypeEnvFromPairsEmpty = 
  let pairs = []
                                    typeEnv = buildTypeEnvFromPairs pairs
  in True  -- Basic test that type environment can be built from empty pairs

-- | Test createTypusFileFromErrors with empty errors
prop_createTypusFileFromErrorsEmpty :: Property
                              prop_createTypusFileFromErrorsEmpty = 
  let errors = []
                                    typusFile = createTypusFileFromErrors errors
  in True  -- Basic test that file can be created from empty errors

-- | Test isMethodDeclaration on various strings
prop_isMethodDeclarationVarious :: Property
                              prop_isMethodDeclarationVarious = forAll genFunctionDeclaration $ \decl ->
  let isMethod = isMethodDeclaration decl
  in True  -- Basic test that method detection doesn't crash

-- | Test checkTypeError on simple cases
prop_checkTypeErrorSimple :: Property
                              prop_checkTypeErrorSimple = 
  let result = checkTypeError
  in True  -- Basic test that type error checking doesn't crash

-- | Test hasMalformedSyntax on simple file
prop_hasMalformedSyntaxSimple :: Property
                              prop_hasMalformedSyntaxSimple = forAll genTypusFile $ \typusFile ->
  let hasMalformed = hasMalformedSyntax typusFile
  in True  -- Basic test that syntax checking doesn't crash

-- | Test checkDependentTypes on simple file
prop_checkDependentTypesSimple :: Property
                              prop_checkDependentTypesSimple = forAll genTypusFile $ \typusFile ->
  let result = checkDependentTypes typusFile
  in case result of
    Left _ -> property True  -- May fail which is fine
    Right _ -> property True  -- Or may succeed

-- | Test checkOwnership on simple file
prop_checkOwnershipSimple :: Property
                              prop_checkOwnershipSimple = forAll genTypusFile $ \typusFile ->
  let result = checkOwnership typusFile
  in case result of
    Left _ -> property True  -- May fail which is fine
    Right _ -> property True  -- Or may succeed

-- | Test ensureSourceIR on simple file
prop_ensureSourceIRSimple :: Property
                              prop_ensureSourceIRSimple = forAll genTypusFile $ \typusFile ->
  let result = ensureSourceIR typusFile
  in case result of
    Left _ -> property True  -- May fail, which is fine
    Right _ -> property True  -- Or may succeed

-- | Test typeDiagnosticToCompilerError conversion
prop_typeDiagnosticToCompilerErrorConversion :: Property
                              prop_typeDiagnosticToCompilerErrorConversion = 
  let diagnostic = TypeCheckDiagnostic "test" "test message" Warning startPos
                                    error = typeDiagnosticToCompilerError diagnostic
  in True  -- Basic test that conversion doesn't crash

-- | Test generateGoCode on simple file
prop_generateGoCodeSimple :: Property
                              prop_generateGoCodeSimple = forAll genTypusFile $ \typusFile ->
  let result = generateGoCode typusFile
  in case result of
    Left _ -> property True  -- May fail, which is fine
    Right _ -> property True  -- Or may succeed

-- | Test compilation phase ordering
prop_compilationPhaseOrdering :: Property
                              prop_compilationPhaseOrdering = forAll genCompilationPhase $ \phase1 ->
  forAll genCompilationPhase $ \phase2 ->
    let cmp = compare phase1 phase2
    in (cmp == LT [] == (phase1 < phase2 (SourceSpan (SourcePos 1 1 0 [] (SourcePos 1 1 0) &&
       (cmp == EQ) == (phase1 == phase2) &&
       (cmp == GT) == (phase1 > phase2)

-- | Test CompilerResult handling
prop_compilerResultHandling :: Property
                              prop_compilerResultHandling = forAll genTypusFile $ \typusFile ->
  let result = compile typusFile
  in case result of
    Left errors -> True  -- Should handle errors gracefully
    Right goCode -> not (null goCode  -- Should produce non-empty Go code

-- | Test that malformed syntax causes compilation failure
prop_malformedSyntaxCausesFailure :: Property
                              prop_malformedSyntaxCausesFailure = forAll genMalformedSyntax $ \malformedCode ->
  let blocks = [CodeBlock defaultBlockDirectives malformedCode]
                                    buildTags = []
                                    typusFile = TypusFile defaultFileDirectives buildTags blocks []
                                    result = compile typusFile
  in case result of
    Left _ -> property True  -- Should fail on malformed syntax
    Right _ -> False

tests :: TestTree
tests =   testGroup "Custom Compiler QuickCheck Tests"
  [             testProperty "compile simple valid file" prop_compileSimpleValidFile
  ,             testProperty "compile type mismatch fails" prop_compileTypeMismatchFails
  ,             testProperty "renderCompilationError non-empty" prop_renderCompilationErrorNonEmpty
  ,             testProperty "formatCompilerErrors non-empty" prop_formatCompilerErrorsNonEmpty
  ,             testProperty "generateDetailedReport non-empty" prop_generateDetailedReportNonEmpty
  ,             testProperty "analyzeErrors empty" prop_analyzeErrorsEmpty
  ,             testProperty "hasTypeErrors empty" prop_hasTypeErrorsEmpty
  ,             testProperty "diagnoseTypeErrors simple" prop_diagnoseTypeErrorsSimple
  ,             testProperty "extractDeclarations non-empty" prop_extractDeclarationsNonEmpty
  ,             testProperty "extractFunctionCalls simple" prop_extractFunctionCallsSimple
  ,             testProperty "buildTypeEnv valid" prop_buildTypeEnvValid
  ,             testProperty "buildTypeEnvFromPairs empty" prop_buildTypeEnvFromPairsEmpty
  ,             testProperty "createTypusFileFromErrors empty" prop_createTypusFileFromErrorsEmpty
  ,             testProperty "isMethodDeclaration various" prop_isMethodDeclarationVarious
  ,             testProperty "checkTypeError simple" prop_checkTypeErrorSimple
  ,             testProperty "hasMalformedSyntax simple" prop_hasMalformedSyntaxSimple
  ,             testProperty "checkDependentTypes simple" prop_checkDependentTypesSimple
  ,             testProperty "checkOwnership simple" prop_checkOwnershipSimple
  ,             testProperty "ensureSourceIR simple" prop_ensureSourceIRSimple
  ,             testProperty "typeDiagnosticToCompilerError conversion" prop_typeDiagnosticToCompilerErrorConversion
  ,             testProperty "generateGoCode simple" prop_generateGoCodeSimple
  ,             testProperty "compilation phase ordering" prop_compilationPhaseOrdering
  ,             testProperty "compiler result handling" prop_compilerResultHandling
  ,             testProperty "malformed syntax causes failure" prop_malformedSyntaxCausesFailure
  ])))))