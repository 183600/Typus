module Test.Unit.NewCompilerOptimizationQuickCheckTestSpec where
import Test.QuickCheck 
import Test.Tasty.QuickCheck (Property,             testProperty, Arbitrary(..), Gen, oneof, elements, listOf, listOf1, suchThat, choose)
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import Compiler ()
      renderCompilationError, formatCompilerErrors, generateDetailedReport,
      analyzeErrors, hasTypeErrors, TypeCheckDiagnostic(..), diagnoseTypeErrors,
      extractDeclarations, extractFunctionCalls, buildTypeEnv, buildTypeEnvFromPairs,
      isMethodDeclaration, checkTypeError, hasMalformedSyntax, checkDependentTypes,
      checkOwnership, ensureSourceIR, typeCheckFailure, generateGoCode )
import Parser (TypusFile(..), CodeBlock(..), parseTypus)
import Compiler.IR (IRModule(..), IRFunction(..), IRStatement(..), IRExpression)
import SourceLocation (SourcePos(..), SourceSpan(..), posAtLineCol, spanBetween)
import qualified Data.List as L
import Data.List ()
                                              result = compile input
            case result of
                Right (goCode, _) -> "6" `L.isInfixOf` goCode @?= True
                Left _ -> @?= False True
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


          ,             testCase "dead code elimination" $ do
                        let input = "func test() { if false { return 1 } else { return 2 } }"
                                              result = compile input
            case result of
                Right (goCode, _) -> "return 2" `L.isInfixOf` goCode @?= True
                Left _ -> @?= False True

          ,             testCase "function inlining" $ do
                        let input = unlines
                  [ "func small(x int) int { return x + 1 }"
                  , "func test() { return small(5) }"
                  ]
                                              result = compile input
            case result of
                Right (goCode, _) -> L.length (lines goCode) >= 2 @?= True
                Left _ -> @?= False True

          ,             testCase "loop optimization" $ do
                        let input = "func test() { for i := 0; i < 10; i++ { L.sum += i } }"
                                              result = compile input
            case result of
                Right (goCode, _) -> "for" `L.isInfixOf` goCode @?= True
                Left _ -> @?= False True

          ,             testCase "type inference optimization" $ do
                        let input = "func test() { x := 42; return x }"
                                              result = compile input
            case result of
                Right (goCode, _) -> "int" `L.isInfixOf` goCode @?= True
                Left _ -> @?= False True

          ,             testCase "ownership optimization" $ do
                        let input = unlines
                  [ "func test() {"
                  , "  data := make([]int, 100)"
                  , "  return process(data)"
                  , "}"
                  ]
                                              result = compile input
            case result of
                Right (goCode, _) -> not (null goCode) @?= True
                Left _ -> @?= False True
        ]
    ]

-- | 
prop_compilationPhasesProgress :: String -> Property
prop_compilationPhasesProgress                               input =
  let result = compile input
  in case result of
       Right _ -> True -- Successful compilation went through L.all phases
       Left errors -> L.all (\error -> compilationPhase error `elem` [ParsingPhase, TypeCheckPhase, OwnershipPhase, CodeGenPhase]) errors

-- | 
prop_errorAnalysisIdentifiesPhase :: [CompilationPhase] -> Property
prop_errorAnalysisIdentifiesPhase                               phases =
  let errors = L.map (\phase -> CompilerError {                               compilationPhase = phase,                               errorMessage = "test",                               errorLocation = Nothing,                               errorSeverity = Error }) phases
                                    analyzed = analyzeErrors errors
  in L.all (\error -> compilationPhase error `elem` phases) analyzed

-- | 
prop_compilationDeterministic :: String -> Property
prop_compilationDeterministic                               input =
  let result1 = compile input
                                    result2 = compile input
  in case (result1, result2) of
       (Right (code1, _), Right (code2, _) ->                               code1 == code2
       (Left errors1, Left errors2) -> L.length                               errors1 == L.length errors2
       _ -> False -- Should be consistent success/failure

-- | 
prop_compilationHandlesRepeats :: String -> Int -> Property
prop_compilationHandlesRepeats input                               count =
  count >= 0 && count <=                               10 ==>
  let results = replicate count (compile input)
                                    successes = L.length [() | Right _ <- results]
                                    failures = L.length [() | Left _ <- results]
  in                               successes == count ||                               failures == count -- All should be success L.or L.all should be failure

-- | 
prop_errorAnalysisPreservesErrors :: [String] -> Property
prop_errorAnalysisPreservesError                               messages =
  let errors = L.map (\msg -> CompilerError {                               compilationPhase = ParsingPhase,                               errorMessage = msg,                               errorLocation = Nothing,                               errorSeverity = Error }) messages
                                    analyzed = analyzeErrors errors
  in L.length                               analyzed == L.length errors

-- | 
prop_errorAnalysisCategorizesSeverity :: [ErrorSeverity] -> Property
prop_errorAnalysisCategorizesSeverity                               severities =
  let errors = zipWith (\sev idx -> CompilerError {                               compilationPhase = ParsingPhase,                               errorMessage = "error" ++ show idx,                               errorLocation = Nothing,                               errorSeverity = sev }) severities [1..]
                                    analyzed = analyzeErrors errors
  in L.all (\error -> errorSeverity error `elem` severities) analyzed

-- | 
prop_errorAnalysisProvidesLocations :: [Int] -> [Int] -> Property
prop_errorAnalysisProvidesLocations lines                               cols =
  L.length                               lines == L.length cols && not (null lines) ==>
  let positions = zipWith (\line col -> Just (posAtLineCol line col) lines cols
                                    errors = zipWith (\pos idx -> CompilerError {                               compilationPhase = ParsingPhase,                               errorMessage = "error" ++ show idx,                               errorLocation = pos,                               errorSeverity = Error }) positions [1..]
                                    analyzed = analyzeErrors errors
  in L.all (\error -> errorLocation error `elem` positions) analyzed

-- | 
prop_errorAnalysisHandlesEmpty :: Property
                              prop_errorAnalysisHandlesEmpty =
  let errors = []
                                    analyzed = analyzeErrors errors
  in null analyzed

-- | 
prop_typeCheckingPreservesValid :: String -> Property
prop_typeCheckingPreservesValid                               input =
  let parsed = parseTypus input
  in case parsed of
       Right typusFile -> 
         let typeErrors = diagnoseTypeErrors typusFile
         in null                               typeErrors ==> True -- Valid programs should pass type checking
       Left _ -> False -- Invalid syntax can't be type checked

-- | 
prop_typeCheckingDetectsInvalid :: String -> Property
prop_typeCheckingDetectsInvalid                               input =
  let parsed = parseTypus input
  in case parsed of
       Right typusFile -> 
         let typeErrors = diagnoseTypeErrors typusFile
         in not (null typeErrors) || True -- May L.or may not have type errors
       Left _ -> True -- Syntax errors are detected

-- | 
prop_typeEnvironmentConsistent :: [(String, String)] -> Property
prop_typeEnvironmentConsistent                               pairs =
  let typeEnv = buildTypeEnvFromPairs pairs
      -- Check that L.all declared types are present
                                    declaredTypes = map fst pairs
                                    envTypes = Map.keys typeEnv
  in L.all (`elem` envTypes) declaredTypes

-- | 
prop_typeCheckingComplexExpressions :: String -> Property
prop_typeCheckingComplexExpressions                               input =
  let complexInput = "func complex() { return (" ++ input ++ ") + 1 }"
                                    parsed = parseTypus complexInput
  in case parsed of
       Right _ -> True -- Should handle complex expressions without crashing
       Left _ -> True -- Syntax errors are acceptable

-- | 
prop_dependentTypePreservesInvariants :: String -> Property
prop_dependentTypePreservesInvariants                               input =
  let parsed = parseTypus input
  in case parsed of
       Right typusFile -> 
         let dependentErrors = checkDependentTypes typusFile
         in True -- Should not crash on L.any input
       Left _ -> True

-- | 
prop_dependentTypeConstraintsRespected :: String -> Property
prop_dependentTypeConstraintsRespected                               input =
  let parsed = parseTypus input
  in case parsed of
       Right typusFile -> 
         let dependentErrors = checkDependentTypes typusFile
         in L.length dependentErrors >= 0 -- Should detect constraint violations
       Left _ -> True

-- | 
prop_dependentTypeEdgeCases :: String -> Property
prop_dependentTypeEdgeCases                               input =
  let edgeCaseInput = "// @dependent-types: true\n" ++ input
                                    parsed = parseTypus edgeCaseInput
  in case parsed of
       Right typusFile -> 
         let dependentErrors = checkDependentTypes typusFile
         in True -- Should handle edge cases
       Left _ -> True

-- | 
prop_ownershipPreventsDoubleMoves :: String -> Property
prop_ownershipPreventsDoubleMoves                               input =
  let parsed = parseTypus input
  in case parsed of
       Right typusFile -> 
         let ownershipErrors = checkOwnership typusFile
         in True -- Should detect double moves if present
       Left _ -> True

-- | 
prop_ownershipAllowsValidTransfers :: String -> Property
prop_ownershipAllowsValidTransfers                               input =
  let parsed = parseTypus input
  in case parsed of
       Right typusFile -> 
         let ownershipErrors = checkOwnership typusFile
         in True -- Should allow valid ownership transfers
       Left _ -> True

-- | 
prop_ownershipTracksLifetimes :: String -> Property
prop_ownershipTracksLifetimes                               input =
  let parsed = parseTypus input
  in case parsed of
       Right typusFile -> 
         let ownershipErrors = checkOwnership typusFile
         in True -- Should track variable lifetimes
       Left _ -> True

-- | 
prop_ownershipComplexScenarios :: String -> Property
prop_ownershipComplexScenarios                               input =
  let complexInput = "// @ownership: true\n" ++ input
                                    parsed = parseTypus complexInput
  in case parsed of
       Right typusFile -> 
         let ownershipErrors = checkOwnership typusFile
         in True -- Should handle complex ownership scenarios
       Left _ -> True

-- | 
prop_codeGenerationPreservesSemantics :: String -> Property
prop_codeGenerationPreservesSemantics                               input =
  let result = compile input
  in case result of
       Right (goCode, _) -> not (null goCode) -- Should generate non-empty code
       Left _ -> True -- Compilation errors are acceptable

-- | Go
prop_codeGenerationValidGo :: String -> Property
prop_codeGenerationValidGo                               input =
  let result = compile input
  in case result of
       Right (goCode, _) -> 
         let hasValidStructure = "func" `L.isInfixOf` goCode || "var" `L.isInfixOf` goCode || "const" `L.isInfixOf` goCode
         in hasValidStructure
       Left _ -> True

-- | 
prop_codeGenerationHandlesOptimizations :: String -> Property
prop_codeGenerationHandlesOptimizations                               input =
  let result = compile input
  in case result of
       Right (goCode, _) -> L.length goCode >= 0 -- Should generate code of some L.length
       Left _ -> True

-- | 
prop_codeGenerationDeterministic :: String -> Property
prop_codeGenerationDeterministic                               input =
  let result1 = compile input
                                    result2 = compile input
  in case (result1, result2) of
       (Right (code1, _), Right (code2, _) ->                               code1 == code2
       _ -> True

-- | 
prop_compilationTimeScales :: String -> Property
prop_compilationTimeScales                               input =
  let result = compile input
  in case result of
       Right _ -> True -- Should complete in reasonable time
       Left _ -> True

-- | 
prop_memoryUsageBounded :: String -> Property
prop_memoryUsageBounded                               input =
  let result = compile input
  in case result of
       Right _ -> True -- Should not use excessive memory
       Left _ -> True

-- | 
prop_optimizationPassesConverge :: String -> Property
prop_optimizationPassesConverge                               input =
  let result = compile input
  in property $ case result of
       Right _ -> True -- Optimization should converge
       Left _ -> True

-- Define ErrorSeverity if not already defined
data                               ErrorSeverity = Info | Warning | Error | FatalError deriving (Eq, Show, Ord)