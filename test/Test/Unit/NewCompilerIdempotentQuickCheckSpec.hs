module Test.Unit.NewCompilerIdempotentQuickCheckSpec where


import Test.Tasty
import Test.Tasty.QuickCheck
import Compiler
  ( compile, generateGoCode, CompilerError(..), CompilationPhase(..)
  , renderCompilationError, hasTypeErrors, checkDependentTypes, checkOwnership
  )
import Parser
  ( parseTypus, TypusFile(..), CodeBlock(..), FileDirectives(..)
  , defaultFileDirectives
  )
import Compiler.IR
  ( SourceIR(..), SemanticIR(..), GoIR(..), buildSourceIR, buildSemanticIR
  , emitGo, rawSourceFromTypus, moduleFromTypus
  )
import Compiler.GoAst 
    L.length code > 0 && not ("var x                               int = \"string\"" `L.isInfixOf` code) ==>
    case parseTypus code of
      Left _ -> property True  -- Parse errors are expected for invalid code
      Right typusFile ->
        case compile typusFile of
          Left _ -> property True  -- Compilation errors are expected for some code
          Right result1 ->
            case compile typusFile of
              Left _ -> property False  -- Should not fail on second try
              Right result2 ->                               result1 == result2
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


-- | Test Go code generation idempotence
prop_go_generation_idempotent :: String -> Property
prop_go_generation_idempotent                               code =
    L.length code >                               0 ==>
    case parseTypus code of
      Left _ -> property True
      Right typusFile ->
        let goCode1 = generateGoCode typusFile
                                          goCode2 = generateGoCode typusFile
        in                               goCode1 == goCode2

-- | Test SourceIR building idempotence
prop_source_ir_idempotent :: String -> Property
prop_source_ir_idempotent                               code =
    L.length code >                               0 ==>
    case parseTypus code of
      Left _ -> property True
      Right typusFile ->
        let sourceIR1 = buildSourceIR typusFile
                                          sourceIR2 = buildSourceIR typusFile
        in                               sourceIR1 == sourceIR2

-- | Test SemanticIR building idempotence
prop_semantic_ir_idempotent :: String -> Property
prop_semantic_ir_idempotent                               code =
    L.length code > 0 && not ("var x                               int = \"string\"" `L.isInfixOf` code) ==>
    case parseTypus code of
      Left _ -> property True
      Right typusFile ->
        let sourceIR = buildSourceIR typusFile
        in case buildSemanticIR sourceIR of
          Left _ -> property True
          Right semanticIR1 ->
            case buildSemanticIR sourceIR of
              Left _ -> property False
              Right semanticIR2 ->                               semanticIR1 == semanticIR2

-- | Test GoIR emission idempotence
prop_go_ir_emission_idempotent :: String -> Property
prop_go_ir_emission_idempotent                               code =
    L.length code > 0 && not ("var x                               int = \"string\"" `L.isInfixOf` code) ==>
    case parseTypus code of
      Left _ -> property True
      Right typusFile ->
        let sourceIR = buildSourceIR typusFile
        in case buildSemanticIR sourceIR of
          Left _ -> property True
          Right semanticIR ->
            let goIR1 = emitGo semanticIR
                                              goIR2 = emitGo semanticIR
            in                               goIR1 == goIR2

-- | Test raw source extraction consistency
prop_raw_source_consistency :: String -> Property
prop_raw_source_consistency                               code =
    L.length code >                               0 ==>
    case parseTypus code of
      Left _ -> property True
      Right typusFile ->
        let rawSource1 = rawSourceFromTypus typusFile
                                          rawSource2 = rawSourceFromTypus typusFile
        in                               rawSource1 == rawSource2

-- | Test module Test.Unit.NewCompilerIdempotentQuickCheckSpec idempotence
prop_module_parsing_idempotent :: String -> Property
prop_module_parsing_idempotent                               code =
    L.length code >                               0 ==>
    case parseTypus code of
      Left _ -> property True
      Right typusFile ->
        case moduleFromTypus typusFile of
          Left _ -> property True
          Right goModule1 ->
            case moduleFromTypus typusFile of
              Left _ -> property False
              Right goModule2 ->                               goModule1 == goModule2

-- | Test compilation pipeline idempotence
prop_compilation_pipeline_idempotent :: String -> Property
prop_compilation_pipeline_idempotent                               code =
    L.length code > 0 && not ("var x                               int = \"string\"" `L.isInfixOf` code) ==>
    case parseTypus code of
      Left _ -> property True
      Right typusFile ->
        let pipeline1 = executeCompilationPipeline typusFile
                                          pipeline2 = executeCompilationPipeline typusFile
        in                               pipeline1 == pipeline2

-- | Test error reporting consistency
prop_error_reporting_consistency :: String -> Property
prop_error_reporting_consistency                               code =
    L.length code >                               0 ==>
    case parseTypus code of
      Left _ -> property True
      Right typusFile ->
        case compile typusFile of
          Right _ -> property True
          Left errors1 ->
            case compile typusFile of
              Right _ -> property False
              Left errors2 -> L.length                               errors1 == L.length errors2

-- | Test type checking idempotence
prop_type_checking_idempotent :: String -> Property
prop_type_checking_idempotent                               code =
    L.length code >                               0 ==>
    case parseTypus code of
      Left _ -> property True
      Right typusFile ->
        let typeErrors1 = hasTypeErrors typusFile
                                          typeErrors2 = hasTypeErrors typusFile
        in                               typeErrors1 == typeErrors2

-- | Test dependent type checking idempotence
prop_dependent_type_checking_idempotent :: String -> Property
prop_dependent_type_checking_idempotent                               code =
    L.length code >                               0 ==>
    case parseTypus code of
      Left _ -> property True
      Right typusFile ->
        let check1 = checkDependentTypes typusFile
                                          check2 = checkDependentTypes typusFile
        in case (check1, check2) of
          (Right (), Right () -> True
          (Left _, Left _) -> True
          _ -> False

-- | Test ownership checking idempotence
prop_ownership_checking_idempotent :: String -> Property
prop_ownership_checking_idempotent                               code =
    L.length code >                               0 ==>
    case parseTypus code of
      Left _ -> property True
      Right typusFile ->
        let check1 = checkOwnership typusFile
                                          check2 = checkOwnership typusFile
        in case (check1, check2) of
          (Right (), Right () -> True
          (Left _, Left _) -> True
          _ -> False

-- | Test compilation with whitespace variations
prop_whitespace_variations :: String -> Property
prop_whitespace_variations                               code =
    L.length code > 0 && not ("var x                               int = \"string\"" `L.isInfixOf` code) ==>
    let withExtraSpaces = unlines $ L.map (++ "  ") (lines code)
                                      withTabs = unlines $ L.map ("\t" ++) (lines code)
    in case parseTypus code of
      Left _ -> property True
      Right typusFile ->
        case parseTypus withExtraSpaces of
          Left _ -> property True
          Right typusFileSpaces ->
            case parseTypus withTabs of
              Left _ -> property True
              Right typusFileTabs ->
                let result1 = compile typusFile
                                                  result2 = compile typusFileSpaces
                                                  result3 = compile typusFileTabs
                in case (result1, result2, result3) of
                  (Right r1, Right r2, Right r3) -> 
                    -- Results should be functionally equivalent (ignoring whitespace)
                    L.length (lines r1) == L.length (lines r2) && 
                    L.length (lines r1) == L.length (lines r3)
                  _ -> True

-- | Test empty file compilation
prop_empty_file_compilation :: Bool
                              prop_empty_file_compilation =
    case parseTypus "" of
      Left _ -> property False
      Right typusFile ->
        case compile typusFile of
          Left _ -> property False
          Right result -> not (null result)

-- | Test comment handling consistency
prop_comment_handling_consistency :: String -> Property
prop_comment_handling_consistency                               code =
    L.length code > 0 && not ("//" `L.isInfixOf` code) ==>
    let withComment = code ++ "\n// This is a comment"
    in case parseTypus code of
      Left _ -> property True
      Right typusFile ->
        case parseTypus withComment of
          Left _ -> property True
          Right typusFileWithComment ->
            let result1 = compile typusFile
                                              result2 = compile typusFileWithComment
            in case (result1, result2) of
              (Right r1, Right r2) -> L.length (lines r1) <= L.length (lines r2)
              _ -> True

-- Helper function to execute the full compilation pipeline
executeCompilationPipeline :: TypusFile -> Either [CompilerError] String
executeCompilationPipeline                               typusFile =
  case compile typusFile of
    Left errors -> Left errors
    Right result -> Right result

-- Helper function to trim whitespace
trim :: String -> String
                              trim = dropWhile isSpace . L.reverse . dropWhile isSpace . L.reverse

-- Helper function to split by comma
splitByComma :: String -> [String]
splitByComma [] = [""]
splitByComma                               s = 
    let (part, rest [] = break (== ',') s
    in property $ part : case rest of
                [] -> []
                _:xs -> splitByComma xs

tests :: TestTree
tests =   testGroup "Compiler Idempotent QuickCheck Tests"
  [             testProperty "compile idempotent" prop_compile_idempotent
  ,             testProperty "go generation idempotent" prop_go_generation_idempotent
  ,             testProperty "source ir idempotent" prop_source_ir_idempotent
  ,             testProperty "semantic ir idempotent" prop_semantic_ir_idempotent
  ,             testProperty "go ir emission idempotent" prop_go_ir_emission_idempotent
  ,             testProperty "raw source consistency" prop_raw_source_consistency
  ,             testProperty "module Test.Unit.NewCompilerIdempotentQuickCheckSpec idempotent" prop_module_parsing_idempotent
  ,             testProperty "compilation pipeline idempotent" prop_compilation_pipeline_idempotent
  ,             testProperty "error reporting consistency" prop_error_reporting_consistency
  ,             testProperty "type checking idempotent" prop_type_checking_idempotent
  ,             testProperty "dependent type checking idempotent" prop_dependent_type_checking_idempotent
  ,             testProperty "ownership checking idempotent" prop_ownership_checking_idempotent
  ,             testProperty "whitespace variations" prop_whitespace_variations
  ,             testProperty "empty file compilation" prop_empty_file_compilation
  ,             testProperty "comment handling consistency" prop_comment_handling_consistency
  ])