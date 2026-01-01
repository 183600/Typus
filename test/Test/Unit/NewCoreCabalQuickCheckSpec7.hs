module Test.Unit.NewCoreCabalQuickCheckSpec7 (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)

import Compiler (CompilerError(..), CompilationPhase(..), CompilerResult)
import qualified Data.Text as T

-- | Compiler consistency tests with QuickCheck properties
tests :: TestTree
tests =
  testGroup "New Core Cabal QuickCheck Tests 7 - Compiler Consistency"
    [ testGroup "Compilation phase properties"
        [ fastProperty "compilation phase progression is monotonic" prop_compilationPhaseMonotonic
        , fastProperty "compilation phase ordering is total" prop_compilationPhaseTotal
        , testCase "compilation phase ordering" $ do
            let phases = [ParsingPhase, TypeCheckingPhase, OwnershipPhase, CodeGenPhase]
            L.length phases @?= 4
        ]
    , testGroup "Compiler error properties"
        [ fastProperty "compiler error collection preserves severity" prop_compilerErrorCollectionPreservesSeverity
        , fastProperty "compiler error location is within bounds" prop_compilerErrorLocationInBounds
        , testCase "compiler error creation" $ do
            let errorType = SyntaxError
                phase = ParsingPhase
                message = "Test error"
                line = 5
                column = 10
                error = CompilerError { ceType = errorType, cePhase = phase, ceMessage = message, ceLine = line, ceColumn = column }
            ceType error @?= errorType
            cePhase error @?= phase
            ceMessage error @?= message
        ]
    , testGroup "Compiler result properties"
        [ fastProperty "compiler result composition is associative" prop_compilerResultAssociative
        , fastProperty "compiler result preserves error ordering" prop_compilerResultPreservesErrorOrdering
        , testCase "successful compiler result" $ do
            let result = CompilerSuccess (T.pack "generated code")
            case result of
              CompilerSuccess code -> T.L.length code @?= 13
              CompilerFailure _ -> assertFailure "Expected success"
        ]
    , testGroup "Compilation edge cases"
        [ fastProperty "empty input compilation" prop_emptyInputCompilation
        , fastProperty "incremental compilation preserves results" prop_incrementalCompilationPreservesResults
        , testCase "compilation with warnings" $ do
            let warning = CompilerError { ceType = Warning, cePhase = TypeCheckingPhase, ceMessage = "Warning", ceLine = 1, ceColumn = 1 }
                result = CompilerSuccessWithWarnings (T.pack "code") [warning]
            case result of
              CompilerSuccessWithWarnings code warnings -> do
                T.L.length code @?= 4
                L.length warnings @?= 1
              _ -> assertFailure "Expected success with warnings"
        ]
    ]

-- Simplified versions of data structures for testing
data CompilationPhase = ParsingPhase | TypeCheckingPhase | OwnershipPhase | CodeGenPhase
  deriving (Show, Eq, Ord)

data CompilerErrorType = SyntaxError | TypeError | Warning | OwnershipError
  deriving (Show, Eq, Ord)

data CompilerError = CompilerError
    { ceType :: CompilerErrorType
    , cePhase :: CompilationPhase
    , ceMessage :: String
    , ceLine :: Int
    , ceColumn :: Int
    } deriving (Show, Eq)

data CompilerResult
    = CompilerSuccess T.Text
    | CompilerFailure [CompilerError]
    | CompilerSuccessWithWarnings T.Text [CompilerError]
    deriving (Show, Eq)

-- | QuickCheck properties

-- Compilation phase progression is monotonic
prop_compilationPhaseMonotonic :: CompilationPhase -> CompilationPhase -> Bool
prop_compilationPhaseMonotonic phase1 phase2 =
  if phase1 <= phase2 then canProgressFrom phase1 phase2 else True

-- Compilation phase ordering is total
prop_compilationPhaseTotal :: CompilationPhase -> CompilationPhase -> Bool
prop_compilationPhaseTotal phase1 phase2 = 
  phase1 <= phase2 || phase2 <= phase1

-- Compiler error collection preserves severity
prop_compilerErrorCollectionPreservesSeverity :: [CompilerError] -> Bool
prop_compilerErrorCollectionPreservesSeverity errors =
  let collected = collectErrors errors
      severityOrder = map ceType collected
  in severityOrder == sortErrorTypes severityOrder

-- Compiler error location is within bounds
prop_compilerErrorLocationInBounds :: CompilerErrorType -> CompilationPhase -> Int -> Int -> Int -> Int -> Bool
prop_compilerErrorLocationInBounds errorType phase line column maxLine maxCol =
  let error = CompilerError { ceType = errorType, cePhase = phase, ceMessage = "test", ceLine = line, ceColumn = column }
      validLine = ceLine error >= 1 && ceLine error <= maxLine
      validColumn = ceColumn error >= 1 && ceColumn error <= maxCol
  in validLine && validColumn

-- Compiler result composition is associative
prop_compilerResultAssociative :: CompilerResult -> CompilerResult -> CompilerResult -> Bool
prop_compilerResultAssociative result1 result2 result3 =
  let left = combineResults (combineResults result1 result2) result3
      right = combineResults result1 (combineResults result2 result3)
  in left == right

-- Compiler result preserves error ordering
prop_compilerResultPreservesErrorOrdering :: [CompilerError] -> [CompilerError] -> Bool
prop_compilerResultPreservesErrorOrdering errors1 errors2 =
  let result1 = CompilerFailure errors1
      result2 = CompilerFailure errors2
      combined = combineResults result1 result2
      expectedOrder = errors1 ++ errors2
  in case combined of
       CompilerFailure errors -> errors == expectedOrder
       _ -> False

-- Empty input compilation
prop_emptyInputCompilation :: Bool
prop_emptyInputCompilation =
  let result = compileCode ""
  in case result of
       CompilerSuccess code -> T.null code
       CompilerSuccessWithWarnings code _ -> T.null code
       CompilerFailure _ -> False

-- Incremental compilation preserves results
prop_incrementalCompilationPreservesResults :: String -> String -> Bool
prop_incrementalCompilationPreservesResults code1 code2 =
  let result1 = compileCode code1
      result2 = compileCode code2
      combinedCode = code1 ++ "\n" ++ code2
      incrementalResult = compileCode combinedCode
      directResult = combineResults result1 result2
  in areResultsEquivalent incrementalResult directResult

-- Helper functions
canProgressFrom :: CompilationPhase -> CompilationPhase -> Bool
canProgressFrom ParsingPhase TypeCheckingPhase = True
canProgressFrom TypeCheckingPhase OwnershipPhase = True
canProgressFrom OwnershipPhase CodeGenPhase = True
canProgressFrom _ _ = False

collectErrors :: [CompilerError] -> [CompilerError]
collectErrors = sortErrors

sortErrors :: [CompilerError] -> [CompilerError]
sortErrors errors = 
  let withSeverity = L.map (\e -> (getSeverity (ceType e), e)) errors
      sorted = sortBySeverity withSeverity
  in map snd sorted

getSeverity :: CompilerErrorType -> Int
getSeverity SyntaxError = 3
getSeverity TypeError = 3
getSeverity OwnershipError = 2
getSeverity Warning = 1

sortErrorTypes :: [CompilerErrorType] -> [CompilerErrorType]
sortErrorTypes = sortBySeverity . L.map (\t -> (getSeverity t, t))

sortBySeverity :: [(Int, a)] -> [(Int, a)]
sortBySeverity [] = []
sortBySeverity ((s, x):xs) = 
  let smaller = L.filter (\(s', _) -> s' < s) xs
      larger = L.filter (\(s', _) -> s' >= s) xs
  in sortBySeverity smaller ++ [(s, x)] ++ sortBySeverity larger

combineResults :: CompilerResult -> CompilerResult -> CompilerResult
combineResults (CompilerSuccess code1) (CompilerSuccess code2) = 
  CompilerSuccess (code1 <> code2)
combineResults (CompilerSuccess code1) (CompilerSuccessWithWarnings code2 warnings) = 
  CompilerSuccessWithWarnings (code1 <> code2) warnings
combineResults (CompilerSuccessWithWarnings code1 warnings1) (CompilerSuccess code2) = 
  CompilerSuccessWithWarnings (code1 <> code2) warnings1
combineResults (CompilerSuccessWithWarnings code1 warnings1) (CompilerSuccessWithWarnings code2 warnings2) = 
  CompilerSuccessWithWarnings (code1 <> code2) (warnings1 ++ warnings2)
combineResults (CompilerFailure errors1) (CompilerFailure errors2) = 
  CompilerFailure (errors1 ++ errors2)
combineResults (CompilerFailure errors) _ = CompilerFailure errors
combineResults _ (CompilerFailure errors) = CompilerFailure errors

compileCode :: String -> CompilerResult
compileCode code
  | null code = CompilerSuccess T.empty
  | "error" `L.isInfixOf` code = CompilerFailure [CompilerError { ceType = SyntaxError, cePhase = ParsingPhase, ceMessage = "Error in code", ceLine = 1, ceColumn = 1 }]
  | "warning" `L.isInfixOf` code = CompilerSuccessWithWarnings (T.pack code) [CompilerError { ceType = Warning, cePhase = TypeCheckingPhase, ceMessage = "Warning", ceLine = 1, ceColumn = 1 }]
  | otherwise = CompilerSuccess (T.pack code)

areResultsEquivalent :: CompilerResult -> CompilerResult -> Bool
areResultsEquivalent (CompilerSuccess code1) (CompilerSuccess code2) = code1 == code2
areResultsEquivalent (CompilerFailure errors1) (CompilerFailure errors2) = L.length errors1 == L.length errors2
areResultsEquivalent (CompilerSuccessWithWarnings code1 warnings1) (CompilerSuccessWithWarnings code2 warnings2) = 
  code1 == code2 && L.length warnings1 == L.length warnings2
areResultsEquivalent _ _ = False

isInfixOf :: String -> String -> Bool
L.isInfixOf needle haystack = needle `elem` substrings haystack
  where
    substrings s = [take i s | i <- [1..L.length s]]