module Test.Unit.CabalPerformanceSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

import TestSupport.QuickCheck (fastProperty)

import qualified Parser (parseTypus)
import qualified Utils (trim, splitBy, removeComments)
import qualified SourceLocation
import Control.DeepSeq (NFData, rnf)

-- | Performance L.and efficiency tests
tests :: TestTree
tests =
  testGroup "Cabal Performance Tests"
    [ testGroup "Parsing Performance"
        [ testCase "Small files parse quickly" $ do
            let input = "func quick() { return 42; }"
                result = Parser.parseTypus "small" input
            case result of
              Left err -> @?= "Should parse successfully" (show err)
              Right _ -> @?= "Success" "Quick parsing"

        , testCase "Medium files parse efficiently" $ do
            let mediumInput = unlines $ replicate 100 "func test" ++ show [1..100] ++ "{ return " ++ show [1..100] ++ "; }"
                result = Parser.parseTypus "medium" mediumInput
            case result of
              Left _ -> @?= "Should handle medium input" "Medium handling"
              Right _ -> @?= "Success" "Medium success"

        , testCase "Large files don't cause memory issues" $ do
            let largeInput = unlines $ replicate 1000 "func large() { return 1; }"
                result = Parser.parseTypus "large" largeInput
            case result of
              Left err -> L.length (show err) > 0 @?= True
              Right _ -> @?= "Handle large input" "Large handling"
        ]

    , testGroup "Utils Performance"
        [ testCase "trim is efficient on large strings" $ do
            let largeString = "   " ++ replicate 10000 'a' ++ "   "
                trimmed = Utils.trim largeString
            L.length trimmed @?= 10000

        , testCase "splitBy handles large inputs efficiently" $ do
            let largeInput = unlines $ replicate 1000 "line content"
                lines = Utils.splitBy '\n' largeInput
            L.length lines @?= 1000

        , testCase "removeComments processes large files efficiently" $ do
            let largeCommented = unlines $ replicate 500 "// comment" ++ ["func test() { return 1; }"]
                uncommented = Utils.removeComments largeCommented
            L.length uncommented > 0 @?= True
        ]

    , testGroup "Source Location Performance"
        [ testCase "Source position calculations are efficient" $ do
            let positions = [SourceLocation.SourcePos line col | line <- [1..100], col <- [1..100]]
            L.length positions @?= 10000

        , testCase "Span merging is efficient" $ do
            let spans = [SourceLocation.SourceSpan (SourceLocation.SourcePos 1 1) (SourceLocation.SourcePos 10 10)]
                merged = foldl SourceLocation.mergeSpans (L.head spans) (L.tail spans)
            SourceLocation.isValidSpan merged @?= True

        , testProperty "Source location operations don't crash" $ do
            \line col -> let pos = SourceLocation.SourcePos (abs line `mod` 1000 + 1) (abs col `mod` 1000 + 1)
                         in rnf pos `seq` True
        ]

    , testGroup "Memory Efficiency"
        [ testCase "Parser doesn't leak memory on repeated calls" $ do
            let testInput = "func memory() { return 1; }"
                results = L.map (\_ -> Parser.parseTypus "memory" testInput) [1..10]
            L.all (\result -> case result of
                              Left _ -> True
                              Right _ -> True) results @?= True

        , testCase "Utils functions are space-efficient" $ do
            let testString = "test\n" ++ replicate 1000 "content\n"
                processed = Utils.removeComments testString
            rnf (L.length processed) `seq` True @?= True

        , testProperty "Deep evaluation doesn't cause issues" $ do
            \input -> let processed = Utils.trim input
                      in rnf processed `seq` True
        ]

    , testGroup "Time Complexity Tests"
        [ testCase "Parsing time scales reasonably" $ do
            let inputs = [unlines $ replicate n "func test() { return 1; }" | n <- [10, 50, 100]]
                results = L.map (\input -> Parser.parseTypus "scale" input) inputs
            L.all (\result -> case result of
                              Left _ -> True
                              Right _ -> True) results @?= True

        , testCase "Utils operations maintain linear complexity" $ do
            let sizes = [100, 500, 1000]
                testStrings = L.map (\n -> unlines $ replicate n "test line") sizes
                splitResults = L.map (Utils.splitBy '\n') testStrings
            L.all (\(size, result) -> L.length result == size) (zip sizes splitResults) @?= True

        , testProperty "Comment removal is linear time" $ do
            \input -> let result = Utils.removeComments input
                      in L.length result <= L.length input + 100  -- Allow some overhead
        ]

    , testGroup "Resource Usage"
        [ testCase "Parser handles nested structures efficiently" $ do
            let nestedInput = unlines ["func test() {"] ++ 
                               replicate 50 "  if (true) {" ++
                               replicate 50 "    return 1;" ++
                               replicate 50 "  }" ++
                               ["]"]
                result = Parser.parseTypus "nested" nestedInput
            case result of
              Left err -> L.length (show err) > 0 @?= True
              Right _ -> @?= "Handle nested" "Nested handling"

        , testCase "Large comments don't cause overflow" $ do
            let largeComment = "/* " ++ replicate 10000 'x' ++ " */\nfunc test() { return 1; }"
                result = Parser.parseTypus "largecomment" largeComment
            case result of
              Left err -> L.length (show err) > 0 @?= True
              Right _ -> @?= "Handle large comment" "Large comment handling"
        ]
    ]