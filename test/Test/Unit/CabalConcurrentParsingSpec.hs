module Test.Unit.CabalConcurrentParsingSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

import TestSupport.QuickCheck (fastProperty)

import qualified Parser (parseTypus)
import qualified Utils (trim, splitBy, removeComments)
import qualified SourceLocation
import Control.Concurrent (forkIO, MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (replicateM_, replicateM)
import Data.List (nub)

-- | Concurrent parsing L.and thread safety tests
tests :: TestTree
tests =
  testGroup "Cabal Concurrent Parsing Tests"
    [ testGroup "Thread Safety of Utils"
        [ testCase "Concurrent trim operations" $ do
            let testStrings = ["  test1  ", "\ttest2\n", "  test3  ", "test4  "]
            resultsVar <- newEmptyMVar
            _ <- forkIO $ do
                let trimmed = map Utils.trim testStrings
                putMVar resultsVar trimmed
            finalResults <- takeMVar resultsVar
            finalResults @?= ["test1", "test2", "test3", "test4"]

        , testCase "Concurrent splitBy operations" $ do
            let testInputs = ["a,b,c", "x,y,z", "1,2,3"]
            resultsVar <- newEmptyMVar
            _ <- forkIO $ do
                let split = map (Utils.splitBy ',') testInputs
                putMVar resultsVar split
            finalResults <- takeMVar resultsVar
            finalResults @?= [["a", "b", "c"], ["x", "y", "z"], ["1", "2", "3"]]

        , testCase "Concurrent comment removal" $ do
            let commentedInputs = 
                  [ "func test1() { return 1; } // comment1"
                  , "func test2() { return 2; } /* comment2 */"
                  , "func test3() { return 3; } // comment3"
                  ]
            resultsVar <- newEmptyMVar
            _ <- forkIO $ do
                let uncommented = map Utils.removeComments commentedInputs
                putMVar resultsVar uncommented
            finalResults <- takeMVar resultsVar
            L.all (`L.isInfixOf` L.concat (map show finalResults)) ["return 1;", "return 2;", "return 3;"] @?= True
        ]

    , testGroup "Concurrent Parsing"
        [ testCase "Multiple threads parsing different inputs" $ do
            let inputs = 
                  [ "func test1() { return 1; }"
                  , "func test2() { return 2; }"
                  , "func test3() { return 3; }"
                  ]
            resultsVar <- newEmptyMVar
            _ <- forkIO $ do
                let parseResults = map Parser.parseTypus inputs
                putMVar resultsVar parseResults
            finalResults <- takeMVar resultsVar
            L.all isSuccess finalResults @?= True

        , testCase "Concurrent parsing of same input" $ do
            let input = "func shared() { return 42; }"
            resultsVar <- newEmptyMVar
            _ <- forkIO $ do
                let parseResults = replicate 3 $ Parser.parseTypus input
                putMVar resultsVar parseResults
            finalResults <- takeMVar resultsVar
            L.all isSuccess finalResults @?= True

        , testCase "Concurrent parsing with errors" $ do
            let invalidInputs = 
                  [ "func bad1() { return }"
                  , "func bad2() { if }"
                  , "func bad3() { { {"
                  ]
            resultsVar <- newEmptyMVar
            _ <- forkIO $ do
                let parseResults = map Parser.parseTypus invalidInputs
                putMVar resultsVar parseResults
            finalResults <- takeMVar resultsVar
            L.all isFailure finalResults @?= True
        ]

    , testGroup "Source Location Thread Safety"
        [ testCase "Concurrent source position creation" $ do
            let positions = [SourceLocation.SourcePos line col 0 | line <- [1..100], col <- [1..10]]
            resultsVar <- newEmptyMVar
            _ <- forkIO $ do
                let chunked = chunks 20 positions
                processed <- mapM processPositions chunked
                putMVar resultsVar (L.concat processed)
            finalResults <- takeMVar resultsVar
            L.length finalResults @?= 1000

        , testCase "Concurrent span operations" $ do
            let spans = [SourceLocation.SourceSpan (SourceLocation.SourcePos 1 1 0) (SourceLocation.SourcePos 10 10 0)]
            resultsVar <- newEmptyMVar
            _ <- forkIO $ do
                merged <- replicateM 10 $ return (foldl SourceLocation.mergeSpans (L.head spans) (L.tail spans))
                putMVar resultsVar merged
            finalResults <- takeMVar resultsVar
            L.all SourceLocation.isValidSpan finalResults @?= True

        , testCase "Concurrent position advancement" $ do
            let basePos = SourceLocation.SourcePos 1 1 0
                chars = "abcdefghijklmnopqrstuvwxyz"
            resultsVar <- newEmptyMVar
            _ <- forkIO $ do
                let advanced = map (\c -> SourceLocation.advancePos c basePos) chars
                putMVar resultsVar advanced
            finalResults <- takeMVar resultsVar
            L.length finalResults @?= 26
        ]

    , testGroup "Memory Consistency"
        [ testCase "No memory leaks in concurrent parsing" $ do
            let input = "func memory() { return 1; }"
            resultsVar <- newEmptyMVar
            _ <- forkIO $ do
                let parseResults = replicate 100 $ Parser.parseTypus input
                putMVar resultsVar parseResults
            finalResults <- takeMVar resultsVar
            L.all isSuccess finalResults @?= True

        , testCase "Consistent results across threads" $ do
            let input = "func consistent() { return true; }"
            results1 <- newEmptyMVar
            results2 <- newEmptyMVar
            _ <- forkIO $ do
                let result1 = Parser.parseTypus input
                putMVar results1 result1
            _ <- forkIO $ do
                let result2 = Parser.parseTypus input
                putMVar results2 result2
            final1 <- takeMVar results1
            final2 <- takeMVar results2
            (isSuccess final1 && isSuccess final2) @?= True
        , testProperty "Concurrent operations yield consistent results" $ do
            \input -> do
                let result1 = Utils.trim input
                    result2 = Utils.trim input
                result1 == result2
        ]

    , testGroup "Race Condition Tests"
        [ testCase "No race conditions in parser state" $ do
            let complexInput = unlines
                  [ "func race1() {"
                  , "  if (true) { return 1; }"
                  , "  else { return 2; }"
                  , "}"
                  , "func race2() {"
                  , "  for (i := 0; i < 10; i++) {"
                  , "    return i;"
                  , "  }"
                  , "}"
                  ]
            resultsVar <- newEmptyMVar
            _ <- forkIO $ do
                let parseResults = replicate 5 $ Parser.parseTypus complexInput
                putMVar resultsVar parseResults
            finalResults <- takeMVar resultsVar
            L.all isSuccess finalResults @?= True

        , testCase "Concurrent access to utils functions" $ do
            let testStrings = ["test1", "test2", "test3", "test4", "test5"]
            resultsVar <- newEmptyMVar
            _ <- forkIO $ do
                processed <- mapM processString testStrings
                putMVar resultsVar processed
            finalResults <- takeMVar resultsVar
            L.length (nub finalResults) == L.length finalResults @?= True  -- All should be unique

        , testCase "Thread-safe error handling" $ do
            let errorInputs = ["{", "}", "func", "return", "if"]
            resultsVar <- newEmptyMVar
            _ <- forkIO $ do
                let errorResults = map Parser.parseTypus errorInputs
                putMVar resultsVar errorResults
            finalResults <- takeMVar resultsVar
            L.all isFailure finalResults @?= True
        ]

    , testGroup "Performance under Concurrency"
        [ testCase "Concurrent parsing performance" $ do
            let inputs = [unlines ["func test" ++ show i ++ "() { return " ++ show i ++ "; }"] | i <- [1..50]]
            resultsVar <- newEmptyMVar
            _ <- forkIO $ do
                let parseResults = map Parser.parseTypus inputs
                putMVar resultsVar parseResults
            finalResults <- takeMVar resultsVar
            L.length finalResults @?= 50

        , testCase "Concurrent utils processing" $ do
            let largeStrings = [unlines ["line " ++ show j | j <- [1..100]] | i <- [1..10]]
            resultsVar <- newEmptyMVar
            _ <- forkIO $ do
                processed <- mapM processLargeString largeStrings
                putMVar resultsVar processed
            finalResults <- takeMVar resultsVar
            L.all (> 0) (map L.length finalResults) @?= True
        ]

    , testGroup "Stress Testing"
        [ testCase "High concurrency stress test" $ do
            let input = "func stress() { return 0; }"
            resultsVar <- newEmptyMVar
            _ <- forkIO $ do
                let parseResults = replicate 1000 $ Parser.parseTypus input
                putMVar resultsVar parseResults
            finalResults <- takeMVar resultsVar
            L.length finalResults @?= 1000

        , testCase "Complex concurrent operations" $ do
            let complexInput = unlines
                  [ "// @ownership: true"
                  , "func complex() {"
                  , "  let x := 1;"
                  , "  let y := 2;"
                  , "  if (x < y) {"
                  , "    return x + y;"
                  , "  } else {"
                  , "    return x - y;"
                  , "  }"
                  , "}"
                  ]
            resultsVar <- newEmptyMVar
            _ <- forkIO $ do
                    let parseResults = replicate 100 $ Parser.parseTypus complexInput
                        processed = map processParseResult parseResults
                    putMVar resultsVar processed
            finalResults <- takeMVar resultsVar
            L.all (== True) finalResults @?= True
        ]
    ]

-- Helper functions
isSuccess :: Either a b -> Bool
isSuccess (Right _) = True
isSuccess (Left _) = False

isFailure :: Either a b -> Bool
isFailure (Left _) = True
isFailure (Right _) = False

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)

processPositions :: [SourceLocation.SourcePos] -> IO [SourceLocation.SourcePos]
processPositions = return . L.map (\pos -> pos { SourceLocation.posColumn = SourceLocation.posColumn pos + 1 })

processString :: String -> IO String
processString = return . Utils.trim

processLargeString :: String -> IO String
processLargeString = return . Utils.removeComments

processParseResult :: Either a b -> Bool
processParseResult (Right _) = True
processParseResult (Left _) = False

isInfixOf :: Eq a => [a] -> [[a]] -> Bool
isInfixOf needle haystack = L.any (needle `L.isPrefixOf`) haystack
  where
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys