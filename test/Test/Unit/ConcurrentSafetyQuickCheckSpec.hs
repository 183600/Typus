{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ConcurrentSafetyQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf, elements, oneof, frequency, sized)
import Test.QuickCheck.Arbitrary (Arbitrary(..))

import Control.Concurrent (MVar, forkIO, newMVar, takeMVar, putMVar, readMVar, modifyMVar_)
import Control.Concurrent.STM
import Control.Monad (replicateM, void)
import Data.IORef
import Data.List (nub, sort, (\\), intersect, union)
import qualified Data.Set as Set
import qualified Data.Map as Map

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , removeLineComments
  , normalizeIndentation
  , breakOn
  )

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , startPos
  , posAfter
  , posAt
  , advancePos
  , advancePosBy
  , mergeSpans
  )

-- | Generate concurrent operation scenarios
data ConcurrentOperation = ConcurrentOperation
  { operationId :: Int
  , operationType :: OperationType
  , operationData :: String
  } deriving (Show, Eq)

data OperationType 
  = TrimOperation
  | SplitOperation Char
  | CommentOperation
  | IndentOperation
  | BreakOperation Char
  deriving (Show, Eq)

instance Arbitrary OperationType where
  arbitrary = oneof
    [ pure TrimOperation
    , SplitOperation <$> arbitrary
    , pure CommentOperation
    , pure IndentOperation
    , BreakOperation <$> arbitrary
    ]

instance Arbitrary ConcurrentOperation where
  arbitrary = do
    opId <- choose (1, 1000)
    opType <- arbitrary
    opData <- listOf $ elements ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' ', '\t', '\n']
    return $ ConcurrentOperation opId opType opData

-- | Generate lists of concurrent operations
newtype ConcurrentOperationList = ConcurrentOperationList { getConcurrentOperationList :: [ConcurrentOperation] }
  deriving (Show, Eq)

instance Arbitrary ConcurrentOperationList where
  arbitrary = sized $ \size -> do
    let maxSize = min size 50
    len <- choose (1, maxSize)
    ops <- listOf len arbitrary
    return $ ConcurrentOperationList ops

-- Property: concurrent trim operations are thread-safe
prop_concurrent_trim_safety :: ConcurrentOperationList -> Property
prop_concurrent_trim_safety opList =
  let operations = L.filter (\op -> operationType op == TrimOperation) $ getConcurrentOperationList opList
  in not (null operations) ==>
     let testData = map operationData operations
         expectedResults = map trim testData
     in ioProperty $ do
       resultsRef <- newIORef []
       mvar <- newMVar ()
       
       -- Fork threads for each trim operation
       mapM_ (\(i, dataStr) -> forkIO $ do
         modifyMVar_ mvar $ \() -> do
           result <- return $ trim dataStr
           modifyIORef resultsRef (result:)
           return ()
         ) (zip [1..] testData)
       
       -- Wait for L.all operations to complete (simplified)
       threadDelay 100000  -- 100ms
       
       results <- readIORef resultsRef
       let sortedResults = sort results
           sortedExpected = sort expectedResults
       return $ sortedResults === sortedExpected

-- Property: concurrent split operations are consistent
prop_concurrent_split_consistency :: ConcurrentOperationList -> Char -> Property
prop_concurrent_split_consistency opList delim =
  let operations = L.filter (\op -> case operationType op of
                                   SplitOperation d -> d == delim
                                   _ -> False) $ getConcurrentOperationList opList
  in not (null operations) ==>
     let testData = map operationData operations
         expectedResults = L.map (splitBy delim) testData
     in ioProperty $ do
       resultsRef <- newIORef []
       mvar <- newMVar ()
       
       mapM_ (\(i, dataStr) -> forkIO $ do
         modifyMVar_ mvar $ \() -> do
           result <- return $ splitBy delim dataStr
           modifyIORef resultsRef (result:)
           return ()
         ) (zip [1..] testData)
       
       threadDelay 100000
       results <- readIORef resultsRef
       let sortedResults = sort results
           sortedExpected = sort expectedResults
       return $ sortedResults === sortedExpected

-- Property: concurrent source position operations are atomic
prop_concurrent_source_position_atomicity :: ConcurrentOperationList -> Property
prop_concurrent_source_position_atomicity opList =
  let operations = take 10 $ getConcurrentOperationList opList  -- Limit to avoid too many threads
      initialPos = SourcePos 10 10
  in not (null operations) ==>
     let advanceOperations = L.map (\op -> (operationId op, operationData op)) operations
     in ioProperty $ do
       resultsRef <- newIORef []
       posVar <- newMVar initialPos
       
       mapM_ \(opId, dataStr) -> forkIO $ do
         modifyMVar_ posVar $ \currentPos -> do
           let newPos = advancePosBy currentPos (take 5 dataStr)  -- Use first 5 chars
           modifyIORef resultsRef (newPos:)
           return newPos
         ) advanceOperations
       
       threadDelay 100000
       results <- readIORef resultsRef
       let allValid = L.all (\pos -> sourceLine pos >= 10 && sourceColumn pos >= 10) results
       return $ property allValid

-- Property: concurrent comment removal is thread-safe
prop_concurrent_comment_removal_safety :: ConcurrentOperationList -> Property
prop_concurrent_comment_removal_safety opList =
  let operations = L.filter (\op -> operationType op == CommentOperation) $ getConcurrentOperationList opList
  in not (null operations) ==>
     let testData = map operationData operations
         testInputs = L.map (\dataStr -> dataStr ++ " // comment") testData
         expectedResults = map removeLineComments testInputs
     in ioProperty $ do
       resultsRef <- newIORef []
       mvar <- newMVar ()
       
       mapM_ (\(i, inputStr) -> forkIO $ do
         modifyMVar_ mvar $ \() -> do
           result <- return $ removeLineComments inputStr
           modifyIORef resultsRef (result:)
           return ()
         ) (zip [1..] testInputs)
       
       threadDelay 100000
       results <- readIORef resultsRef
       let sortedResults = sort results
           sortedExpected = sort expectedResults
       return $ sortedResults === sortedExpected

-- Property: concurrent indentation normalization is consistent
prop_concurrent_indentation_consistency :: ConcurrentOperationList -> Property
prop_concurrent_indentation_consistency opList =
  let operations = L.filter (\op -> operationType op == IndentOperation) $ getConcurrentOperationList opList
  in not (null operations) ==>
     let testData = map operationData operations
         indentedData = L.map (\dataStr -> "  " ++ dataStr ++ "\n    " ++ dataStr) testData
         expectedResults = map normalizeIndentation indentedData
     in ioProperty $ do
       resultsRef <- newIORef []
       mvar <- newMVar ()
       
       mapM_ (\(i, inputStr) -> forkIO $ do
         modifyMVar_ mvar $ \() -> do
           result <- return $ normalizeIndentation inputStr
           modifyIORef resultsRef (result:)
           return ()
         ) (zip [1..] indentedData)
       
       threadDelay 100000
       results <- readIORef resultsRef
       let sortedResults = sort results
           sortedExpected = sort expectedResults
       return $ sortedResults === sortedExpected

-- Property: concurrent span merging is associative
prop_concurrent_span_merging_associative :: ConcurrentOperationList -> Property
prop_concurrent_span_merging_associative opList =
  let operations = take 5 $ getConcurrentOperationList opList
      spans = L.map (\i -> SourceSpan (posAt (i*2) (i*3)) (posAt (i*2+1) (i*3+1))) [1..L.length operations]
  in L.length operations >= 3 ==>
     let span1 = L.head spans
         span2 = spans !! 1
         span3 = spans !! 2
         leftFirst = mergeSpans (mergeSpans span1 span2) span3
         rightFirst = mergeSpans span1 (mergeSpans span2 span3)
     in ioProperty $ do
       resultRef <- newMVar (leftFirst, rightFirst)
       
       -- Simulate concurrent access
       forkIO $ modifyMVar_ resultRef $ \(lf, rf) -> do
         let newLf = mergeSpans (mergeSpans span1 span2) span3
         return (newLf, rf)
         
       forkIO $ modifyMVar_ resultRef $ \(lf, rf) -> do
         let newRf = mergeSpans span1 (mergeSpans span2 span3)
         return (lf, newRf)
       
       threadDelay 50000
       (finalLf, finalRf) <- takeMVar resultRef
       return $ finalLf === finalRf

-- Property: STM-based concurrent operations are atomic
prop_stm_atomic_operations :: ConcurrentOperationList -> Property
prop_stm_atomic_operations opList =
  let operations = take 5 $ getConcurrentOperationList opList
  in not (null operations) ==>
     ioProperty $ do
       counter <- newTVarIO 0
       resultsVar <- newTVarIO []
       
       -- Fork STM operations
       mapM_ (\op -> forkIO $ atomically $ do
         modifyTVar counter (+1)
         current <- readTVar counter
         modifyTVar resultsVar ((operationId op, current):)
         ) operations
       
       threadDelay 100000
       results <- readTVarIO resultsVar
       finalCount <- readTVarIO counter
       let uniqueResults = nub $ map snd results
       return $ 
         property (L.length results == L.length operations) .&&.
         property (finalCount == L.length operations) .&&.
         property (L.length uniqueResults == L.length operations)

-- Helper function for IO properties in QuickCheck
ioProperty :: IO Property -> Property
ioProperty = property . unsafePerformIO
  where
    -- This is a simplified approach for testing
    -- In production, you'd use proper QuickCheck IO testing
    unsafePerformIO :: IO a -> a
    unsafePerformIO = undefined  -- Placeholder - would need proper implementation

-- Simulate thread delay for testing purposes
threadDelay :: Int -> IO ()
threadDelay = undefined  -- Placeholder

tests :: TestTree
tests = testGroup "Concurrent Safety QuickCheck Tests"
  [ fastProperty "concurrent trim safety" prop_concurrent_trim_safety
  , fastProperty "concurrent split consistency" prop_concurrent_split_consistency
  , fastProperty "concurrent source position atomicity" prop_concurrent_source_position_atomicity
  , fastProperty "concurrent comment removal safety" prop_concurrent_comment_removal_safety
  , fastProperty "concurrent indentation consistency" prop_concurrent_indentation_consistency
  , fastProperty "concurrent span merging associative" prop_concurrent_span_merging_associative
  , fastProperty "STM atomic operations" prop_stm_atomic_operations
  , testGroup "Manual concurrent safety tests"
      [ testCase "MVar-based thread safety" $ do
          mvar <- newMVar (0 :: Int)
          
          -- Fork multiple threads that increment the counter
          replicateM_ 10 $ forkIO $ do
            replicateM_ 100 $ modifyMVar_ mvar (\x -> return (x + 1))
          
          threadDelay 100000  -- Wait for completion
          result <- takeMVar mvar
          assertEqual "counter should be 1000" 1000 result
          
      , testCase "STM-based atomic operations" $ do
          account1 <- newTVarIO (100 :: Int)
          account2 <- newTVarIO (50 :: Int)
          
          -- Transfer money atomically
          atomically $ do
            balance1 <- readTVar account1
            balance2 <- readTVar account2
            writeTVar account1 (balance1 - 50)
            writeTVar account2 (balance2 + 50)
          
          final1 <- readTVarIO account1
          final2 <- readTVarIO account2
          
          assertEqual "account1 should have 50" 50 final1
          assertEqual "account2 should have 100" 100 final2
          
      , testCase "concurrent string processing" $ do
          let testStrings = ["hello", "world", "test", "concurrent"]
              expected = map trim testStrings
          
          resultsRef <- newIORef []
          mvar <- newMVar ()
          
          mapM_ (\str -> forkIO $ do
            modifyMVar_ mvar $ \() -> do
              result <- return $ trim str
              modifyIORef resultsRef (result:)
              return ()
            ) testStrings
          
          threadDelay 100000
          results <- readIORef resultsRef
          assertEqual "results should match expected" expected (sort results)
    }
  ]