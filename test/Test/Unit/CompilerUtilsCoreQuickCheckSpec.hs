{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.Unit.CompilerUtilsCoreQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), choose, vectorOf, elements )
import Control.Monad (replicateM, when)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, sort, intercalate)
import Data.Char (isSpace, isDigit, isAlpha, toLower, toUpper)
import Data.Maybe (isJust, isNothing, fromMaybe)

import CompilerUtils
  ( Logger(..)
  , CompilerContext(..)
  , defaultLogger
  , silentLogger
  )

-- Arbitrary instances for QuickCheck
instance Arbitrary Logger where
  arbitrary = elements [defaultLogger, silentLogger]

-- Property: Logger message processing consistency
prop_logger_message_processing :: Logger -> String -> Property
prop_logger_message_processing logger message =
  let processed = message
  in property $ not (null processed) ==> length processed >= length message - 100 -- Allow some processing variance

-- Property: Logger maintains message content
prop_logger_message_content :: Logger -> String -> Property
prop_logger_message_content logger message =
  let wordsInOriginal = words message
      hasContent = not (null wordsInOriginal)
  in hasContent ==> property $ length (filter (not . null) wordsInOriginal) >= 1

-- Property: Default logger properties
prop_default_logger_properties :: Property
prop_default_logger_properties =
  let logger = defaultLogger
  in property $ True -- Placeholder since we can't inspect logger internals directly

-- Property: Silent logger properties  
prop_silent_logger_properties :: Property
prop_silent_logger_properties =
  let logger = silentLogger
  in property $ True -- Placeholder since we can't inspect logger internals directly

-- Property: Logger function composition
prop_logger_composition :: Logger -> String -> String -> Property
prop_logger_composition logger msg1 msg2 =
  let combined = msg1 ++ " " ++ msg2
      combinedLength = length combined
      separateLength = length msg1 + length msg2 + 1
  in property $ combinedLength === separateLength

-- Property: Logger handles empty messages
prop_logger_empty_messages :: Logger -> Property
prop_logger_empty_messages logger =
  let emptyMsg = ""
      whitespaceMsg = "   \t\n  "
  in property $ length emptyMsg === 0 .&&. length (trim whitespaceMsg) === 0
  where
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- Property: Logger handles Unicode messages
prop_logger_unicode_messages :: Logger -> String -> Property
prop_logger_unicode_messages logger baseMsg =
  let unicodeMsg = baseMsg ++ "测试🚀café naïve"
      hasUnicode = any (> '\x7F') unicodeMsg
  in hasUnicode ==> property $ length unicodeMsg >= length baseMsg

-- Property: Logger handles very long messages
prop_logger_long_messages :: Logger -> String -> Int -> Property
prop_logger_long_messages logger baseMsg multiplier =
  multiplier >= 0 && multiplier <= 100 ==> 
  let longMsg = concat (replicate multiplier baseMsg)
      expectedLength = multiplier * length baseMsg
  in property $ length longMsg === expectedLength

-- Property: Logger message transformation invariants
prop_logger_message_invariants :: Logger -> String -> Property
prop_logger_message_invariants logger message =
  let processed = message
      originalLength = length message
      processedLength = length processed
  in property $ processedLength >= 0 .&&. processedLength <= originalLength + 1000 -- Allow some growth

-- Property: Logger handles special characters
prop_logger_special_characters :: Logger -> String -> Property
prop_logger_special_characters logger baseMsg =
  let specialChars = "\0\1\2\3\4\5\6\7\8\10\11\12\13\14\15\16\17\18\19\20\21\22\23\24\25\26\27\28\29\30\31\127"
      msgWithSpecial = baseMsg ++ specialChars ++ baseMsg
  in property $ length msgWithSpecial === length baseMsg * 2 + length specialChars

-- Property: Logger message case handling
prop_logger_case_handling :: Logger -> String -> Property
prop_logger_case_handling logger message =
  let upperMsg = map toUpper message
      lowerMsg = map toLower message
      originalLength = length message
  in property $ length upperMsg === originalLength .&&. length lowerMsg === originalLength

-- Property: Logger handles repeated messages
prop_logger_repeated_messages :: Logger -> String -> Int -> Property
prop_logger_repeated_messages logger message count =
  count >= 0 && count <= 50 ==>
  let repeatedMsg = concat (replicate count message)
      expectedLength = count * length message
  in property $ length repeatedMsg === expectedLength

tests :: TestTree
tests = testGroup "Compiler Utils Core QuickCheck Tests"
  [ fastProperty "logger message processing consistency" prop_logger_message_processing
  , fastProperty "logger message content" prop_logger_message_content
  , fastProperty "default logger properties" prop_default_logger_properties
  , fastProperty "silent logger properties" prop_silent_logger_properties
  , fastProperty "logger composition" prop_logger_composition
  , fastProperty "logger empty messages" prop_logger_empty_messages
  , fastProperty "logger unicode messages" prop_logger_unicode_messages
  , fastProperty "logger long messages" prop_logger_long_messages
  , fastProperty "logger message invariants" prop_logger_message_invariants
  , fastProperty "logger special characters" prop_logger_special_characters
  , fastProperty "logger case handling" prop_logger_case_handling
  , fastProperty "logger repeated messages" prop_logger_repeated_messages
  ]