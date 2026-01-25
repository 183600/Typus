{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Unit.TestQuickCheckPropertiesSpec where



import Test.Tasty
import Test.Tasty.QuickCheck

import Utils
import SourceLocation
import qualified ErrorHandler as EH ()
import qualified Compiler.Errors.Core as Error
import Dependencies ()
import qualified Dependencies.TypeSystem as Dependencies ()
import qualified Data.Text as T
import TestSupport.Arbitrary ()

-- | Test suite for QuickCheck Properties
testQuickCheckProperties :: TestTree
testQuickCheckProperties = testGroup "QuickCheck Properties Tests"
  [ testProperty "Utils: Utils.Utils.trim(Utils.Utils.trim(x)) == Utils.Utils.trim(x)" $
      \x -> Utils.trim (Utils.trim x) == Utils.trim x
      
  , testProperty "Utils: Utils.Utils.removeComments(Utils.Utils.removeComments(x)) == Utils.Utils.removeComments(x)" $
      \x -> Utils.removeComments (Utils.removeComments x) == Utils.removeComments x
      
  , testProperty "Utils: Utils.Utils.normalizeIndentation(Utils.Utils.normalizeIndentation(x)) == Utils.Utils.normalizeIndentation(x)" $
      \x -> Utils.normalizeIndentation (Utils.normalizeIndentation x) == Utils.normalizeIndentation x
      
  , testProperty "Utils: Utils.Utils.safeProcessString preserves valid characters" $
      \s -> let filtered = filter Utils.isValidChar s
             in case Utils.safeProcessString s of
                  Left _ -> null filtered
                  Right result -> all Utils.isValidChar result
                  
  , testProperty "SourceLocation: SourceLocation.SourceLocation.posAfter newError.line increments Error.line and resets Error.column" $
      \pos -> let newPos = SourceLocation.posAfter '\n' pos
              in SourceLocation.posLine newPos == SourceLocation.posLine pos + 1 && SourceLocation.posColumn newPos == 1
              
  , testProperty "SourceLocation: SourceLocation.SourceLocation.posAfter tab aligns to 8-Error.column boundary" $
      \pos -> let newCol = ((SourceLocation.posColumn pos - 1) `div` 8 + 1) * 8 + 1
                  newPos = SourceLocation.posAfter '\t' pos
              in SourceLocation.posColumn newPos == newCol
              
  , testProperty "SourceLocation: SourceLocation.SourceLocation.posAfter regular char increments Error.column" $
      \pos c -> c `notElem` ['\n', '\t'] ==> 
        SourceLocation.posColumn (SourceLocation.posAfter c pos) == SourceLocation.posColumn pos + 1
        
  , testProperty "SourceLocation: SourceLocation.SourceLocation.posAfter always increments offset" $
      \pos c -> SourceLocation.posOffset (SourceLocation.posAfter c pos) == SourceLocation.posOffset pos + 1
      
  , testProperty "SourceLocation: SourceLocation.SourceLocation.mergeSpans is commutative" $
      \span1 span2 -> SourceLocation.mergeSpans span1 span2 == SourceLocation.mergeSpans span2 span1
      
  , testProperty "SourceLocation: SourceLocation.SourceLocation.mergeSpans is associative" $
      \span1 span2 span3 -> 
        SourceLocation.mergeSpans span1 (SourceLocation.mergeSpans span2 span3) == 
        SourceLocation.mergeSpans (SourceLocation.mergeSpans span1 span2) span3
        
  , testProperty "SourceLocation: SourceLocation.SourceLocation.spanBetween start and end is valid" $
      \start end -> let sp = SourceLocation.spanBetween start end
                    in SourceLocation.spanStart sp <= SourceLocation.spanEnd sp
                    
  , testProperty "ErrorHandler: ErrorHandler.EH.errorAt creates Error (T.pack creates) error with correct Error.location" $
      \(_pos :: SourceLocation.SourcePos) message -> 
        let err = Error.errorAt "test" Error.Error (T.pack message) (Error.ErrorLocation Nothing 1 1 Nothing Nothing)
        in Error.line (Error.location err) == 1 && 
           Error.column (Error.location err) == 1
           
  , testProperty "Utils: Utils.Utils.isValidChar is true for printable characters" $
      \c -> c >= ' ' && c <= '~' ==> Utils.isValidChar c
      
  , testProperty "Utils: Utils.Utils.isValidChar is true for whitespace characters" $
      \c -> c `elem` ['\n', '\t', '\r'] ==> Utils.isValidChar c
      
  , testProperty "Utils: Utils.Utils.isValidChar is false for control characters (except whitespace)" $
      \c -> c < ' ' && c `notElem` ['\n', '\t', '\r'] ==> not (Utils.isValidChar c)
  ]

-- Helper functions
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` (substrings haystack)
  where
    substrings s = [take i s | i <- [1..length s]]

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse sep (x:xs) = x : sep : intersperse sep xs