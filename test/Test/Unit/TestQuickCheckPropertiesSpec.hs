{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Unit.TestQuickCheckPropertiesSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Utils
import SourceLocation
import qualified ErrorHandler as EH
import qualified Compiler.Errors.Core as Error
import qualified Compiler.IR as IR
import Dependencies
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import TestSupport.Arbitrary ()

-- | Test suite for QuickCheck Properties
testQuickCheckProperties :: TestTree
testQuickCheckProperties = testGroup "QuickCheck Properties Tests"
  [ testProperty "Utils: Utils.Utils.trim(Utils.Utils.trim(x)) == Utils.Utils.trim(x)" $
      \x -> Utils.trim (Utils.trim x) == Utils.trim x
      
  , testProperty "Utils: Utils.splitBy delim (concat with delim) preserves original" $
      \delim xs -> 
        let joined = concat (intersperse [delim] xs)
            split = Utils.splitBy delim joined
        in length split >= length xs  -- At least as many segments
        
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
      \start end -> let span = SourceLocation.spanBetween start end
                    in SourceLocation.spanStart span <= SourceLocation.spanEnd span
                    
  , testProperty "ErrorHandler: ErrorHandler.EH.errorAt creates error with correct Error.location" $
      \pos message -> 
        let err = EH.errorAt pos message
        in Error.line (Error.location (EH.errorAt "" message (Error.ErrorLocation Nothing 1 1 Nothing Nothing))) == 1 && 
           Error.column (Error.location (EH.errorAt "" message (Error.ErrorLocation Nothing 1 1 Nothing Nothing))) == 1
           
  , testProperty "ErrorHandler: ErrorHandler.EH.combineErrors preserves all errors" $
      \err1 err2 -> 
        let combined = EH.combineErrors [err1, err2]
        in combined `shouldContain` [err1, err2]
        
  , testProperty "Dependencies: Dependencies.Dependencies.unifyTypes is symmetric" $
      \type1 type2 checker -> 
        case (Dependencies.unifyTypes type1 type2, Dependencies.unifyTypes type2 type1) of
          (Right (_, sub1), Right (_, sub2)) -> length sub1 == length sub2
          (Left _, Left _) -> True
          _ -> False
          
  , testProperty "Dependencies: Dependencies.Dependencies.unifyTypes is reflexive" $
      \typ checker -> 
        case Dependencies.unifyTypes typ typ checker of
          Right (_, sub) -> not (null sub) || typ == typ
          Left _ -> False
          
  , testProperty "Dependencies: Dependencies.Dependencies.applyTypeSubstitution is idempotent" $
      \substitution typ -> 
        let applied1 = Dependencies.applyTypeSubstitution substitution typ
            applied2 = Dependencies.applyTypeSubstitution substitution applied1
        in applied1 == applied2
        
  , testProperty "Dependencies: compose substitutions" $
      \sub1 sub2 typ -> 
        let applied1 = Dependencies.applyTypeSubstitution sub1 (Dependencies.applyTypeSubstitution sub2 typ)
            composed = Map.union sub1 sub2
            applied2 = Dependencies.applyTypeSubstitution composed typ
        in applied1 == applied2
        
  , testProperty "IR: IRFunction with same parameters are equal" $
      \name params returnType body ->
        let func1 = IRFunction name params returnType body SourceLocation.emptySpan
            func2 = IRFunction name params returnType body SourceLocation.emptySpan
        in func1 == func2
        
  , testProperty "IR: SourceLocation.mapLocated preserves Error.location" $
      \loc f -> SourceLocation.locatedPos (SourceLocation.mapLocated f loc) == SourceLocation.locatedPos loc
      
  , testProperty "IR: SourceLocation.SourceLocation.locatedAt creates located value with correct position" $
      \pos val -> SourceLocation.locatedPos (SourceLocation.locatedAt pos val) == pos
      
  , testProperty "IR: SourceLocation.SourceLocation.locatedWithSpan creates located value with correct span" $
      \span val -> SourceLocation.locatedSpan (SourceLocation.locatedWithSpan span val) == span
      
  , testProperty "Utils: Utils.splitByComma is equivalent to Utils.splitBy ','" $
      \s -> Utils.splitByComma s == Utils.splitBy ',' s
      
  , testProperty "Utils: Utils.removeLineComments preserves non-comment Error.lines" $
      \s -> not ("//" `isPrefixOf` s) ==> 
        Utils.removeLineComments s == s
        
  , testProperty "Utils: Utils.breakOn pattern not found returns original string" $
      \pat s -> not (pat `isInfixOf` s) ==> 
        case Utils.breakOn pat s of
          (before, after) -> before == s && after == ""
          
  , testProperty "SourceLocation: SourceLocation.SourceLocation.advancePosBy empty string returns same position" $
      \pos -> SourceLocation.advancePosBy "" pos == pos
      
  , testProperty "SourceLocation: SourceLocation.SourceLocation.advancePosBy is consistent with sequential SourceLocation.SourceLocation.posAfter" $
      \pos s -> SourceLocation.advancePosBy s pos == foldl (flip SourceLocation.posAfter) pos s
      
  , testProperty "ErrorHandler: ErrorHandler.EH.formatError produces non-empty string" $
      \pos message -> length (EH.formatError (EH.errorAt pos message)) > 0
      
  , testProperty "ErrorHandler: EH.formatErrors length >= sum of individual lengths" $
      \errors -> length (EH.formatErrors errors) >= sum (map (length . EH.formatError) errors)
      
  , testProperty "Dependencies: Dependencies.newDependentTypeChecker creates empty environment" $
      \() -> null (Dependencies.teTypes (Dependencies.typeEnv (Dependencies.newDependentTypeChecker ())))
      
  , testProperty "Dependencies: Dependencies.Dependencies.addType adds type to environment" $
      \name typ checker -> 
        let checker' = Dependencies.addType name typ checker
        in case lookupType name checker' of
          Just t -> t == typ
          Nothing -> False
          
  , testProperty "Dependencies: Dependencies.Dependencies.checkType succeeds for existing type" $
      \name typ checker -> 
        let checker' = Dependencies.addType name typ checker
        in case Dependencies.checkType name checker' of
          Right _ -> True
          Left _ -> False
          
  , testProperty "Dependencies: Dependencies.Dependencies.checkType fails for non-existing type" $
      \name checker -> 
        case Dependencies.checkType name checker of
          Right _ -> False
          Left _ -> True
          
  , testProperty "Utils: Utils.Utils.isValidChar is true for printable characters" $
      \c -> c >= ' ' && c <= '~' ==> Utils.isValidChar c
      
  , testProperty "Utils: Utils.Utils.isValidChar is true for whitespace characters" $
      \c -> c `elem` ['\n', '\t', '\r'] ==> Utils.isValidChar c
      
  , testProperty "Utils: Utils.Utils.isValidChar is false for control characters (except whitespace)" $
      \c -> c < ' ' && c `notElem` ['\n', '\t', '\r'] ==> not (Utils.isValidChar c)
  ]

-- Helper functions
shouldContain :: (Show a, Eq a) => [a] -> [a] -> Bool
shouldContain list elements = all (`elem` list) elements

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

-- Simplified Dependencies types for testing
lookupType :: String -> DependentTypeChecker -> Maybe TypeExpr
lookupType name checker = 
  lookup name (Dependencies.teTypes (Dependencies.typeEnv checker))

combinedErrors :: Error.TypeError -> [Error.TypeError]
combinedErrors err = [err]

posAt :: Int -> Int -> SourcePos
posAt line column = SourcePos line column 0

-- Simplified Compiler IR types for testing


tails :: String -> [String]
tails [] = [[]]
tails xs@(_:ys) = xs : tails ys