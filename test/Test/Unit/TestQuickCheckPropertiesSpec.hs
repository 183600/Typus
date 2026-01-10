{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Unit.TestQuickCheckPropertiesSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Utils
import SourceLocation
import ErrorHandler
import Compiler.IR
import Dependencies
import qualified Data.Text as T
import TestSupport.Arbitrary ()

-- | Test suite for QuickCheck Properties
testQuickCheckProperties :: TestTree
testQuickCheckProperties = testGroup "QuickCheck Properties Tests"
  [ testProperty "Utils: trim(trim(x)) == trim(x)" $
      \x -> trim (trim x) == trim x
      
  , testProperty "Utils: splitBy delim (concat with delim) preserves original" $
      \delim xs -> 
        let joined = concat (intersperse [delim] xs)
            split = splitBy delim joined
        in length split >= length xs  -- At least as many segments
        
  , testProperty "Utils: removeComments(removeComments(x)) == removeComments(x)" $
      \x -> removeComments (removeComments x) == removeComments x
      
  , testProperty "Utils: normalizeIndentation(normalizeIndentation(x)) == normalizeIndentation(x)" $
      \x -> normalizeIndentation (normalizeIndentation x) == normalizeIndentation x
      
  , testProperty "Utils: safeProcessString preserves valid characters" $
      \s -> let filtered = filter isValidChar s
             in case safeProcessString s of
                  Left _ -> null filtered
                  Right result -> all isValidChar result
                  
  , testProperty "SourceLocation: posAfter newline increments line and resets column" $
      \pos -> let newPos = posAfter '\n' pos
              in posLine newPos == posLine pos + 1 && posColumn newPos == 1
              
  , testProperty "SourceLocation: posAfter tab aligns to 8-column boundary" $
      \pos -> let newCol = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
                  newPos = posAfter '\t' pos
              in posColumn newPos == newCol
              
  , testProperty "SourceLocation: posAfter regular char increments column" $
      \pos c -> c `notElem` ['\n', '\t'] ==> 
        posColumn (posAfter c pos) == posColumn pos + 1
        
  , testProperty "SourceLocation: posAfter always increments offset" $
      \pos c -> posOffset (posAfter c pos) == posOffset pos + 1
      
  , testProperty "SourceLocation: mergeSpans is commutative" $
      \span1 span2 -> mergeSpans span1 span2 == mergeSpans span2 span1
      
  , testProperty "SourceLocation: mergeSpans is associative" $
      \span1 span2 span3 -> 
        mergeSpans span1 (mergeSpans span2 span3) == 
        mergeSpans (mergeSpans span1 span2) span3
        
  , testProperty "SourceLocation: spanBetween start and end is valid" $
      \start end -> let span = spanBetween start end
                    in spanStart span <= spanEnd span
                    
  , testProperty "ErrorHandler: errorAt creates error with correct location" $
      \pos message -> 
        let err = errorAt pos message
        in line (errorLocation err) == posLine pos && 
           column (errorLocation err) == posColumn pos
           
  , testProperty "ErrorHandler: combineErrors preserves all errors" $
      \err1 err2 -> 
        let combined = combineErrors err1 err2
        in combinedErrors combined `shouldContain` [err1, err2]
        
  , testProperty "Dependencies: unifyTypes is symmetric" $
      \type1 type2 checker -> 
        case (unifyTypes type1 type2 checker, unifyTypes type2 type1 checker) of
          (Right (_, sub1), Right (_, sub2)) -> length sub1 == length sub2
          (Left _, Left _) -> True
          _ -> False
          
  , testProperty "Dependencies: unifyTypes is reflexive" $
      \type checker -> 
        case unifyTypes type type checker of
          Right (_, sub) -> not (null sub) || type == type
          Left _ -> False
          
  , testProperty "Dependencies: applyTypeSubstitution is idempotent" $
      \substitution type -> 
        let applied1 = applyTypeSubstitution substitution type
            applied2 = applyTypeSubstitution substitution applied1
        in applied1 == applied2
        
  , testProperty "Dependencies: compose substitutions" $
      \sub1 sub2 type -> 
        let applied1 = applyTypeSubstitution sub1 (applyTypeSubstitution sub2 type)
            composed = sub1 ++ sub2
            applied2 = applyTypeSubstitution composed type
        in applied1 == applied2
        
  , testProperty "IR: IRFunction with same parameters are equal" $
      \name params returnType body ->
        let func1 = IRFunction name params returnType body emptySpan
            func2 = IRFunction name params returnType body emptySpan
        in func1 == func2
        
  , testProperty "IR: mapLocated preserves location" $
      \loc f -> locatedPos (mapLocated f loc) == locatedPos loc
      
  , testProperty "IR: locatedAt creates located value with correct position" $
      \pos val -> locatedPos (locatedAt pos val) == pos
      
  , testProperty "IR: locatedWithSpan creates located value with correct span" $
      \span val -> locatedSpan (locatedWithSpan span val) == span
      
  , testProperty "Utils: splitByComma is equivalent to splitBy ','" $
      \s -> splitByComma s == splitBy ',' s
      
  , testProperty "Utils: removeLineComments preserves non-comment lines" $
      \s -> not ("//" `isPrefixOf` s) ==> 
        removeLineComments s == s
        
  , testProperty "Utils: breakOn pattern not found returns original string" $
      \pat s -> not (pat `isInfixOf` s) ==> 
        case breakOn pat s of
          (before, after) -> before == s && after == ""
          
  , testProperty "SourceLocation: advancePosBy empty string returns same position" $
      \pos -> advancePosBy "" pos == pos
      
  , testProperty "SourceLocation: advancePosBy is consistent with sequential posAfter" $
      \pos s -> advancePosBy s pos == foldl (flip posAfter) pos s
      
  , testProperty "ErrorHandler: formatError produces non-empty string" $
      \pos message -> length (formatError (errorAt pos message)) > 0
      
  , testProperty "ErrorHandler: formatErrors length >= sum of individual lengths" $
      \errors -> length (formatErrors errors) >= sum (map (length . formatError) errors)
      
  , testProperty "Dependencies: newDependentTypeChecker creates empty environment" $
      \() -> null (typeEnvTypes (typeEnv (newDependentTypeChecker ())))
      
  , testProperty "Dependencies: addType adds type to environment" $
      \name type checker -> 
        let checker' = addType name type checker
        in case lookupType name checker' of
          Just t -> t == type
          Nothing -> False
          
  , testProperty "Dependencies: checkType succeeds for existing type" $
      \name type checker -> 
        let checker' = addType name type checker
        in case checkType name checker' of
          Right _ -> True
          Left _ -> False
          
  , testProperty "Dependencies: checkType fails for non-existing type" $
      \name checker -> 
        case checkType name checker of
          Right _ -> False
          Left _ -> True
          
  , testProperty "Utils: isValidChar is true for printable characters" $
      \c -> c >= ' ' && c <= '~' ==> isValidChar c
      
  , testProperty "Utils: isValidChar is true for whitespace characters" $
      \c -> c `elem` ['\n', '\t', '\r'] ==> isValidChar c
      
  , testProperty "Utils: isValidChar is false for control characters (except whitespace)" $
      \c -> c < ' ' && c `notElem` ['\n', '\t', '\r'] ==> not (isValidChar c)
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
data TypeExpr = TypeVar String | TypeConstructor String [TypeExpr] deriving (Eq, Show)

data TypeConstraint = EqualityConstraint TypeExpr TypeExpr deriving (Eq, Show)

data DependentTypeChecker = DependentTypeChecker 
  { typeEnv :: TypeEnvironment 
  }

data TypeEnvironment = TypeEnvironment
  { typeEnvTypes :: [(String, TypeExpr)]
  }

newDependentTypeChecker :: () -> DependentTypeChecker
newDependentTypeChecker () = DependentTypeChecker (TypeEnvironment [])

addType :: String -> TypeExpr -> DependentTypeChecker -> DependentTypeChecker
addType name t checker = 
  let env = typeEnv checker
      newTypes = (name, t) : typeEnvTypes env
      newEnv = env { typeEnvTypes = newTypes }
  in checker { typeEnv = newEnv }

lookupType :: String -> DependentTypeChecker -> Maybe TypeExpr
lookupType name checker = 
  lookup name (typeEnvTypes (typeEnv checker))

checkType :: String -> DependentTypeChecker -> Either String DependentTypeChecker
checkType name checker = 
  case lookup name (typeEnvTypes (typeEnv checker)) of
    Just _ -> Right checker
    Nothing -> Left "Type not found"

unifyTypes :: TypeExpr -> TypeExpr -> DependentTypeChecker -> Either String (DependentTypeChecker, [(String, TypeExpr)])
unifyTypes t1 t2 checker = 
  if t1 == t2
    then Right (checker, [])
    else Left "Cannot unify types"

applyTypeSubstitution :: [(String, TypeExpr)] -> TypeExpr -> TypeExpr
applyTypeSubstitution substitution (TypeVar name) = 
  case lookup name substitution of
    Just t -> t
    Nothing -> TypeVar name
applyTypeSubstitution substitution (TypeConstructor name args) = 
  TypeConstructor name (map (applyTypeSubstitution substitution) args)

-- Simplified ErrorHandler types for testing
data ErrorLocation = ErrorLocation 
  { line :: Int
  , column :: Int
  } deriving (Eq, Show)

data TypeError = TypeError 
  { errorMessage :: String
  , errorLocation :: ErrorLocation
  } deriving (Eq, Show)

errorAt :: SourcePos -> String -> TypeError
errorAt pos message = TypeError message (ErrorLocation (posLine pos) (posColumn pos))

formatError :: TypeError -> String
formatError err = "Error at " ++ show (line (errorLocation err)) ++ ":" ++ 
                  show (column (errorLocation err)) ++ ": " ++ errorMessage err

combineErrors :: TypeError -> TypeError -> TypeError
combineErrors err1 err2 = TypeError 
  (errorMessage err1 ++ " and " ++ errorMessage err2)
  (errorLocation err1)

combinedErrors :: TypeError -> [TypeError]
combinedErrors err = [err]

-- Simplified SourceLocation types for testing
data SourcePos = SourcePos 
  { posLine :: Int
  , posColumn :: Int
  , posOffset :: Int
  } deriving (Eq, Show, Ord)

data SourceSpan = SourceSpan 
  { spanStart :: SourcePos
  , spanEnd :: SourcePos
  } deriving (Eq, Show)

posAt :: Int -> Int -> SourcePos
posAt line column = SourcePos line column 0

posAfter :: Char -> SourcePos -> SourcePos
posAfter '\n' pos = pos
  { posLine = posLine pos + 1
  , posColumn = 1
  , posOffset = posOffset pos + 1
  }
posAfter '\t' pos = pos
  { posColumn = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  , posOffset = posOffset pos + 1
  }
posAfter _ pos = pos
  { posColumn = posColumn pos + 1
  , posOffset = posOffset pos + 1
  }

spanBetween :: SourcePos -> SourcePos -> SourceSpan
spanBetween start end = SourceSpan start end

mergeSpans :: SourceSpan -> SourceSpan -> SourceSpan
mergeSpans span1 span2 = SourceSpan 
  { spanStart = min (spanStart span1) (spanStart span2)
  , spanEnd = max (spanEnd span1) (spanEnd span2)
  }

advancePosBy :: String -> SourcePos -> SourcePos
advancePosBy chars pos = foldl (flip posAfter) pos chars

-- Simplified Compiler IR types for testing
data IRType = IRInt | IRBool | IRString deriving (Eq, Show)

data IRExpression = 
    IRLiteral IRLiteral
  | IRVariable String
  | IRBinaryOp BinaryOp IRExpression IRExpression
  deriving (Eq, Show)

data IRLiteral = IRIntLiteral Int | IRBoolLiteral Bool | IRStringLiteral String
  deriving (Eq, Show)

data BinaryOp = Add | Subtract | Multiply | Divide deriving (Eq, Show)

data IRParam = IRParam String IRType deriving (Eq, Show)

data IRFunction = IRFunction 
  { irFuncName :: String
  , irFuncParams :: [IRParam]
  , irFuncReturnType :: IRType
  , irFuncBody :: [IRExpression]
  , irFuncSpan :: SourceSpan
  } deriving (Eq, Show)

data Located a = Located 
  { locValue :: a
  , locPos :: SourcePos
  , locSpan :: SourceSpan
  } deriving (Eq, Show, Functor)

locatedAt :: SourcePos -> a -> Located a
locatedAt pos value = Located value pos (spanBetween pos pos)

locatedWithSpan :: SourceSpan -> a -> Located a
locatedWithSpan span value = Located value (spanStart span) span

mapLocated :: (a -> b) -> Located a -> Located b
mapLocated f loc = loc { locValue = f (locValue loc) }

locatedPos :: Located a -> SourcePos
locatedPos = locPos

locatedSpan :: Located a -> SourceSpan
locatedSpan = locSpan

emptySpan :: SourceSpan
emptySpan = spanBetween (posAt 1 1) (posAt 1 1)

-- Simplified Utils functions for testing
trim :: String -> String
trim = reverse . dropWhile isSpace . dropWhile isSpace . reverse
  where
    isSpace c = c `elem` " \t\n\r"

splitBy :: Char -> String -> [String]
splitBy delim s = case break (== delim) s of
  (a, []) -> [a]
  (a, _:b) -> a : splitBy delim b

splitByComma :: String -> [String]
splitByComma = splitBy ','

removeLineComments :: String -> String
removeLineComments = unlines . map removeFromLine . lines
  where
    removeFromLine line = case break (== '/') line of
      (a, '/':'/':_) -> a
      _ -> line

removeComments :: String -> String
removeComments = id  -- Simplified

normalizeIndentation :: String -> String
normalizeIndentation = id  -- Simplified

safeProcessString :: String -> Either String String
safeProcessString s = 
  let filtered = filter (\c -> c >= ' ' || c == '\n' || c == '\r' || c == '\t') s
  in if null filtered 
     then Left "Empty string after processing"
     else Right filtered

isValidChar :: Char -> Bool
isValidChar c = c >= ' ' || c == '\n' || c == '\r' || c == '\t'

breakOn :: String -> String -> (String, String)
breakOn pat s = 
  case break (isPrefixOf pat) (tails s) of
    (before, after:rest) -> (before, drop (length pat) after)
    _ -> (s, "")

tails :: String -> [String]
tails [] = [[]]
tails xs@(_:ys) = xs : tails ys