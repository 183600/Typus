{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewParserQuickCheckSpec (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Parser
import SourceLocation (SourcePos(..), SourceSpan(..))
import qualified Data.Text as T
import Data.Char (isAlphaNum, isSpace)

-- | Test properties for Parser module
tests :: TestTree
tests = testGroup "Parser QuickCheck Tests"
  [ testProperty "parseTypus: handles empty input" propParseTypusEmpty
  , testProperty "parseTypus: handles simple text without directives" propParseTypusSimpleText
  , testProperty "parseTypus: handles file directives" propParseTypusFileDirectives
  , testProperty "parseTypus: handles block directives" propParseTypusBlockDirectives
  , testProperty "parseTypus: handles build tags" propParseTypusBuildTags
  , testProperty "defaultFileDirectives: has all Nothing values" propDefaultFileDirectives
  , testProperty "defaultBlockDirectives: has all Nothing values" propDefaultBlockDirectives
  , testProperty "parseTypus: preserves content order" propParseTypusPreservesOrder
  , testProperty "parseTypus: handles mixed content" propParseTypusMixedContent
  ]

-- | parseTypus: Handles empty input
propParseTypusEmpty :: Property
propParseTypusEmpty = 
  let result = parseTypus ""
  in case result of
       Left err -> counterexample ("Parse error: " ++ err) $ False
       Right typusFile -> 
         counterexample ("Result: " ++ show typusFile) $
           tfDirectives typusFile == defaultFileDirectives &&
           null (tfBuildTags typusFile) &&
           null (tfBlocks typusFile)

-- | parseTypus: Handles simple text without directives
propParseTypusSimpleText :: String -> Property
propParseTypusSimpleText text = 
  not ("//" `isPrefixOf` text) && not ("//!" `isInfixOf` text) && not ("//!:" `isInfixOf` text) ==>
    let input = text ++ "\n"
        result = parseTypus input
    in case result of
         Left err -> counterexample ("Parse error: " ++ err) $ False
         Right typusFile -> 
           counterexample ("Result: " ++ show typusFile) $
             tfDirectives typusFile == defaultFileDirectives &&
             null (tfBuildTags typusFile) &&
             length (tfBlocks typusFile) == 1 &&
             cbContent (head (tfBlocks typusFile)) == text

-- | parseTypus: Handles file directives
propParseTypusFileDirectives :: Bool -> Bool -> Bool -> Property
propParseTypusFileDirectives ownership dependentTypes constraints = 
  let ownershipStr = if ownership then "true" else "false"
      dependentTypesStr = if dependentTypes then "true" else "false"
      constraintsStr = if constraints then "true" else "false"
      directives = ["ownership: " ++ ownershipStr,
                    "dependent-types: " ++ dependentTypesStr,
                    "constraints: " ++ constraintsStr]
      input = "//! " ++ intercalate ", " directives ++ "\ncontent\n"
      result = parseTypus input
  in case result of
       Left err -> counterexample ("Parse error: " ++ err) $ False
       Right typusFile -> 
         counterexample ("Result: " ++ show (tfDirectives typusFile)) $
           let fileDirs = tfDirectives typusFile
               checkOwnership = case fdOwnership fileDirs of
                                 Nothing -> not ownership
                                 Just (Located _ val) -> val == ownership
               checkDependentTypes = case fdDependentTypes fileDirs of
                                       Nothing -> not dependentTypes
                                       Just (Located _ val) -> val == dependentTypes
               checkConstraints = case fdConstraints fileDirs of
                                    Nothing -> not constraints
                                    Just (Located _ val) -> val == constraints
           in checkOwnership && checkDependentTypes && checkConstraints

-- | parseTypus: Handles block directives
propParseTypusBlockDirectives :: Bool -> Bool -> Bool -> Property
propParseTypusBlockDirectives ownership dependentTypes constraints = 
  let ownershipStr = if ownership then "true" else "false"
      dependentTypesStr = if dependentTypes then "true" else "false"
      constraintsStr = if constraints then "true" else "false"
      directives = ["ownership: " ++ ownershipStr,
                    "dependent-types: " ++ dependentTypesStr,
                    "constraints: " ++ constraintsStr]
      input = "{//! " ++ intercalate ", " directives ++ "}\ncontent\n"
      result = parseTypus input
  in case result of
       Left err -> counterexample ("Parse error: " ++ err) $ False
       Right typusFile -> 
         counterexample ("Result: " ++ show (tfBlocks typusFile)) $
           if null (tfBlocks typusFile) then False
           else
             let block = head (tfBlocks typusFile)
                 blockDirs = cbDirectives block
                 checkOwnership = case bdOwnership blockDirs of
                                   Nothing -> not ownership
                                   Just (Located _ val) -> val == ownership
                 checkDependentTypes = case bdDependentTypes blockDirs of
                                         Nothing -> not dependentTypes
                                         Just (Located _ val) -> val == dependentTypes
                 checkConstraints = case bdConstraints blockDirs of
                                      Nothing -> not constraints
                                      Just (Located _ val) -> val == constraints
             in checkOwnership && checkDependentTypes && checkConstraints

-- | parseTypus: Handles build tags
propParseTypusBuildTags :: [String] -> Property
propParseTypusBuildTags tags = 
  all (not . null) tags && all (not . isPrefixOf "//") tags ==> 
    let tagLines = map (\tag -> "// +build " ++ tag) tags
        input = unlines tagLines ++ "\ncontent\n"
        result = parseTypus input
    in case result of
         Left err -> counterexample ("Parse error: " ++ err) $ False
         Right typusFile -> 
           counterexample ("Expected tags: " ++ show tags ++ 
                          ", Actual tags: " ++ show (map locValue (tfBuildTags typusFile))) $
             length (tfBuildTags typusFile) == length tags &&
             all (\(Located _ tag) -> tag `elem` tags) (tfBuildTags typusFile)

-- | defaultFileDirectives: Has all Nothing values
propDefaultFileDirectives :: Property
propDefaultFileDirectives = 
  let dirs = defaultFileDirectives
  in fdOwnership dirs == Nothing &&
     fdDependentTypes dirs == Nothing &&
     fdConstraints dirs == Nothing

-- | defaultBlockDirectives: Has all Nothing values
propDefaultBlockDirectives :: Property
propDefaultBlockDirectives = 
  let dirs = defaultBlockDirectives
  in bdOwnership dirs == Nothing &&
     bdDependentTypes dirs == Nothing &&
     bdConstraints dirs == Nothing

-- | parseTypus: Preserves content order
propParseTypusPreservesOrder :: [String] -> Property
propParseTypusPreservesOrder contentBlocks = 
  all (not . null) contentBlocks && 
  all (not . any (`isPrefixOf` "//") . words) contentBlocks ==> 
    let input = unlines contentBlocks
        result = parseTypus input
    in case result of
         Left err -> counterexample ("Parse error: " ++ err) $ False
         Right typusFile -> 
           counterexample ("Expected: " ++ show contentBlocks ++ 
                          ", Actual: " ++ show (map cbContent (tfBlocks typusFile))) $
             map cbContent (tfBlocks typusFile) == contentBlocks

-- | parseTypus: Handles mixed content
propParseTypusMixedContent :: String -> String -> String -> Property
propParseTypusMixedContent content1 content2 content3 = 
  not (any (`isPrefixOf` "//") [content1, content2, content3]) ==>
    let input = unlines 
          [ "// +build tag1"
          , "{//! ownership: true}"
          , content1
          , "{//! dependent-types: true}"
          , content2
          , "{//! constraints: true}"
          , content3
          ]
        result = parseTypus input
    in case result of
         Left err -> counterexample ("Parse error: " ++ err) $ False
         Right typusFile -> 
           counterexample ("Result: " ++ show typusFile) $
             length (tfBuildTags typusFile) == 1 &&
             locValue (head (tfBuildTags typusFile)) == "tag1" &&
             length (tfBlocks typusFile) == 3 &&
             cbContent (tfBlocks typusFile !! 0) == content1 &&
             cbContent (tfBlocks typusFile !! 1) == content2 &&
             cbContent (tfBlocks typusFile !! 2) == content3

-- Helper function to check if a string is a prefix of another
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

-- Helper function to intercalate a list with a separator
intercalate :: [a] -> [[a]] -> [a]
intercalate _ [] = []
intercalate _ [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

-- Helper function to check if any element of a list satisfies a predicate
any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any p (x:xs) = p x || any p xs

-- Helper function to get words from a string
words :: String -> [String]
words = go []
  where
    go acc [] = [reverse acc]
    go acc (c:cs)
      | isSpace c = if null acc then go [] cs else reverse acc : go [] cs
      | otherwise = go (c:acc) cs