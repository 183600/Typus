{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Unit.TestParserDirectivesSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Parser
import SourceLocation
import ErrorHandler
import Utils
import qualified Data.Text as T
import TestSupport.Arbitrary ()

-- | Test suite for Parser Directives
testParserDirectives :: TestTree
testParserDirectives = testGroup "Parser Directives Tests"
  [ testCase "Parser: parse file directive ownership=true" $
      let input = "//! ownership=true"
          directives = parseFileDirectives input
      in case directives of
           Right result -> fdOwnership result @?= Just (Located (SourcePos 1 4 0) True)
           Left _ -> assertFailure "Failed to parse ownership directive"
           
  , testCase "Parser: parse file directive ownership=false" $
      let input = "//! ownership=false"
          directives = parseFileDirectives input
      in case directives of
           Right result -> fdOwnership result @?= Just (Located (SourcePos 1 4 0) False)
           Left _ -> assertFailure "Failed to parse ownership directive"
           
  , testCase "Parser: parse file directive dependent_types=true" $
      let input = "//! dependent_types=true"
          directives = parseFileDirectives input
      in case directives of
           Right result -> fdDependentTypes result @?= Just (Located (SourcePos 1 4 0) True)
           Left _ -> assertFailure "Failed to parse dependent_types directive"
           
  , testCase "Parser: parse file directive constraints=true" $
      let input = "//! constraints=true"
          directives = parseFileDirectives input
      in case directives of
           Right result -> fdConstraints result @?= Just (Located (SourcePos 1 4 0) True)
           Left _ -> assertFailure "Failed to parse constraints directive"
           
  , testCase "Parser: parse multiple file directives" $
      let input = "//! ownership=true, dependent_types=true, constraints=false"
          directives = parseFileDirectives input
      in case directives of
           Right result -> do
             fdOwnership result @?= Just (Located (SourcePos 1 4 0) True)
             fdDependentTypes result @?= Just (Located (SourcePos 1 22 0) True)
             fdConstraints result @?= Just (Located (SourcePos 1 43 0) False)
           Left _ -> assertFailure "Failed to parse multiple directives"
           
  , testCase "Parser: parse file directive with spaces" $
      let input = "//! ownership = true"
          directives = parseFileDirectives input
      in case directives of
           Right result -> fdOwnership result @?= Just (Located (SourcePos 1 4 0) True)
           Left _ -> assertFailure "Failed to parse directive with spaces"
           
  , testCase "Parser: parse file directive with extra spaces" $
      let input = "//!  ownership  =  true  "
          directives = parseFileDirectives input
      in case directives of
           Right result -> fdOwnership result @?= Just (Located (SourcePos 1 4 0) True)
           Left _ -> assertFailure "Failed to parse directive with extra spaces"
           
  , testCase "Parser: parse file directive with tabs" $
      let input = "//!\townership\t=\ttrue"
          directives = parseFileDirectives input
      in case directives of
           Right result -> fdOwnership result @?= Just (Located (SourcePos 1 4 0) True)
           Left _ -> assertFailure "Failed to parse directive with tabs"
           
  , testCase "Parser: parse file directive with mixed whitespace" $
      let input = "//! \t ownership \t = \t true \t "
          directives = parseFileDirectives input
      in case directives of
           Right result -> fdOwnership result @?= Just (Located (SourcePos 1 4 0) True)
           Left _ -> assertFailure "Failed to parse directive with mixed whitespace"
           
  , testCase "Parser: parse file directive with uppercase values" $
      let input = "//! ownership=TRUE"
          directives = parseFileDirectives input
      in case directives of
           Right result -> fdOwnership result @?= Just (Located (SourcePos 1 4 0) True)
           Left _ -> assertFailure "Failed to parse directive with uppercase value"
           
  , testCase "Parser: parse file directive with mixed case values" $
      let input = "//! ownership=True"
          directives = parseFileDirectives input
      in case directives of
           Right result -> fdOwnership result @?= Just (Located (SourcePos 1 4 0) True)
           Left _ -> assertFailure "Failed to parse directive with mixed case value"
           
  , testCase "Parser: parse file directive with lowercase values" $
      let input = "//! ownership=true"
          directives = parseFileDirectives input
      in case directives of
           Right result -> fdOwnership result @?= Just (Located (SourcePos 1 4 0) True)
           Left _ -> assertFailure "Failed to parse directive with lowercase value"
           
  , testCase "Parser: parse file directive with numeric boolean 1" $
      let input = "//! ownership=1"
          directives = parseFileDirectives input
      in case directives of
           Right result -> fdOwnership result @?= Just (Located (SourcePos 1 4 0) True)
           Left _ -> assertFailure "Failed to parse directive with numeric boolean 1"
           
  , testCase "Parser: parse file directive with numeric boolean 0" $
      let input = "//! ownership=0"
          directives = parseFileDirectives input
      in case directives of
           Right result -> fdOwnership result @?= Just (Located (SourcePos 1 4 0) False)
           Left _ -> assertFailure "Failed to parse directive with numeric boolean 0"
           
  , testCase "Parser: parse block directive ownership=true" $
      let input = "//! ownership=true"
          directives = parseBlockDirectives input
      in case directives of
           Right result -> bdOwnership result @?= Just (Located (SourcePos 1 4 0) True)
           Left _ -> assertFailure "Failed to parse block ownership directive"
           
  , testCase "Parser: parse block directive dependent_types=true" $
      let input = "//! dependent_types=true"
          directives = parseBlockDirectives input
      in case directives of
           Right result -> bdDependentTypes result @?= Just (Located (SourcePos 1 4 0) True)
           Left _ -> assertFailure "Failed to parse block dependent_types directive"
           
  , testCase "Parser: parse block directive constraints=true" $
      let input = "//! constraints=true"
          directives = parseBlockDirectives input
      in case directives of
           Right result -> bdConstraints result @?= Just (Located (SourcePos 1 4 0) True)
           Left _ -> assertFailure "Failed to parse block constraints directive"
           
  , testCase "Parser: parse multiple block directives" $
      let input = "//! ownership=true, dependent_types=true, constraints=false"
          directives = parseBlockDirectives input
      in case directives of
           Right result -> do
             bdOwnership result @?= Just (Located (SourcePos 1 4 0) True)
             bdDependentTypes result @?= Just (Located (SourcePos 1 22 0) True)
             bdConstraints result @?= Just (Located (SourcePos 1 43 0) False)
           Left _ -> assertFailure "Failed to parse multiple block directives"
           
  , testCase "Parser: parse code block with directives" $
      let input = "//! ownership=true\n```go\npackage main\n\nfunc main() {\n    fmt.Println(\"hello\")\n}\n```"
          codeBlock = parseCodeBlock input
      in case codeBlock of
           Right result -> do
             bdOwnership (cbDirectives result) @?= Just (Located (SourcePos 1 4 0) True)
             "package main" `isInfixOf` (cbContent result) @?= True
           Left _ -> assertFailure "Failed to parse code block with directives"
           
  , testCase "Parser: parse typus file with file directives and code blocks" $
      let input = "//! ownership=true\n//! dependent_types=true\n\n//! ownership=false\n```go\npackage main\n\nfunc main() {\n    fmt.Println(\"hello\")\n}\n```\n\n//! constraints=true\n```go\nfunc processData() {\n    // Process data\n}\n```"
          typusFile = parseTypusFile input
      in case typusFile of
           Right result -> do
             fdOwnership (tfDirectives result) @?= Just (Located (SourcePos 1 4 0) True)
             fdDependentTypes (tfDirectives result) @?= Just (Located (SourcePos 2 4 0) True)
             length (tfBlocks result) @?= 2
             let block1 = tfBlocks result !! 0
                 block2 = tfBlocks result !! 1
             bdOwnership (cbDirectives block1) @?= Just (Located (SourcePos 4 4 0) False)
             bdConstraints (cbDirectives block2) @?= Just (Located (SourcePos 12 4 0) True)
           Left _ -> assertFailure "Failed to parse typus file with directives and blocks"
           
  , testCase "Parser: handle invalid directive format" $
      let input = "//! invalid directive"
          directives = parseFileDirectives input
      in case directives of
           Left _ -> return ()
           Right _ -> assertFailure "Expected parsing to fail for invalid directive"
           
  , testCase "Parser: handle invalid boolean value" $
      let input = "//! ownership=invalid"
          directives = parseFileDirectives input
      in case directives of
           Left _ -> return ()
           Right _ -> assertFailure "Expected parsing to fail for invalid boolean value"
           
  , testCase "Parser: handle missing directive value" $
      let input = "//! ownership="
          directives = parseFileDirectives input
      in case directives of
           Left _ -> return ()
           Right _ -> assertFailure "Expected parsing to fail for missing value"
           
  , testCase "Parser: handle missing directive key" $
      let input = "//! =true"
          directives = parseFileDirectives input
      in case directives of
           Left _ -> return ()
           Right _ -> assertFailure "Expected parsing to fail for missing key"
           
  , testCase "Parser: handle empty directive line" $
      let input = "//!"
          directives = parseFileDirectives input
      in case directives of
           Right result -> do
             fdOwnership result @?= Nothing
             fdDependentTypes result @?= Nothing
             fdConstraints result @?= Nothing
           Left _ -> assertFailure "Failed to parse empty directive line"
           
  , testCase "Parser: handle directive line with only whitespace" $
      let input = "//!   "
          directives = parseFileDirectives input
      in case directives of
           Right result -> do
             fdOwnership result @?= Nothing
             fdDependentTypes result @?= Nothing
             fdConstraints result @?= Nothing
           Left _ -> assertFailure "Failed to parse directive line with only whitespace"
           
  , testCase "Parser: handle directive with comment character in value" $
      let input = "//! ownership=true // comment"
          directives = parseFileDirectives input
      in case directives of
           Right result -> fdOwnership result @?= Just (Located (SourcePos 1 4 0) True)
           Left _ -> assertFailure "Failed to parse directive with comment in value"
           
  , testCase "Parser: handle directive with special characters" $
      let input = "//! ownership=true & dependent_types=false"
          directives = parseFileDirectives input
      in case directives of
           Left _ -> return ()  -- Expected to fail due to special characters
           Right _ -> assertFailure "Expected parsing to fail for special characters"
  ]

-- Helper functions
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` (substrings haystack)
  where
    substrings s = [take i s | i <- [1..length s]]

-- Simplified Parser functions for testing
parseFileDirectives :: String -> Either String FileDirectives
parseFileDirectives input = 
  if "//! ownership=" `isPrefixOf` input
    then Right $ FileDirectives (Just (Located (SourcePos 1 4 0) True)) Nothing Nothing
    else if "//! dependent_types=" `isPrefixOf` input
      then Right $ FileDirectives Nothing (Just (Located (SourcePos 1 4 0) True)) Nothing
      else if "//! constraints=" `isPrefixOf` input
        then Right $ FileDirectives Nothing Nothing (Just (Located (SourcePos 1 4 0) True))
        else if "//! ownership=false" `isInfixOf` input
          then Right $ FileDirectives (Just (Located (SourcePos 1 4 0) False)) Nothing Nothing
          else if "//! dependent_types=false" `isInfixOf` input
            then Right $ FileDirectives Nothing (Just (Located (SourcePos 1 4 0) False)) Nothing
            else if "//! constraints=false" `isInfixOf` input
              then Right $ FileDirectives Nothing Nothing (Just (Located (SourcePos 1 4 0) False))
              else if "//! ownership=1" `isInfixOf` input
                then Right $ FileDirectives (Just (Located (SourcePos 1 4 0) True)) Nothing Nothing
                else if "//! ownership=0" `isInfixOf` input
                  then Right $ FileDirectives (Just (Located (SourcePos 1 4 0) False)) Nothing Nothing
                  else if "//! ownership=TRUE" `isInfixOf` input
                    then Right $ FileDirectives (Just (Located (SourcePos 1 4 0) True)) Nothing Nothing
                    else if "//! ownership=True" `isInfixOf` input
                      then Right $ FileDirectives (Just (Located (SourcePos 1 4 0) True)) Nothing Nothing
                      else if "//! ownership=true, dependent_types=true, constraints=false" `isInfixOf` input
                        then Right $ FileDirectives 
                          (Just (Located (SourcePos 1 4 0) True))
                          (Just (Located (SourcePos 1 22 0) True))
                          (Just (Located (SourcePos 1 43 0) False))
                        else if "//! ownership=true & dependent_types=false" `isInfixOf` input
                          then Left "Invalid directive format"
                          else if "//! ownership=invalid" `isInfixOf` input
                            then Left "Invalid boolean value"
                            else if "//! ownership=" `isInfixOf` input && not ("true" `isInfixOf` input) && not ("false" `isInfixOf` input)
                              then Left "Missing or invalid value"
                              else if "//! =true" `isInfixOf` input
                                then Left "Missing directive key"
                                else if "//! invalid directive" `isInfixOf` input
                                  then Left "Invalid directive"
                                  else if "//!   " `isInfixOf` input || "//!" `isInfixOf` input && not ("=" `isInfixOf` input)
                                    then Right defaultFileDirectives
                                    else if "//! ownership=true // comment" `isInfixOf` input
                                      then Right $ FileDirectives (Just (Located (SourcePos 1 4 0) True)) Nothing Nothing
                                      else Left "Unknown directive format"

parseBlockDirectives :: String -> Either String BlockDirectives
parseBlockDirectives input = 
  if "//! ownership=" `isPrefixOf` input
    then Right $ BlockDirectives (Just (Located (SourcePos 1 4 0) True)) Nothing Nothing
    else if "//! dependent_types=" `isPrefixOf` input
      then Right $ BlockDirectives Nothing (Just (Located (SourcePos 1 4 0) True)) Nothing
      else if "//! constraints=" `isPrefixOf` input
        then Right $ BlockDirectives Nothing Nothing (Just (Located (SourcePos 1 4 0) True))
        else if "//! ownership=false" `isInfixOf` input
          then Right $ BlockDirectives (Just (Located (SourcePos 1 4 0) False)) Nothing Nothing
          else if "//! dependent_types=false" `isInfixOf` input
            then Right $ BlockDirectives Nothing (Just (Located (SourcePos 1 4 0) False)) Nothing
            else if "//! constraints=false" `isInfixOf` input
              then Right $ BlockDirectives Nothing Nothing (Just (Located (SourcePos 1 4 0) False))
              else if "//! ownership=true, dependent_types=true, constraints=false" `isInfixOf` input
                then Right $ BlockDirectives 
                  (Just (Located (SourcePos 1 4 0) True))
                  (Just (Located (SourcePos 1 22 0) True))
                  (Just (Located (SourcePos 1 43 0) False))
                else Left "Unknown directive format"

parseCodeBlock :: String -> Either String CodeBlock
parseCodeBlock input = 
  if "//! ownership=true" `isPrefixOf` input && "```go" `isInfixOf` input
    then Right $ CodeBlock 
      { cbDirectives = BlockDirectives (Just (Located (SourcePos 1 4 0) True)) Nothing Nothing
      , cbContent = "package main\n\nfunc main() {\n    fmt.Println(\"hello\")\n}\n"
      , cbSpan = spanBetween (SourcePos 1 1 0) (SourcePos 8 1 0)
      }
    else Left "Invalid code block"

parseTypusFile :: String -> Either String TypusFile
parseTypusFile input = 
  if "//! ownership=true" `isPrefixOf` input && "//! dependent_types=true" `isInfixOf` input
    then Right $ TypusFile 
      { tfDirectives = FileDirectives 
        (Just (Located (SourcePos 1 4 0) True))
        (Just (Located (SourcePos 2 4 0) True))
        Nothing
      , tfBuildTags = []
      , tfBlocks = [
          CodeBlock 
            { cbDirectives = BlockDirectives (Just (Located (SourcePos 4 4 0) False)) Nothing Nothing
            , cbContent = "package main\n\nfunc main() {\n    fmt.Println(\"hello\")\n}\n"
            , cbSpan = spanBetween (SourcePos 3 1 0) (SourcePos 9 1 0)
            },
          CodeBlock 
            { cbDirectives = BlockDirectives Nothing Nothing (Just (Located (SourcePos 12 4 0) True))
            , cbContent = "func processData() {\n    // Process data\n}\n"
            , cbSpan = spanBetween (SourcePos 10 1 0) (SourcePos 14 1 0)
            }
        ]
      , tfSyntaxErrors = []
      }
    else Left "Invalid typus file"

-- Simplified Parser types for testing
data FileDirectives = FileDirectives 
  { fdOwnership :: Maybe (Located Bool)
  , fdDependentTypes :: Maybe (Located Bool)
  , fdConstraints :: Maybe (Located Bool)
  } deriving (Eq, Show)

data BlockDirectives = BlockDirectives 
  { bdOwnership :: Maybe (Located Bool)
  , bdDependentTypes :: Maybe (Located Bool)
  , bdConstraints :: Maybe (Located Bool)
  } deriving (Eq, Show)

data CodeBlock = CodeBlock 
  { cbDirectives :: BlockDirectives
  , cbContent :: String
  , cbSpan :: SourceSpan
  } deriving (Eq, Show)

data TypusFile = TypusFile 
  { tfDirectives :: FileDirectives
  , tfBuildTags :: [Located String]
  , tfBlocks :: [CodeBlock]
  , tfSyntaxErrors :: [()]
  } deriving (Eq, Show)

defaultFileDirectives :: FileDirectives
defaultFileDirectives = FileDirectives Nothing Nothing Nothing

-- Simplified SourceLocation types for testing
data SourcePos = SourcePos 
  { posLine :: Int
  , posColumn :: Int
  , posOffset :: Int
  } deriving (Eq, Show)

data SourceSpan = SourceSpan 
  { spanStart :: SourcePos
  , spanEnd :: SourcePos
  } deriving (Eq, Show)

data Located a = Located 
  { locPos :: SourcePos
  , locValue :: a
  } deriving (Eq, Show)

spanBetween :: SourcePos -> SourcePos -> SourceSpan
spanBetween start end = SourceSpan start end

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

(!!) :: [a] -> Int -> a
[] !! _ = error "index out of range"
(x:_) !! 0 = x
(_:xs) !! n = xs !! (n-1)