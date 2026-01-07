module Test.Unit.NewParserCombinatorPropertiesSpec where


import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (property) as QC
import Parser
  ( FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )
import SourceLocation (Located(..), SourcePos(..), startPos)
import Data.Char 