{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewParserQuickCheckTestSpec where

import Test.Tasty
import qualified Data.List as L
import Test.Tasty.QuickCheck
import Parser (FileDirectives(..), BlockDirectives(..), defaultFileDirectives, defaultBlockDirectives)
import SourceLocation (Located(..), SourcePos(..))
import Data.Maybe (isJust, isNothing)

-- 测试默认指令的性质
prop_default_file_directives_properties :: Bool
prop_default_file_directives_properties =
  let FileDirectives{..} = defaultFileDirectives
  in isNothing fdOwnership && 
     isNothing fdDependentTypes && 
     isNothing fdConstraints

prop_default_block_directives_properties :: Bool
prop_default_block_directives_properties =
  let BlockDirectives{..} = defaultBlockDirectives
  in isNothing bdOwnership && 
     isNothing bdDependentTypes && 
     isNothing bdConstraints

-- 测试FileDirectives的性质
prop_file_directives_equality :: Maybe Bool -> Maybe Bool -> Maybe Bool -> Bool
prop_file_directives_equality own deps cons =
  let directives1 = FileDirectives (fL.map (locatedAt testPos) own) 
                                   (fL.map (locatedAt testPos) deps) 
                                   (fL.map (locatedAt testPos) cons)
      directives2 = FileDirectives (fL.map (locatedAt testPos) own) 
                                   (fL.map (locatedAt testPos) deps) 
                                   (fL.map (locatedAt testPos) cons)
  in directives1 == directives2
  where
    testPos = SourcePos 1 1
    locatedAt pos val = Located pos val

-- 测试BlockDirectives的性质
prop_block_directives_equality :: Maybe Bool -> Maybe Bool -> Maybe Bool -> Bool
prop_block_directives_equality own deps cons =
  let directives1 = BlockDirectives (fL.map (locatedAt testPos) own) 
                                    (fL.map (locatedAt testPos) deps) 
                                    (fL.map (locatedAt testPos) cons)
      directives2 = BlockDirectives (fL.map (locatedAt testPos) own) 
                                    (fL.map (locatedAt testPos) deps) 
                                    (fL.map (locatedAt testPos) cons)
  in directives1 == directives2
  where
    testPos = SourcePos 1 1
    locatedAt pos val = Located pos val

-- 测试指令值的提取
prop_file_directives_extraction :: Bool -> Bool -> Bool -> Bool
prop_file_directives_extraction own deps cons =
  let directives = FileDirectives (Just (locatedAt testPos own)) 
                                  (Just (locatedAt testPos deps)) 
                                  (Just (locatedAt testPos cons))
  in fmap locatedValue (fdOwnership directives) == Just own &&
     fmap locatedValue (fdDependentTypes directives) == Just deps &&
     fmap locatedValue (fdConstraints directives) == Just cons
  where
    testPos = SourcePos 1 1
    locatedAt pos val = Located pos val

prop_block_directives_extraction :: Bool -> Bool -> Bool -> Bool
prop_block_directives_extraction own deps cons =
  let directives = BlockDirectives (Just (locatedAt testPos own)) 
                                   (Just (locatedAt testPos deps)) 
                                   (Just (locatedAt testPos cons))
  in fmap locatedValue (bdOwnership directives) == Just own &&
     fmap locatedValue (bdDependentTypes directives) == Just deps &&
     fmap locatedValue (bdConstraints directives) == Just cons
  where
    testPos = SourcePos 1 1
    locatedAt pos val = Located pos val

-- 测试指令的一致性
prop_directives_consistency :: Maybe Bool -> Maybe Bool -> Maybe Bool -> Bool
prop_directives_consistency own deps cons =
  let fileDirs = FileDirectives (fL.map (locatedAt testPos) own) 
                                (fL.map (locatedAt testPos) deps) 
                                (fL.map (locatedAt testPos) cons)
      blockDirs = BlockDirectives (fL.map (locatedAt testPos) own) 
                                 (fL.map (locatedAt testPos) deps) 
                                 (fL.map (locatedAt testPos) cons)
      extractFile fd = fmap locatedValue fd
      extractBlock bd = fmap locatedValue bd
  in extractFile (fdOwnership fileDirs) == extractBlock (bdOwnership blockDirs) &&
     extractFile (fdDependentTypes fileDirs) == extractBlock (bdDependentTypes blockDirs) &&
     extractFile (fdConstraints fileDirs) == extractBlock (bdConstraints blockDirs)
  where
    testPos = SourcePos 1 1
    locatedAt pos val = Located pos val

-- 生成测试套件
tests :: TestTree
tests = testGroup "Parser QuickCheck Tests"
  [ testProperty "default file directives have no values" prop_default_file_directives_properties
  , testProperty "default block directives have no values" prop_default_block_directives_properties
  , testProperty "file directives equality" prop_file_directives_equality
  , testProperty "block directives equality" prop_block_directives_equality
  , testProperty "file directives extraction" prop_file_directives_extraction
  , testProperty "block directives extraction" prop_block_directives_extraction
  , testProperty "directives consistency" prop_directives_consistency
  ]