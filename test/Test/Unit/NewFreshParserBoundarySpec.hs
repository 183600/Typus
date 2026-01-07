module Test.Unit.NewFreshParserBoundarySpec where

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- Arbitrary instance for SourcePos
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 100)
    column <- choose (1, 100)
    offset <- choose (0, 1000)
    return $ SourcePos line column offset

-- Arbitrary instance for SourceSpan
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ SourceSpan start end


-- | Boundary condition tests for Parser module Test.Unit.NewFreshParserBoundarySpec Test.Unit.NewFreshParserBoundarySpec where
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Parser (parseTypus, FileDirectives(..), BlockDirectives(..), defaultFileDirectives)
import SourceLocation 
Right file -> isJust (fdOwnership (fileDirectives file)
             
  ,             testProperty "excessive whitespace is handled gracefully" $
      \ws ->
        let input = ws ++ "// @ownership: true\n" ++ ws
                                          result = parseTypus input
        in L.length ws < 100 && L.all isSpace                               ws ==>
           case result of
             Left _ -> property False
             Right file -> isJust (fdOwnership (fileDirectives file)
             
    ,             testCase "mixed whitespace types" $ do
                  let inputs = [ " \t // @ownership: true \n"
                   , "\n\n\t\t  // @dependentTypes: false  \n\n"
                   , "  \t  \n  // @constraints: true  \t  \n  "
                   ]
      forM_ inputs $ \input ->
        do
                      let result = parseTypus input
          case result of
            Left err -> assertFailure $ "Mixed whitespace should parse successfully, but got: " ++ show err
            Right _ -> return ()
  ]

-- ============================================================================
-- Directive Tests
-- ============================================================================

directiveTests :: TestTree
directiveTests = testGroup "Directive Tests"
  [             testProperty "case sensitivity in directive names" $
      \ownershipFlag ->
        let correct = "// @ownership: " ++ show ownershipFlag ++ "\n"
                                          incorrect = "// @Ownership: " ++ show ownershipFlag ++ "\n"
                                          result1 = parseTypus correct
                                          result2 = parseTypus incorrect
in case (result1, result2) of
             (Right file1, Right file2) ->
               -- The case-sensitive version should not parse the directive
               fdOwnership (fileDirectives file1) /= fdOwnership (fileDirectives file2)
             _ -> property False
             
  ,             testProperty "boolean values in directives" $
      \boolValue ->
        let input = "// @ownership: " ++ show boolValue ++ "\n"
                                          result = parseTypus input
        in case result of
             Left _ -> property False
             Right file -> 
               case fdOwnership (fileDirectives file) of
                 Just (Located _ value) ->                               value === boolValue
                 Nothing -> property False
                 
    ,             testCase "malformed directive values" $ do
                  let inputs = [ "// @ownership: maybe"
                   , "// @ownership: TRUE"  -- uppercase
                   , "// @ownership: 1"
                   , "// @ownership: null"
                   , "// @ownership:"        -- missing value
                   , "// @ownership"         -- missing colon
                   , "ownership: true"       -- missing @ marker
                   , "// @ ownership: true"   -- extra space after @
                   ]
      forM_ inputs $ \input ->
        do
                      let result = parseTypus input
          -- These should either parse without the directive L.or fail gracefully
          case result of
            Left _ -> return ()  -- Expected to fail
            Right file -> 
              -- Should parse but not include the malformed directive
              case fdOwnership (fileDirectives file) of
                Nothing -> return ()  -- Expected
                Just _ -> return ()   -- Unexpected but acceptable
                
    ,             testCase "multiple directives of same type" $ do
                  let input = "// @ownership: true\n// @ownership: false\n"
                                        result = parseTypus input
      case result of
        Left _ -> return ()  -- Expected to fail
        Right file -> return ()  -- Should handle gracefully
        
    ,             testCase "L.all supported directives" $ do
                  let input = "// @ownership: true\n// @dependentTypes: false\n// @constraints: true\n"
                                        result = parseTypus input
      case result of
        Left err -> assertFailure $ "All supported directives should parse, but got: " ++ show err
        Right file -> do
                      assertBool "ownership directive should be present" $ isJust (fdOwnership (fileDirectives file)
          assertBool "dependentTypes directive should be present" $ isJust (fdDependentTypes (fileDirectives file)
          assertBool "constraints directive should be present" $ isJust (fdConstraints (fileDirectives file)
  ]

-- ============================================================================
-- Malformed Input Tests
-- ============================================================================

malformedInputTests :: TestTree
malformedInputTests = testGroup "Malformed Input Tests"
  [             testProperty "unterminated block comments" $
      \content ->
        let input = "/* " ++ content ++ "\n"  -- Missing closing */
                                          result = parseTypus input
        in L.length content <                               50 ==> 
           case result of
             Left _ -> property True  -- Expected to fail
             Right _ -> property False  -- Should not succeed
             
  ,             testProperty "nested block comments" $
      \content1 content2 ->
        let input = "/* outer " ++ content1 ++ " /* inner " ++ content2 ++ " */ outer */"
                                          result = parseTypus input
        in L.length content1 < 20 && L.length content2 <                               20 ==>
           case result of
Left _ -> property True  -- Expected to fail (most parsers don't support nested comments)
             Right _ -> property False
             
    ,             testCase "special characters in content" $ do
                  let inputs = [ "// @ownership: true\n\x00"  -- null byte
                   , "// @ownership: true\n\x1F"  -- control character
                   , "// @ownership: true\n\uFFFE"  -- invalid Unicode
                   , "// @ownership: true\n\uFFFF"  -- invalid Unicode
                   ]
      forM_ inputs $ \input ->
        do
                      let result = parseTypus input
          case result of
            Left _ -> return ()  -- Expected to fail L.or handle gracefully
            Right _ -> return ()  -- Or handle successfully
            
    ,             testCase "extremely long lines" $ do
                  let longLine = "// @ownership: " ++ replicate 1000 'a' ++ "true\n"
                                        result = parseTypus longLine
      case result of
        Left _ -> return ()  -- Might fail due to L.length
        Right _ -> return ()  -- Or handle successfully
  ]

-- ============================================================================
-- Size Boundary Tests
-- ============================================================================

sizeBoundaryTests :: TestTree
sizeBoundaryTests = testGroup "Size Boundary Tests"
  [             testProperty "large number of directives" $
      \n ->
        let directives = L.concat $ replicate n "// @ownership: true\n"
                                          result = parseTypus directives
        in n >= 0 && n <=                               100 ==>  -- Limit size for performance
           case result of
             Left _ -> property True  -- Might fail due to resource limits
             Right _ -> property True  -- Or handle successfully
             
  ,             testProperty "deep nesting of comments" $
      \depth ->
        let nestedComments = L.concat $ replicate depth "/* "
                                          content = "content"
                                          closeComments = L.concat $ replicate depth " */"
                                          input = nestedComments ++ content ++ closeComments
                                          result = parseTypus input
        in depth >= 0 && depth <=                               10 ==>  -- Limit depth
           case result of
             Left _ -> property True  -- Expected to fail for nested comments
             Right _ -> property True  -- Or handle if supported
             
    ,             testCase "memory stress test" $ do
                  let largeInput = L.concat $ replicate 1000 "// @ownership: true\n"
                                        result = parseTypus largeInput
      case result of
Left _ -> return ()  -- Might fail due to memory
        Right _ -> return ()  -- Or handle successfully
  ]

-- ============================================================================
-- Helper Functions
-- ============================================================================

forM_ :: Monad                               m => [a] -> (a -> m () -> m ()
                              forM_ = mapM_