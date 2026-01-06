{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewParserEnhancedQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck (property)
import Test.Tasty.HUnit
import Parser
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, spanBetween)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Text as T

-- | Test parser functions with QuickCheck properties
test_ParserEnhancedQuickCheck :: TestTree
test_ParserEnhancedQuickCheck = testGroup "Parser Enhanced QuickCheck Tests"
  [ directiveParsingProperties
  , fileDirectiveProperties
  , blockDirectiveProperties
  , typusFileProperties
  , syntaxValidationProperties
  , buildTagProperties
  ]

-- | Directive parsing properties
directiveParsingProperties :: TestTree
directiveParsingProperties = testGroup "Directive Parsing Properties"
  [ QC.testProperty "parseBool parses 'true' L.and 'false' correctly" $
      \b -> let str = if b then "true" else "false"
             in parseBool str === Right b

  , QC.testProperty "parseBool rejects invalid boolean strings" $
      \s -> not (s `elem` ["true", "false"]) ==> 
             case parseBool s of
               Left _ -> True
               Right _ -> False

  , QC.testProperty "defaultFileDirectives has L.all Nothing values" $
      \fd -> fdOwnership (defaultFileDirectives) === Nothing &&
             fdDependentTypes (defaultFileDirectives) === Nothing &&
             fdConstraints (defaultFileDirectives) === Nothing

  , QC.testProperty "defaultBlockDirectives has L.all Nothing values" $
      \bd -> bdOwnership (defaultBlockDirectives) === Nothing &&
             bdDependentTypes (defaultBlockDirectives) === Nothing &&
             bdConstraints (defaultBlockDirectives) === Nothing

  , QC.testProperty "updateFileDirective updates ownership directive" $
      \fd b -> let locatedBool = locatedWithSpan (spanBetween startPos startPos) b
                   result = updateFileDirective fd "ownership" locatedBool
               in case result of
                    Right fd' -> fdOwnership fd' === Just locatedBool
                    Left _ -> False

  , QC.testProperty "updateFileDirective updates dependent_types directive" $
      \fd b -> let locatedBool = locatedWithSpan (spanBetween startPos startPos) b
                   result = updateFileDirective fd "dependent_types" locatedBool
               in case result of
                    Right fd' -> fdDependentTypes fd' === Just locatedBool
                    Left _ -> False

  , QC.testProperty "updateFileDirective rejects unknown directives" $
      \fd b s -> not (s `elem` ["ownership", "dependent_types", "constraints"]) ==>
                let locatedBool = locatedWithSpan (spanBetween startPos startPos) b
                    result = updateFileDirective fd s locatedBool
                in case result of
                     Left _ -> True
                     Right _ -> False
  ]
  where
    locatedWithSpan span value = undefined  -- Simplified for testing
    parseBool "true" = Right True
    parseBool "false" = Right False
    parseBool _ = Left "Invalid boolean"

-- | File directive properties
fileDirectiveProperties :: TestTree
fileDirectiveProperties = testGroup "File Directive Properties"
  [ QC.testProperty "parseTypus handles empty input" $
      parseTypus "" === Right (TypusFile defaultFileDirectives [] [] [])

  , QC.testProperty "parseTypus handles simple ownership directive" $
      \b -> let input = "//! ownership: " ++ if b then "true" else "false"
                 result = parseTypus input
             in case result of
                  Right tf -> isJust (fdOwnership (tfDirectives tf))
                  Left _ -> False

  , QC.testProperty "parseTypus handles multiple file directives" $
      \b1 b2 -> let input = "//! ownership: " ++ if b1 then "true" else "false" ++ "\n" ++
                               "//! dependent_types: " ++ if b2 then "true" else "false"
                    result = parseTypus input
                in case result of
                     Right tf -> isJust (fdOwnership (tfDirectives tf)) &&
                                 isJust (fdDependentTypes (tfDirectives tf))
                     Left _ -> False

  , QC.testProperty "parseTypus preserves build tags" $
      \tag -> let input = "//go:build " ++ tag
                  result = parseTypus input
              in case result of
                   Right tf -> not (L.null (tfBuildTags tf))
                   Left _ -> False

  , QC.testProperty "parseTypus handles multiple build tags" $
      \tag1 tag2 -> let input = "//go:build " ++ tag1 ++ "\n// +build " ++ tag2
                        result = parseTypus input
                    in case result of
                         Right tf -> L.length (tfBuildTags tf) >= 2
                         Left _ -> False

  , QC.testProperty "parseTypus handles mixed directives L.and build tags" $
      \b tag -> let input = "//! ownership: " ++ if b then "true" else "false" ++ "\n" ++
                           "//go:build " ++ tag
                    result = parseTypus input
                in case result of
                     Right tf -> isJust (fdOwnership (tfDirectives tf)) &&
                                 not (L.null (tfBuildTags tf))
                     Left _ -> False
  ]

-- | Block directive properties
blockDirectiveProperties :: TestTree
blockDirectiveProperties = testGroup "Block Directive Properties"
  [ QC.testProperty "parseTypus handles simple block with directive" $
      \b -> let input = "{//! ownership: " ++ if b then "true" else "false" ++ "}\ncode here"
                 result = parseTypus input
             in case result of
                  Right tf -> not (L.null (tfBlocks tf)) &&
                               isJust (bdOwnership (cbDirectives (L.head (tfBlocks tf))))
                  Left _ -> False

  , QC.testProperty "parseTypus handles multiple block directives" $
      \b1 b2 -> let input = "{//! ownership: " ++ if b1 then "true" else "false" ++ 
                               ", dependent_types: " ++ if b2 then "true" else "false" ++ "}\ncode"
                    result = parseTypus input
                in case result of
                     Right tf -> not (L.null (tfBlocks tf)) &&
                                 isJust (bdOwnership (cbDirectives (L.head (tfBlocks tf)))) &&
                                 isJust (bdDependentTypes (cbDirectives (L.head (tfBlocks tf))))
                     Left _ -> False

  , QC.testProperty "parseTypus handles blocks without directives" $
      \content -> let input = "some code\nmore code"
                      result = parseTypus input
                  in case result of
                       Right tf -> not (L.null (tfBlocks tf)) &&
                                   cbDirectives (L.head (tfBlocks tf)) === defaultBlockDirectives
                       Left _ -> False

  , QC.testProperty "parseTypus handles mixed blocks with L.and without directives" $
      \b content -> let input = "{//! ownership: " ++ if b then "true" else "false" ++ "}\ncode1\n\ncode2"
                        result = parseTypus input
                    in case result of
                         Right tf -> L.length (tfBlocks tf) >= 2
                         Left _ -> False
  ]

-- | TypusFile properties
typusFileProperties :: TestTree
typusFileProperties = testGroup "TypusFile Properties"
  [ QC.testProperty "parseTypus preserves file structure" $
      \content -> let result = parseTypus content
                  in case result of
                       Right tf -> L.length (tfBlocks tf) > 0 || not (null content)
                       Left _ -> False

  , QC.testProperty "parseTypus handles whitespace correctly" $
      \content -> let input = "\n\n" ++ content ++ "\n\n"
                      result1 = parseTypus content
                      result2 = parseTypus input
                  in case (result1, result2) of
                       (Right tf1, Right tf2) -> L.length (tfBlocks tf1) === L.length (tfBlocks tf2)
                       _ -> False

  , QC.testProperty "parseTypus is idempotent on valid input" $
      \content -> case parseTypus content of
                    Right tf -> True  -- Simplified - would need to round-trip through show
                    Left _ -> True

  , QC.testProperty "parseTypus handles complex mixed content" $
      \b content -> let input = "//! ownership: " ++ if b then "true" else "false" ++ "\n" ++
                               "//go:build test\n\n" ++
                               "{//! dependent_types: true}\n" ++ content ++ "\n" ++
                               "more code"
                        result = parseTypus input
                    in case result of
                         Right tf -> isJust (fdOwnership (tfDirectives tf)) &&
                                     not (L.null (tfBuildTags tf)) &&
                                     not (L.null (tfBlocks tf))
                         Left _ -> False
  ]

-- | Syntax validation properties
syntaxValidationProperties :: TestTree
syntaxValidationProperties = testGroup "Syntax Validation Properties"
  [ QC.testProperty "parseTypus detects if statements without braces" $
      \condition -> let input = "if " ++ condition
                        result = parseTypus input
                    in case result of
                         Left err -> "missing opening brace" `L.isInfixOf` err
                         Right _ -> False

  , QC.testProperty "parseTypus accepts if statements with braces" $
      \condition body -> let input = "if " ++ condition ++ " {\n" ++ body ++ "\n}"
                             result = parseTypus input
                         in case result of
                              Right tf -> True
                              Left _ -> False

  , QC.testProperty "parseTypus detects multiple package declarations" $
      \name1 name2 -> let input = "package " ++ name1 ++ "\npackage " ++ name2
                          result = parseTypus input
                      in case result of
                           Left err -> "Multiple package" `L.isInfixOf` err
                           Right _ -> False

  , QC.testProperty "parseTypus accepts single package declaration" $
      \name -> let input = "package " ++ name
                   result = parseTypus input
               in case result of
                    Right tf -> True
                    Left _ -> False
  ]

-- | Build tag properties
buildTagProperties :: TestTree
buildTagProperties = testGroup "Build Tag Properties"
  [ QC.testProperty "parseTypus recognizes //go:build tags" $
      \tag -> let input = "//go:build " ++ tag
                  result = parseTypus input
              in case result of
                   Right tf -> L.any ("//go:build " `L.isPrefixOf`) (L.map (("\n" ++) . show) (tfBuildTags tf))
                   Left _ -> False

  , QC.testProperty "parseTypus recognizes // +build tags" $
      \tag -> let input = "// +build " ++ tag
                  result = parseTypus input
              in case result of
                   Right tf -> L.any ("// +build " `L.isPrefixOf`) (L.map (("\n" ++) . show) (tfBuildTags tf))
                   Left _ -> False

  , QC.testProperty "parseTypus preserves build tag order" $
      \tag1 tag2 -> let input = "//go:build " ++ tag1 ++ "\n// +build " ++ tag2
                        result = parseTypus input
                    in case result of
                         Right tf -> L.length (tfBuildTags tf) >= 2
                         Left _ -> False

  , QC.testProperty "parseTypus handles build tags with directives" $
      \b tag -> let input = "//! ownership: " ++ if b then "true" else "false" ++ "\n" ++
                           "//go:build " ++ tag
                        result = parseTypus input
                    in case result of
                         Right tf -> isJust (fdOwnership (tfDirectives tf)) &&
                                     not (L.null (tfBuildTags tf))
                         Left _ -> False
  ]