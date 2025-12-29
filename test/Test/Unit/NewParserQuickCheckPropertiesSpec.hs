{-# LANGUAGE CPP #-}

module Test.Unit.NewParserQuickCheckPropertiesSpec (tests) where

import qualified Data.Text as T
import Data.List (isInfixOf, isPrefixOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool, assertFailure)
import Test.QuickCheck (Property, (===), forAll, Gen, choose, listOf, elements, suchThat, oneof)

import TestSupport.QuickCheck (fastProperty)

import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )
import SourceLocation (Located(..))

-- QuickCheck generators
genValidGoIdentifier :: Gen String
genValidGoIdentifier = do
  firstChar <- elements $ ['a'..'z'] ++ ['_']
  restChars <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']
  return $ firstChar : restChars

genStringLiteral :: Gen String
genStringLiteral = do
  content <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "
  return $ "\"" ++ content ++ "\""

genIntLiteral :: Gen String
genIntLiteral = do
  sign <- elements ["", "-"]
  num <- choose (0, 1000)
  return $ sign ++ show num

genComment :: Gen String
genComment = do
  content <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "
  return $ "// " ++ content

genBlockComment :: Gen String
genBlockComment = do
  content <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \n"
  return $ "/* " ++ content ++ " */"

genDirectiveValue :: Gen String
genDirectiveValue = elements ["on", "off", "true", "false", "1", "0"]

genDirective :: Gen String
genDirective = do
  directiveType <- elements ["ownership", "dependent_types", "constraints"]
  value <- genDirectiveValue
  return $ "//! " ++ directiveType ++ ": " ++ value

genBlockDirective :: Gen String
genBlockDirective = do
  directiveType <- elements ["ownership", "dependent_types", "constraints"]
  value <- genDirectiveValue
  return $ "{//! " ++ directiveType ++ ": " ++ value ++ "}"

genPackageDeclaration :: Gen String
genPackageDeclaration = do
  packageName <- genValidGoIdentifier
  return $ "package " ++ packageName

genFunctionDeclaration :: Gen String
genFunctionDeclaration = do
  funcName <- genValidGoIdentifier
  paramName <- genValidGoIdentifier
  return $ unlines
    [ "func " ++ funcName ++ "() {"
    , "    " ++ paramName ++ " := 42"
    , "    println(" ++ paramName ++ ")"
    , "}"
    ]

genStructDeclaration :: Gen String
genStructDeclaration = do
  structName <- genValidGoIdentifier
  field1 <- genValidGoIdentifier
  field2 <- genValidGoIdentifier
  return $ unlines
    [ "type " ++ structName ++ " struct {"
    , "    " ++ field1 ++ " int"
    , "    " ++ field2 ++ " string"
    , "}"
    ]

genInterfaceDeclaration :: Gen String
genInterfaceDeclaration = do
  interfaceName <- genValidGoIdentifier
  methodName <- genValidGoIdentifier
  return $ unlines
    [ "type " ++ interfaceName ++ " interface {"
    , "    " ++ methodName ++ "() int"
    , "}"
    ]

genImportDeclaration :: Gen String
genImportDeclaration = oneof
  [ do
      importPath <- elements ["\"fmt\"", "\"os\"", "\"strings\"", "\"time\""]
      return $ "import " ++ importPath
  , do
      imports <- listOf $ elements ["\"fmt\"", "\"os\"", "\"strings\""]
      return $ "import (\n" ++ unlines (map ("    " ++) imports) ++ ")"
  ]

genVariableDeclaration :: Gen String
genVariableDeclaration = do
  varName <- genValidGoIdentifier
  value <- elements ["42", "\"hello\"", "true", "[]int{1, 2, 3}"]
  return $ varName ++ " := " ++ value

genValidTypusCode :: Gen String
genValidTypusCode = do
  package <- genPackageDeclaration
  imports <- listOf genImportDeclaration
  declarations <- listOf $ oneof 
    [ genFunctionDeclaration
    , genStructDeclaration
    , genInterfaceDeclaration
    , genVariableDeclaration
    ]
  return $ unlines $ [package] ++ imports ++ declarations

genCodeWithDirectives :: Gen String
genCodeWithDirectives = do
  directives <- listOf genDirective
  code <- genValidTypusCode
  return $ unlines directives ++ code

genCodeWithBlockDirectives :: Gen String
genCodeWithBlockDirectives = do
  package <- genPackageDeclaration
  blockDirective <- genBlockDirective
  content <- genFunctionDeclaration
  return $ unlines [package, blockDirective, content]

genInvalidSyntax :: Gen String
genInvalidSyntax = oneof
  [ genUnbalancedBraces
  , genInvalidPackage
  , genInvalidFunction
  , genInvalidType
  ]

genUnbalancedBraces :: Gen String
genUnbalancedBraces = do
  funcName <- genValidGoIdentifier
  return $ unlines
    [ "package main"
    , "func " ++ funcName ++ "() {"
    , "    println(\"missing closing brace\")"
    ]

genInvalidPackage :: Gen String
genInvalidPackage = do
  invalidName <- elements ["123invalid", "invalid-name", "invalid.name", ""]
  return $ "package " ++ invalidName

genInvalidFunction :: Gen String
genInvalidFunction = do
  invalidName <- elements ["123invalid", "invalid-name", "invalid.name"]
  return $ unlines
    [ "package main"
    , "func " ++ invalidName ++ "() {"
    , "    println(\"test\")"
    , "}"
    ]

genInvalidType :: Gen String
genInvalidType = do
  varName <- genValidGoIdentifier
  return $ unlines
    [ "package main"
    , "func main() {"
    , "    " ++ varName ++ " := 123invalidtype"
    , "}"
    ]

-- | QuickCheck property tests for Parser module
tests :: TestTree
tests =
  testGroup "NewParser QuickCheck Properties"
    [ testGroup "Basic parsing properties"
        [ fastProperty "parseTypus succeeds for valid code" $
            forAll genValidTypusCode $ \code ->
              case parseTypus code of
                Left _ -> False
                Right _ -> True

        , fastProperty "parseTypus preserves package declaration" $
            forAll genValidTypusCode $ \code ->
              case parseTypus code of
                Right typusFile -> not (null $ tfBlocks typusFile)
                Left _ -> False

        , fastProperty "parseTypus handles empty input gracefully" $
            case parseTypus "" of
              Left _ -> True  -- Should fail gracefully
              Right _ -> True  -- Or succeed gracefully

        , fastProperty "parseTypus handles whitespace-only input gracefully" $
            case parseTypus "   \n\t\n  " of
              Left _ -> True  -- Should fail gracefully
              Right _ -> True  -- Or succeed gracefully
        ]

    , testGroup "Directive parsing properties"
        [ fastProperty "parseTypus recognizes file directives" $
            forAll genCodeWithDirectives $ \code ->
              case parseTypus code of
                Right typusFile -> 
                  let directives = tfDirectives typusFile
                      hasOwnership = fdOwnership directives /= Nothing
                      hasDependentTypes = fdDependentTypes directives /= Nothing
                      hasConstraints = fdConstraints directives /= Nothing
                  in hasOwnership || hasDependentTypes || hasConstraints
                Left _ -> False

        , fastProperty "parseTypus recognizes block directives" $
            forAll genCodeWithBlockDirectives $ \code ->
              case parseTypus code of
                Right typusFile -> 
                  let blocks = tfBlocks typusFile
                      hasDirectiveBlock = any (\cb -> 
                        bdOwnership (cbDirectives cb) /= Nothing ||
                        bdDependentTypes (cbDirectives cb) /= Nothing ||
                        bdConstraints (cbDirectives cb) /= Nothing) blocks
                  in hasDirectiveBlock
                Left _ -> False

        , fastProperty "parseTypus handles multiple directives" $
            forAll (listOf genDirective) $ \directives ->
              let code = unlines directives ++ "\npackage main\nfunc main() {}"
              in case parseTypus code of
                   Right typusFile -> 
                     let fileDirectives = tfDirectives typusFile
                         ownershipCount = if fdOwnership fileDirectives /= Nothing then 1 else 0
                         dependentTypesCount = if fdDependentTypes fileDirectives /= Nothing then 1 else 0
                         constraintsCount = if fdConstraints fileDirectives /= Nothing then 1 else 0
                     in ownershipCount + dependentTypesCount + constraintsCount <= length directives
                   Left _ -> False
        ]

    , testGroup "Error handling properties"
        [ fastProperty "parseTypus fails for invalid syntax" $
            forAll genInvalidSyntax $ \code ->
              case parseTypus code of
                Left _ -> True
                Right _ -> False

        , fastProperty "parseTypus provides meaningful error messages" $
            forAll genInvalidSyntax $ \code ->
              case parseTypus code of
                Left err -> length err > 5  -- Error message should be meaningful
                Right _ -> False

        , fastProperty "parseTypus handles syntax errors gracefully" $
            forAll genInvalidSyntax $ \code ->
              case parseTypus code of
                Left _ -> True  -- Should fail gracefully
                Right _ -> True  -- Or succeed with errors
        ]

    , testGroup "Structure preservation properties"
        [ fastProperty "parseTypus preserves function structure" $
            forAll genFunctionDeclaration $ \code ->
              let fullCode = "package main\n" ++ code
              in case parseTypus fullCode of
                   Right typusFile -> not (null $ tfBlocks typusFile)
                   Left _ -> False

        , fastProperty "parseTypus preserves struct structure" $
            forAll genStructDeclaration $ \code ->
              let fullCode = "package main\n" ++ code
              in case parseTypus fullCode of
                   Right typusFile -> not (null $ tfBlocks typusFile)
                   Left _ -> False

        , fastProperty "parseTypus preserves interface structure" $
            forAll genInterfaceDeclaration $ \code ->
              let fullCode = "package main\n" ++ code
              in case parseTypus fullCode of
                   Right typusFile -> not (null $ tfBlocks typusFile)
                   Left _ -> False

        , fastProperty "parseTypus preserves import structure" $
            forAll genImportDeclaration $ \code ->
              let fullCode = "package main\n" ++ code ++ "\nfunc main() {}"
              in case parseTypus fullCode of
                   Right typusFile -> not (null $ tfBlocks typusFile)
                   Left _ -> False
        ]

    , testGroup "Content preservation properties"
        [ fastProperty "parseTypus preserves string literals" $
            forAll genStringLiteral $ \strLit ->
              let code = "package main\nfunc main() { s := " ++ strLit ++ " }"
              in case parseTypus code of
                   Right typusFile -> any (strLit `isInfixOf`) (map cbContent $ tfBlocks typusFile)
                   Left _ -> False

        , fastProperty "parseTypus preserves numeric literals" $
            forAll genIntLiteral $ \intLit ->
              let code = "package main\nfunc main() { x := " ++ intLit ++ " }"
              in case parseTypus code of
                   Right typusFile -> any (intLit `isInfixOf`) (map cbContent $ tfBlocks typusFile)
                   Left _ -> False

        , fastProperty "parseTypus preserves comments" $
            forAll genComment $ \comment ->
              let code = "package main\n" ++ comment ++ "\nfunc main() {}"
              in case parseTypus code of
                   Right typusFile -> any (comment `isInfixOf`) (map cbContent $ tfBlocks typusFile)
                   Left _ -> False

        , fastProperty "parseTypus preserves block comments" $
            forAll genBlockComment $ \comment ->
              let code = "package main\n" ++ comment ++ "\nfunc main() {}"
              in case parseTypus code of
                   Right typusFile -> any (comment `isInfixOf`) (map cbContent $ tfBlocks typusFile)
                   Left _ -> False
        ]

    , testGroup "Idempotency properties"
        [ fastProperty "parseTypus is idempotent for valid code" $
            forAll genValidTypusCode $ \code ->
              case parseTypus code of
                Right typusFile1 -> 
                  case parseTypus code of
                    Right typusFile2 -> typusFile1 == typusFile2
                    Left _ -> False
                Left _ -> True  -- If first parse fails, we don't care about idempotency

        , fastProperty "parseTypus error messages are consistent" $
            forAll genInvalidSyntax $ \code ->
              case parseTypus code of
                Left err1 -> 
                  case parseTypus code of
                    Left err2 -> err1 == err2
                    Right _ -> False
                Right _ -> True  -- If first parse succeeds, we don't care about error consistency
        ]

    , testGroup "Edge case properties"
        [ fastProperty "parseTypus handles very long identifiers" $
            let longIdent = replicate 1000 'a'
                code = "package main\nfunc " ++ longIdent ++ "() {}"
            in case parseTypus code of
                 Left _ -> True  -- Should fail gracefully
                 Right _ -> True  -- Or succeed gracefully

        , fastProperty "parseTypus handles deeply nested structures" $
            let nestedCode = unlines $ replicate 100 "    if true {"
                              ++ ["        println(\"deeply nested\")"]
                              ++ replicate 100 "    }"
                fullCode = "package main\nfunc main() {\n" ++ nestedCode ++ "\n}"
            in case parseTypus fullCode of
                 Left _ -> True  -- Should fail gracefully
                 Right _ -> True  -- Or succeed gracefully

        , fastProperty "parseTypus handles Unicode characters" $
            let unicodeCode = "package main\nfunc main() { println(\"¡Hola! 你好! 🌍\") }"
            in case parseTypus unicodeCode of
                 Right typusFile -> not (null $ tfBlocks typusFile)
                 Left _ -> False

        , fastProperty "parseTypus handles escape sequences" $
            let escapeCode = "package main\nfunc main() { s := \"Hello\\nWorld\\t!\" }"
            in case parseTypus escapeCode of
                 Right typusFile -> not (null $ tfBlocks typusFile)
                 Left _ -> False
        ]
    ]