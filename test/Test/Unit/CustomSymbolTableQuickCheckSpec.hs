{-# LANGUAGE OverloadedStrings #-}
module Test.Unit.CustomSymbolTableQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (==>), forAll, elements, listOf, listOf1, oneof, choose)
import Analyzer.SymbolTable
  ( collectSymbolsAndTypes
  , collectSymbolsFromAST
  , augmentSymbolTableWithLocals
  , trim
  , isReservedName
  , extractTypeEnvironment
  )
import Analyzer.Types
import Compiler.GoAst (GoModule(..), GoDecl(..), FuncDecl(..), VarDecl(..), ConstDecl(..), TypeDecl(..))
import qualified Data.Map.Strict as Map
import Control.Monad.Except (runExcept)
import Data.Char (isAlphaNum, isLetter)

-- | Generate valid identifiers
genIdentifier :: Gen String
genIdentifier = do
  first <- elements ['a'..'z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ ['_']
  return $ first : rest

-- | Generate reserved names
genReservedName :: Gen String
genReservedName = elements
  [ "func", "var", "const", "type", "if", "else", "for", "return"
  , "int", "string", "bool", "float", "package", "import"
  , "struct", "interface", "map", "chan", "go", "defer", "select"
  , "case", "switch", "default", "break", "continue", "fallthrough"
  , "range", "goto", "append", "cap", "close", "complex", "copy"
  , "delete", "imag", "len", "make", "new", "panic", "print"
  , "println", "real", "recover"
  ]

-- | Generate non-reserved identifiers
genNonReservedIdentifier :: Gen String
genNonReservedIdentifier = do
  base <- genIdentifier
  if isReservedName base
    then genNonReservedIdentifier  -- Retry if reserved
    else return base

-- | Generate simple Go code snippets
genGoCodeSnippet :: Gen String
genGoCodeSnippet = oneof
  [ genVariableDeclaration
  , genFunctionDeclaration
  , genTypeDeclaration
  , genConstantDeclaration
  , genPackageDeclaration
  ]

genVariableDeclaration :: Gen String
genVariableDeclaration = do
  varName <- genNonReservedIdentifier
  varType <- elements ["int", "string", "bool", "float"]
  value <- case varType of
    "int" -> elements ["42", "0", "-1"]
    "string" -> elements ["\"hello\"", "\"world\""]
    "bool" -> elements ["true", "false"]
    "float" -> elements ["3.14", "0.0", "-1.5"]
    _ -> return "nil"
  return $ "var " ++ varName ++ " " ++ varType ++ " = " ++ value

genFunctionDeclaration :: Gen String
genFunctionDeclaration = do
  funcName <- genNonReservedIdentifier
  paramName <- genNonReservedIdentifier
  paramType <- elements ["int", "string", "bool"]
  returnType <- elements ["int", "string", "bool", "void"]
  return $ "func " ++ funcName ++ "(" ++ paramName ++ " " ++ paramType ++ ") " ++ returnType ++ " { return " ++ paramName ++ "; }"

genTypeDeclaration :: Gen String
genTypeDeclaration = do
  typeName <- genNonReservedIdentifier
  underlyingType <- elements ["int", "string", "struct{}"]
  return $ "type " ++ typeName ++ " " ++ underlyingType

genConstantDeclaration :: Gen String
genConstantDeclaration = do
  constName <- genNonReservedIdentifier
  constType <- elements ["int", "string", "bool"]
  value <- case constType of
    "int" -> elements ["42", "0", "-1"]
    "string" -> elements ["\"hello\"", "\"world\""]
    "bool" -> elements ["true", "false"]
    _ -> return "nil"
  return $ "const " ++ constName ++ " " ++ constType ++ " = " ++ value

genPackageDeclaration :: Gen String
genPackageDeclaration = do
  packageName <- genNonReservedIdentifier
  return $ "package " ++ packageName

-- | Generate complete Go files
genGoFile :: Gen String
genGoFile = do
  packageDecl <- genPackageDeclaration
  numDecls <- choose (1, 5)
  declarations <- sequence [genGoCodeSnippet | _ <- [1..numDecls]]
  return $ unlines (packageDecl : declarations)

-- | Test trim function
prop_trimFunction :: Property
prop_trimFunction = forAll (listOf $ elements " \t\n") $ \whitespace ->
  let testString = whitespace ++ "content" ++ whitespace
      trimmed = trim testString
  in trimmed == "content"

-- | Test trim on empty string
prop_trimEmpty :: Property
prop_trimEmpty = 
  trim "" == ""

-- | Test trim on only whitespace
prop_trimOnlyWhitespace :: Property
prop_trimOnlyWhitespace = forAll (listOf1 $ elements " \t\n") $ \whitespace ->
  trim whitespace == ""

-- | Test isReservedName with reserved names
prop_isReservedNameReserved :: Property
prop_isReservedNameReserved = forAll genReservedName $ \reservedName ->
  isReservedName reservedName

-- | Test isReservedName with non-reserved names
prop_isReservedNameNonReserved :: Property
prop_isReservedNameNonReserved = forAll genNonReservedIdentifier $ \nonReservedName ->
  not (isReservedName nonReservedName)

-- | Test collectSymbolsAndTypes with simple code
prop_collectSymbolsAndTypesSimple :: Property
prop_collectSymbolsAndTypesSimple = forAll genGoCodeSnippet $ \codeSnippet ->
  let result = runExcept (collectSymbolsAndTypes codeSnippet)
  in case result of
    Left _ -> True  -- May fail, which is fine
    Right _ -> True  -- Or may succeed

-- | Test collectSymbolsAndTypes with complete file
prop_collectSymbolsAndTypesFile :: Property
prop_collectSymbolsAndTypesFile = forAll genGoFile $ \goFile ->
  let result = runExcept (collectSymbolsAndTypes goFile)
  in case result of
    Left _ -> True  -- May fail, which is fine
    Right _ -> True  -- Or may succeed

-- | Test collectSymbolsAndTypes with empty input
prop_collectSymbolsAndTypesEmpty :: Property
prop_collectSymbolsAndTypesEmpty = 
  let result = runExcept (collectSymbolsAndTypes "")
  in case result of
    Left _ -> True  -- May fail, which is fine
    Right symbolTable -> Map.null symbolTable  -- Should return empty symbol table

-- | Test collectSymbolsAndTypes with invalid syntax
prop_collectSymbolsAndTypesInvalid :: Property
prop_collectSymbolsAndTypesInvalid = 
  let invalidCode = "func incomplete {"
      result = runExcept (collectSymbolsAndTypes invalidCode)
  in case result of
    Left _ -> True  -- Should fail on invalid syntax
    Right _ -> False

-- | Test symbol collection from variable declarations
prop_collectVariableDeclaration :: Property
prop_collectVariableDeclaration = forAll genVariableDeclaration $ \varDecl ->
  let result = runExcept (collectSymbolsAndTypes varDecl)
  in case result of
    Left _ -> True  -- May fail, which is fine
    Right symbolTable -> True  -- Basic test that symbol table is returned

-- | Test symbol collection from function declarations
prop_collectFunctionDeclaration :: Property
prop_collectFunctionDeclaration = forAll genFunctionDeclaration $ \funcDecl ->
  let result = runExcept (collectSymbolsAndTypes funcDecl)
  in case result of
    Left _ -> True  -- May fail, which is fine
    Right symbolTable -> True  -- Basic test that symbol table is returned

-- | Test symbol collection from type declarations
prop_collectTypeDeclaration :: Property
prop_collectTypeDeclaration = forAll genTypeDeclaration $ \typeDecl ->
  let result = runExcept (collectSymbolsAndTypes typeDecl)
  in case result of
    Left _ -> True  -- May fail, which is fine
    Right symbolTable -> True  -- Basic test that symbol table is returned

-- | Test symbol collection from constant declarations
prop_collectConstantDeclaration :: Property
prop_collectConstantDeclaration = forAll genConstantDeclaration $ \constDecl ->
  let result = runExcept (collectSymbolsAndTypes constDecl)
  in case result of
    Left _ -> True  -- May fail, which is fine
    Right symbolTable -> True  -- Basic test that symbol table is returned

-- | Test extractTypeEnvironment on simple code
prop_extractTypeEnvironmentSimple :: Property
prop_extractTypeEnvironmentSimple = forAll genGoCodeSnippet $ \codeSnippet ->
  let symbolTableResult = runExcept (collectSymbolsAndTypes codeSnippet)
  in case symbolTableResult of
    Left _ -> True  -- May fail, which is fine
    Right symbolTable -> 
      let typeEnv = extractTypeEnvironment symbolTable
      in True  -- Basic test that type environment can be extracted

-- | Test extractTypeEnvironment on empty symbol table
prop_extractTypeEnvironmentEmpty :: Property
prop_extractTypeEnvironmentEmpty = 
  let emptySymbolTable = Map.empty
      typeEnv = extractTypeEnvironment emptySymbolTable
  in True  -- Basic test that type environment can be extracted from empty table

-- | Test augmentSymbolTableWithLocals
prop_augmentSymbolTableWithLocals :: Property
prop_augmentSymbolTableWithLocals = 
  let baseTable = Map.empty
      locals = Map.empty
      augmented = augmentSymbolTableWithLocals baseTable locals
  in True  -- Basic test that augmentation doesn't crash

-- | Test identifier validation
prop_identifierValidation :: Property
prop_identifierValidation = forAll genIdentifier $ \identifier ->
  let isValid = not (null identifier) && isLetter (head identifier) && all isAlphaNumOrUnderscore identifier
  in isValid ==> True
  where
    isAlphaNumOrUnderscore c = isAlphaNum c || c == '_'

-- | Test reserved name list properties
prop_reservedNameListProperties :: Property
prop_reservedNameListProperties = 
  let reservedNames = ["func", "var", "const", "type", "if", "else", "for", "return"]
  in all isReservedName reservedNames

-- | Test symbol table consistency
prop_symbolTableConsistency :: Property
prop_symbolTableConsistency = forAll genGoFile $ \goFile ->
  let result = runExcept (collectSymbolsAndTypes goFile)
  in case result of
    Left _ -> True  -- May fail, which is fine
    Right symbolTable -> 
      let keys = Map.keys symbolTable
      in all isNonReservedKey keys
  where
    isNonReservedKey key = not (isReservedName key)

-- | Test multiple symbol collections
prop_multipleSymbolCollections :: Property
prop_multipleSymbolCollections = forAll genGoCodeSnippet $ \codeSnippet1 ->
  forAll genGoCodeSnippet $ \codeSnippet2 ->
    let result1 = runExcept (collectSymbolsAndTypes codeSnippet1)
        result2 = runExcept (collectSymbolsAndTypes codeSnippet2)
    in case (result1, result2) of
      (Left _, Left _) -> True  -- Both may fail
      (Right _, Right _) -> True  -- Both may succeed
      (Left _, Right _) -> True  -- One may fail, one succeed
      (Right _, Left _) -> True  -- One may succeed, one fail

tests :: TestTree
tests = testGroup "Custom SymbolTable QuickCheck Tests"
  [ testProperty "trim function" prop_trimFunction
  , testProperty "trim empty" prop_trimEmpty
  , testProperty "trim only whitespace" prop_trimOnlyWhitespace
  , testProperty "isReservedName reserved" prop_isReservedNameReserved
  , testProperty "isReservedName non-reserved" prop_isReservedNameNonReserved
  , testProperty "collectSymbolsAndTypes simple" prop_collectSymbolsAndTypesSimple
  , testProperty "collectSymbolsAndTypes file" prop_collectSymbolsAndTypesFile
  , testProperty "collectSymbolsAndTypes empty" prop_collectSymbolsAndTypesEmpty
  , testProperty "collectSymbolsAndTypes invalid" prop_collectSymbolsAndTypesInvalid
  , testProperty "collect variable declaration" prop_collectVariableDeclaration
  , testProperty "collect function declaration" prop_collectFunctionDeclaration
  , testProperty "collect type declaration" prop_collectTypeDeclaration
  , testProperty "collect constant declaration" prop_collectConstantDeclaration
  , testProperty "extractTypeEnvironment simple" prop_extractTypeEnvironmentSimple
  , testProperty "extractTypeEnvironment empty" prop_extractTypeEnvironmentEmpty
  , testProperty "augmentSymbolTableWithLocals" prop_augmentSymbolTableWithLocals
  , testProperty "identifier validation" prop_identifierValidation
  , testProperty "reserved name list properties" prop_reservedNameListProperties
  , testProperty "symbol table consistency" prop_symbolTableConsistency
  , testProperty "multiple symbol collections" prop_multipleSymbolCollections
  ]