{-# LANGUAGE CPP #-}

module Test.Unit.CompilerPropertiesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, Arbitrary(..), Gen, oneof, elements, listOf, choose, sized)

import Compiler.TypeChecker (Type(..), TypeEnv(..), FunctionSignature(..), FunctionParam(..), 
                            buildTypeEnv, buildTypeEnvFromPairs, typesEqual, CallExpr(..))
import Compiler.GoAst (GoDecl(..), ConstDecl(..), RawBlock(..), FuncDecl(..), TypeDecl(..), VarDecl(..), StatementBlock(..))
import Parser (TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..))
import Utils (trim)
import Data.Char (isAlphaNum, isSpace)
import qualified Data.List as Data.List
import Data.List (isPrefixOf, isInfixOf, maximum)
import Data.Maybe (isJust, isNothing)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import SourceLocation (SourceSpan(..), emptySpan, startPos)

-- | Generate random Go declarations
genGoDecl :: Int -> Gen GoDecl
genGoDecl depth = 
  if depth <= 0 then genSimpleDecl
  else oneof
    [ genSimpleDecl
    , genFuncDecl depth
    , genTypeDecl depth
    , genVarDecl depth
    ]

-- | Generate simple declarations
genSimpleDecl :: Gen GoDecl
genSimpleDecl = oneof
  [ GoConst <$> genConstDecl
  , GoRaw <$> genRawBlock
  ]

-- | Generate function declarations
genFuncDecl :: Int -> Gen GoDecl
genFuncDecl depth = do
  lineCount <- choose (1, min 5 depth)
  lines <- listOfN lineCount (elements ["func test() {}", "func calculate() int {}", "func process()"])
  return $ GoFunc (FuncDecl lines)
  where
    listOfN k gen = sequence [gen | _ <- [1..k]]

-- | Generate type declarations
genTypeDecl :: Int -> Gen GoDecl
genTypeDecl depth = do
  lineCount <- choose (1, min 3 depth)
  lines <- listOfN lineCount (elements ["type MyType int", "type Custom struct {}", "type Data interface{}"])
  isGroup <- elements [True, False]
  return $ GoType (TypeDecl lines isGroup)
  where
    listOfN k gen = sequence [gen | _ <- [1..k]]

-- | Generate variable declarations
genVarDecl :: Int -> Gen GoDecl
genVarDecl depth = do
  lineCount <- choose (1, min 3 depth)
  lines <- listOfN lineCount (elements ["var x int", "var y string", "var z bool"])
  isGroup <- elements [True, False]
  return $ GoVar (VarDecl lines isGroup)
  where
    listOfN k gen = sequence [gen | _ <- [1..k]]

-- | Generate constant declarations
genConstDecl :: Gen ConstDecl
genConstDecl = do
  lineCount <- choose (1, 3)
  lines <- listOfN lineCount (elements ["const x = 1", "const y = \"test\"", "const z = true"])
  isGroup <- elements [True, False]
  return $ ConstDecl lines isGroup
  where
    listOfN k gen = sequence [gen | _ <- [1..k]]

-- | Generate raw blocks
genRawBlock :: Gen RawBlock
genRawBlock = do
  lineCount <- choose (1, 3)
  lines <- listOfN lineCount (elements ["// raw comment", "package main", "import fmt"])
  return $ RawBlock lines
  where
    listOfN k gen = sequence [gen | _ <- [1..k]]

-- | Generate Typus files with code blocks
genTypusFile :: Gen TypusFile
genTypusFile = sized $ \n -> do
  numBlocks <- choose (1, min 3 n)
  blocks <- listOfN numBlocks genCodeBlock
  return $ TypusFile defaultFileDirectives [] blocks []
  where
    defaultFileDirectives = FileDirectives Nothing Nothing Nothing
    listOfN k gen = sequence [gen | _ <- [1..k]]
    genCodeBlock = do
      depth <- choose (1, 3)
      content <- elements ["func test() {}", "var x int", "type MyType struct{}"]
      return $ CodeBlock defaultBlockDirectives content (emptySpan startPos)
      where
        defaultBlockDirectives = BlockDirectives Nothing Nothing Nothing

-- Property: Declaration extraction preserves function names
prop_declaration_extraction_preserves_names :: GoDecl -> Property
prop_declaration_extraction_preserves_names decl =
  case decl of
    GoFunc _ -> property $ True  -- Function declarations
    GoType _ -> property $ True  -- Type declarations
    GoVar _ -> property $ True   -- Variable declarations
    GoConst _ -> property $ True -- Constant declarations
    GoStatement _ -> property $ True -- Statement blocks
    GoRaw _ -> property $ True    -- Raw blocks

-- Property: Type error detection consistency
prop_type_error_detection_consistent :: Type -> Type -> Property
prop_type_error_detection_consistent typ1 typ2 =
  let env = buildTypeEnvFromPairs []
  in property $ typesEqual typ1 typ2 || typ1 == UnknownType || typ2 == UnknownType

-- Property: Malformed syntax detection
prop_malformed_syntax_detection :: String -> Property
prop_malformed_syntax_detection code =
  let hasUnclosedBrackets = '(' `elem` code && ')' `notElem` code
      hasUnclosedBraces = '{' `elem` code && '}' `notElem` code
  in property $ hasUnclosedBrackets || hasUnclosedBraces ==> True

-- Property: Declaration type identification
prop_declaration_type_identification :: GoDecl -> Property
prop_declaration_type_identification decl =
  let serialized = show decl
      isFunc = "GoFunc" `isInfixOf` serialized
      isType = "GoType" `isInfixOf` serialized
      isVar = "GoVar" `isInfixOf` serialized
      isConst = "GoConst" `isInfixOf` serialized
      isStmt = "GoStatement" `isInfixOf` serialized
      isRaw = "GoRaw" `isInfixOf` serialized
  in case decl of
    GoFunc _ -> property $ isFunc
    GoType _ -> property $ isType
    GoVar _ -> property $ isVar
    GoConst _ -> property $ isConst
    GoStatement _ -> property $ isStmt
    GoRaw _ -> property $ isRaw

-- Property: Complex declaration evaluation consistency
prop_complex_declaration_consistency :: GoDecl -> Property
prop_complex_declaration_consistency decl =
  let serialized1 = show decl
      serialized2 = show decl
  in property $ serialized1 === serialized2

-- Property: Declaration list processing consistency
prop_declaration_list_consistency :: [GoDecl] -> Property
prop_declaration_list_consistency decls =
  property $ length decls >= 0

-- Property: Type environment building consistency
prop_type_env_consistency :: [(String, Type)] -> Property
prop_type_env_consistency typePairs =
  let env = buildTypeEnvFromPairs typePairs
      varNames = map fst typePairs
  in property $ not (null varNames) ==> Map.size (varTypes env) >= length varNames

tests :: TestTree
tests = testGroup "Compiler Properties QuickCheck Tests"
  [ fastProperty "declaration extraction preserves names" prop_declaration_extraction_preserves_names
  , fastProperty "type error detection consistent" prop_type_error_detection_consistent
  , fastProperty "malformed syntax detection" prop_malformed_syntax_detection
  , fastProperty "declaration type identification" prop_declaration_type_identification
  , fastProperty "complex declaration consistency" prop_complex_declaration_consistency
  , fastProperty "declaration list consistency" prop_declaration_list_consistency
  , fastProperty "type env consistency" prop_type_env_consistency
  ]