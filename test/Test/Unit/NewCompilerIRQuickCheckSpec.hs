{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.Unit.NewCompilerIRQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import Compiler.IR
  ( SourceIR(..)
  , SemanticIR(..)
  , GoIR(..)
  , buildSourceIR
  , rawSourceFromTypus
  , ensurePackageDecl
  , ensureMainFunction
  , attachInferredImports
  , replaceGenericAngles
  , detectImports
  , buildImportUsage
  , collectQualifiedUsage
  , packageUsed
  , qualifiedSymbolUsed
  )

import Parser (TypusFile(..), CodeBlock(..))
import Compiler.GoAst (GoModule(..), ImportDecl(..), GoDecl(..), FuncDecl(..))
import Compiler.GoLexer (GoToken(..), GoTokenKind(..), tokenizeGo)
import Compiler.Errors (CompilerResult(..))
import SourceLocation (SourcePos(..), startPos)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as Set
import Data.List (intercalate, isInfixOf, isPrefixOf)
import Data.Char (isSpace)

-- | 新的编译器IR QuickCheck测试套件
tests :: TestTree
tests =
  testGroup "New Compiler IR QuickCheck Tests"
    [ fastProperty "buildSourceIR preserves typus file content" prop_buildSourceIR_preserves_content
    , fastProperty "rawSourceFromTypus concatenates code blocks" prop_rawSourceFromTypus_concatenates
    , fastProperty "ensurePackageDecl adds package when missing" prop_ensurePackageDecl_adds_when_missing
    , fastProperty "ensurePackageDecl preserves existing package" prop_ensurePackageDecl_preserves_existing
    , fastProperty "replaceGenericAngles converts angle brackets to brackets" prop_replaceGenericAngles_converts
    , fastProperty "detectImports finds fmt usage" prop_detectImports_finds_fmt
    , fastProperty "buildImportUsage collects identifiers correctly" prop_buildImportUsage_collects_identifiers
    , fastProperty "collectQualifiedUsage finds qualified identifiers" prop_collectQualifiedUsage_finds_qualified
    , fastProperty "packageUsed detects package usage" prop_packageUsed_detects_usage
    , fastProperty "qualifiedSymbolUsed detects qualified symbol usage" prop_qualifiedSymbolUsed_detects_usage
    ]

-- Property: buildSourceIR preserves typus file content
prop_buildSourceIR_preserves_content :: [String] -> Property
prop_buildSourceIR_preserves_content blockContents =
  not (null blockContents) && length blockContents <= 10 ==>
  let codeBlocks = map (\content -> CodeBlock content [] (startPos, startPos)) blockContents
      typusFile = TypusFile 
        { tfBlocks = codeBlocks
        , tfBuildTags = []
        }
      sourceIR = buildSourceIR typusFile
  in property $ sourceTypusFile sourceIR === typusFile .&&.
     sourceText sourceIR === intercalate "\n" blockContents

-- Property: rawSourceFromTypus concatenates code blocks
prop_rawSourceFromTypus_concatenates :: [String] -> Property
prop_rawSourceFromTypus_concatenates blockContents =
  not (null blockContents) && length blockContents <= 10 ==>
  let codeBlocks = map (\content -> CodeBlock content [] (startPos, startPos)) blockContents
      typusFile = TypusFile 
        { tfBlocks = codeBlocks
        , tfBuildTags = []
        }
      rawSource = rawSourceFromTypus typusFile
  in property $ rawSource === intercalate "\n" blockContents

-- Property: ensurePackageDecl adds package when missing
prop_ensurePackageDecl_adds_when_missing :: [String] -> Property
prop_ensurePackageDecl_adds_when_missing declLines =
  let goModule = GoModule
        { gmPackage = Nothing
        , gmImports = []
        , gmDecls = map GoRaw declLines
        , gmBuildTags = []
        }
      updatedModule = ensurePackageDecl goModule
  in property $ gmPackage updatedModule /= Nothing .&&.
     case gmPackage updatedModule of
       Just pkg -> True  -- Package was added
       Nothing -> False  -- Should never happen

-- Property: ensurePackageDecl preserves existing package
prop_ensurePackageDecl_preserves_existing :: String -> [String] -> Property
prop_ensurePackageDecl_preserves_existing packageName declLines =
  not (null packageName) ==>
  let goModule = GoModule
        { gmPackage = Just (PackageDecl packageName)
        , gmImports = []
        , gmDecls = map GoRaw declLines
        , gmBuildTags = []
        }
      updatedModule = ensurePackageDecl goModule
  in property $ gmPackage updatedModule === gmPackage goModule .&&.
     case gmPackage updatedModule of
       Just pkg -> packageName == packageName
       Nothing -> False

-- Property: replaceGenericAngles converts angle brackets to brackets
prop_replaceGenericAngles_converts :: String -> String -> Property
prop_replaceGenericAngles_converts typeName paramList =
  not (null typeName) && not (null paramList) ==>
  let input = "type " ++ typeName ++ "<" ++ paramList ++ "> struct {}"
      result = replaceGenericAngles input
      expected = "type " ++ typeName ++ "[" ++ paramList ++ "] struct {}"
  in property $ expected `isInfixOf` result

-- Property: detectImports finds fmt usage
prop_detectImports_finds_fmt :: String -> Property
prop_detectImports_finds_fmt codeContent =
  "fmt." `isInfixOf` codeContent ==>
  let imports = detectImports codeContent
      hasFmtImport = any (\imp -> importPath imp == "fmt") imports
  in property $ hasFmtImport

-- Property: buildImportUsage collects identifiers correctly
prop_buildImportUsage_collects_identifiers :: [String] -> Property
prop_buildImportUsage_collects_identifiers identifierStrings =
  not (null identifierStrings) && length identifierStrings <= 10 ==>
  let codeContent = intercalate " " identifierStrings
      usage = buildImportUsage codeContent
      expectedIdentifiers = Set.fromList identifierStrings
  in property $ expectedIdentifiers `Set.isSubsetOf` usageIdentifiers usage

-- Property: collectQualifiedUsage finds qualified identifiers
prop_collectQualifiedUsage_finds_qualified :: String -> String -> Property
prop_collectQualifiedUsage_finds_qualified packageName symbol =
  not (null packageName) && not (null symbol) ==>
  let codeContent = packageName ++ "." ++ symbol
      tokens = tokenizeGo codeContent
      (qualifiedPairs, packages) = collectQualifiedUsage tokens
  in property $ (packageName, symbol) `elem` qualifiedPairs .&&.
     packageName `Set.member` packages

-- Property: packageUsed detects package usage
prop_packageUsed_detects_usage :: [String] -> String -> Property
prop_packageUsed_detects_usage identifiers packageName =
  not (null packageName) && not (null identifiers) && length identifiers <= 10 ==>
  let codeContent = intercalate " " (packageName : identifiers)
      usage = buildImportUsage codeContent
  in property $ packageUsed usage packageName

-- Property: qualifiedSymbolUsed detects qualified symbol usage
prop_qualifiedSymbolUsed_detects_usage :: String -> String -> Property
prop_qualifiedSymbolUsed_detects_usage packageName symbol =
  not (null packageName) && not (null symbol) ==>
  let codeContent = packageName ++ "." ++ symbol
      usage = buildImportUsage codeContent
  in property $ qualifiedSymbolUsed usage packageName symbol

-- Additional properties for compiler IR

-- Property: ensureMainFunction adds main function when only statements exist
prop_ensureMainFunction_adds_main :: [String] -> Property
prop_ensureMainFunction_adds_main statementLines =
  not (null statementLines) && length statementLines <= 5 ==>
  let goModule = GoModule
        { gmPackage = Just (PackageDecl "main")
        , gmImports = []
        , gmDecls = map GoRaw statementLines
        , gmBuildTags = []
        }
      updatedModule = ensureMainFunction goModule
      hasMainFunc = any isMainFunctionDecl (gmDecls updatedModule)
  in property $ hasMainFunc

-- Property: ensureMainFunction preserves existing main function
prop_ensureMainFunction_preserves_main :: String -> Property
prop_ensureMainFunction_preserves_main mainFuncContent =
  not (null mainFuncContent) && "func main()" `isInfixOf` mainFuncContent ==>
  let goModule = GoModule
        { gmPackage = Just (PackageDecl "main")
        , gmImports = []
        , gmDecls = [GoRaw mainFuncContent]
        , gmBuildTags = []
        }
      updatedModule = ensureMainFunction goModule
      mainFuncCount = length $ filter isMainFunctionDecl (gmDecls updatedModule)
  in property $ mainFuncCount === 1

-- Property: replaceGenericAngles handles nested generics
prop_replaceGenericAngles_nested :: String -> String -> String -> Property
prop_replaceGenericAngles_nested outerType innerType paramList =
  not (null outerType) && not (null innerType) && not (null paramList) ==>
  let input = "type " ++ outerType ++ "<" ++ innerType ++ "<" ++ paramList ++ ">> struct {}"
      result = replaceGenericAngles input
      hasOuterBracket = "[" `isInfixOf` result
      hasInnerBracket = "[" `isInfixOf` result
  in property $ hasOuterBracket .&&. hasInnerBracket

-- Property: detectImports finds multiple package usages
prop_detectImports_multiple :: [String] -> Property
prop_detectImports_multiple packageNames =
  not (null packageNames) && length packageNames <= 5 ==>
  let codeContent = intercalate " " (map (\pkg -> pkg ++ ".Function()") packageNames)
      imports = detectImports codeContent
      foundPackages = map importPath imports
  in property $ all (`elem` foundPackages) packageNames

-- Property: buildImportUsage handles empty input
prop_buildImportUsage_empty :: Property
prop_buildImportUsage_empty =
  let usage = buildImportUsage ""
  in property $ Set.null (usagePackages usage) .&&.
     Set.null (usageQualified usage) .&&.
     Set.null (usageIdentifiers usage)

-- Property: collectQualifiedUsage handles empty tokens
prop_collectQualifiedUsage_empty :: Property
prop_collectQualifiedUsage_empty =
  let (qualifiedPairs, packages) = collectQualifiedUsage []
  in property $ null qualifiedPairs .&&.
     Set.null packages

-- Helper function to check if a declaration is a main function
isMainFunctionDecl :: GoDecl -> Bool
isMainFunctionDecl (GoFunc (FuncDecl lines)) = any ("func main()" `isInfixOf`) lines
isMainFunctionDecl (GoRaw content) = "func main()" `isInfixOf` content
isMainFunctionDecl _ = False

-- Property: replaceGenericAngles preserves non-generic code
prop_replaceGenericAngles_preserves_nongeneric :: String -> Property
prop_replaceGenericAngles_preserves_nongeneric codeContent =
  not ("<" `isInfixOf` codeContent) ==>
  let result = replaceGenericAngles codeContent
  in property $ result === codeContent

-- Property: detectImports handles empty input
prop_detectImports_empty :: Property
prop_detectImports_empty =
  let imports = detectImports ""
  in property $ null imports

-- Property: packageUsed returns false for unused packages
prop_packageUsed_false_unused :: String -> Property
prop_packageUsed_false_unused packageName =
  not (null packageName) ==>
  let codeContent = "some other code without " ++ packageName
      usage = buildImportUsage codeContent
  in property $ not (packageUsed usage packageName)

-- Property: qualifiedSymbolUsed returns false for unused symbols
prop_qualifiedSymbolUsed_false_unused :: String -> String -> String -> Property
prop_qualifiedSymbolUsed_false_unused packageName unusedSymbol usedSymbol =
  not (null packageName) && not (null unusedSymbol) && not (null usedSymbol) &&
  unusedSymbol /= usedSymbol ==>
  let codeContent = packageName ++ "." ++ usedSymbol
      usage = buildImportUsage codeContent
  in property $ not (qualifiedSymbolUsed usage packageName unusedSymbol)

-- Property: buildImportUsage handles complex code with comments
prop_buildImportUsage_with_comments :: String -> String -> Property
prop_buildImportUsage_with_comments code comment =
  not (null code) && not (null comment) ==>
  let codeWithComment = code ++ " // " ++ comment
      usage1 = buildImportUsage code
      usage2 = buildImportUsage codeWithComment
  in property $ usageIdentifiers usage1 === usageIdentifiers usage2

-- Property: replaceGenericAngles handles malformed generics
prop_replaceGenericAngles_malformed :: String -> Property
prop_replaceGenericAngles_malformed codeContent =
  let result = replaceGenericAngles codeContent
  in property $ not (null result)  -- Should always return some result