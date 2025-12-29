module Test.Unit.NewCabalGoToolchainQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), counterexample, forAll, oneof, elements, listOf, suchThat)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Maybe (isJust, isNothing)

import GoToolchain
import TestSupport.QuickCheck (fastProperty)

-- | QuickCheck tests for GoToolchain module Go toolchain functions
tests :: TestTree
tests =
  testGroup "New Cabal GoToolchain QuickCheck Tests"
    [ testProperty "GoVersion construction preserves fields" prop_goVersionConstruction
    , testProperty "GoVersion equality works correctly" prop_goVersionEquality
    , testProperty "GoVersion ordering respects semantic versioning" prop_goVersionOrdering
    , testProperty "GoVersion Show instance is formatted correctly" prop_goVersionShowFormatting
    , testProperty "GoToolchainConfig construction preserves fields" prop_goToolchainConfigConstruction
    , testProperty "GoToolchainConfig equality works correctly" prop_goToolchainConfigEquality
    , testProperty "parseGoVersion handles valid versions" prop_parseGoVersionValid
    , testProperty "parseGoVersion handles invalid versions" prop_parseGoVersionInvalid
    , testProperty "isGoVersionSupported checks version compatibility" prop_isGoVersionSupported
    , testProperty "findGoTool finds go tool" prop_findGoTool
    , testGroup "Edge cases"
        [ testCase "parseGoVersion handles standard version format" $ do
            let result = parseGoVersion "1.21.0"
            case result of
                Just (GoVersion major minor patch) -> do
                    major @?= 1
                    minor @?= 21
                    patch @?= 0
                _ -> assertFailure "Expected Just (GoVersion 1 21 0)"
        , testCase "parseGoVersion handles invalid format" $ do
            let result = parseGoVersion "invalid.version"
            result @?= Nothing
        , testCase "GoVersion ordering respects major version" $ do
            let v1 = GoVersion 1 20 0
                v2 = GoVersion 2 0 0
            compare v1 v2 @?= LT
        , testCase "GoVersion ordering respects minor version" $ do
            let v1 = GoVersion 1 20 0
                v2 = GoVersion 1 21 0
            compare v1 v2 @?= LT
        , testCase "GoVersion ordering respects patch version" $ do
            let v1 = GoVersion 1 21 0
                v2 = GoVersion 1 21 1
            compare v1 v2 @?= LT
        , testCase "GoVersion Show instance formats correctly" $ do
            let version = GoVersion 1 21 5
                showOutput = show version
            showOutput @?= "1.21.5"
        , testCase "isGoVersionSupported checks minimum version" $ do
            let version = GoVersion 1 21 0
                minVersion = GoVersion 1 20 0
            isGoVersionSupported version minVersion @?= True
        ]
    ]

-- | Property: GoVersion construction preserves fields
prop_goVersionConstruction :: Int -> Int -> Int -> Property
prop_goVersionConstruction major minor patch = 
  major >= 0 && minor >= 0 && patch >= 0 ==>
  let version = GoVersion major minor patch
  in goMajor version === major .&&.
     goMinor version === minor .&&.
     goPatch version === patch
  where
    goMajor (GoVersion m _ _) = m
    goMinor (GoVersion _ m _) = m
    goPatch (GoVersion _ _ p) = p

-- | Property: GoVersion equality works correctly
prop_goVersionEquality :: Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_goVersionEquality major1 minor1 patch1 major2 minor2 patch2 = 
  major1 >= 0 && minor1 >= 0 && patch1 >= 0 &&
  major2 >= 0 && minor2 >= 0 && patch2 >= 0 ==>
  let v1 = GoVersion major1 minor1 patch1
      v2 = GoVersion major2 minor2 patch2
  in (v1 == v2) === (major1 == major2 && minor1 == minor2 && patch1 == patch2)

-- | Property: GoVersion ordering respects semantic versioning
prop_goVersionOrdering :: Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_goVersionOrdering major1 minor1 patch1 major2 minor2 patch2 = 
  major1 >= 0 && minor1 >= 0 && patch1 >= 0 &&
  major2 >= 0 && minor2 >= 0 && patch2 >= 0 ==>
  let v1 = GoVersion major1 minor1 patch1
      v2 = GoVersion major2 minor2 patch2
      comparison = compare v1 v2
      expectedComparison = compare (major1, minor1, patch1) (major2, minor2, patch2)
  in comparison === expectedComparison

-- | Property: GoVersion Show instance is formatted correctly
prop_goVersionShowFormatting :: Int -> Int -> Int -> Property
prop_goVersionShowFormatting major minor patch = 
  major >= 0 && minor >= 0 && patch >= 0 ==>
  let version = GoVersion major minor patch
      showOutput = show version
      expected = show major ++ "." ++ show minor ++ "." ++ show patch
  in showOutput === expected

-- | Property: GoToolchainConfig construction preserves fields
prop_goToolchainConfigConstruction :: String -> String -> String -> Property
prop_goToolchainConfigConstruction goPath goRoot goCache = 
  let config = GoToolchainConfig goPath goRoot goCache
  in goToolchainGoPath config === goPath .&&.
     goToolchainGoRoot config === goRoot .&&.
     goToolchainGoCache config === goCache
  where
    goToolchainGoPath (GoToolchainConfig gp _ _) = gp
    goToolchainGoRoot (GoToolchainConfig _ gr _) = gr
    goToolchainGoCache (GoToolchainConfig _ _ gc) = gc

-- | Property: GoToolchainConfig equality works correctly
prop_goToolchainConfigEquality :: String -> String -> String -> String -> String -> String -> Property
prop_goToolchainConfigEquality goPath1 goRoot1 goCache1 goPath2 goRoot2 goCache2 = 
  let config1 = GoToolchainConfig goPath1 goRoot1 goCache1
      config2 = GoToolchainConfig goPath2 goRoot2 goCache2
  in (config1 == config2) === (goPath1 == goPath2 && goRoot1 == goRoot2 && goCache1 == goCache2)

-- | Property: parseGoVersion handles valid versions
prop_parseGoVersionValid :: Int -> Int -> Int -> Property
prop_parseGoVersionValid major minor patch = 
  major >= 0 && minor >= 0 && patch >= 0 ==>
  let versionString = show major ++ "." ++ show minor ++ "." ++ show patch
      result = parseGoVersion versionString
  in isJust result
  where
    isJust Nothing = False
    isJust (Just _) = True

-- | Property: parseGoVersion handles invalid versions
prop_parseGoVersionInvalid :: String -> Property
prop_parseGoVersionInvalid invalidVersion = 
  not (isValidVersionFormat invalidVersion) ==>
  let result = parseGoVersion invalidVersion
  in isNothing result
  where
    isNothing Nothing = True
    isNothing (Just _) = False
    isValidVersionFormat s = case words s of
      [majorPart, minorPart, patchPart] -> 
        all (all (`elem` ['0'..'9'])) [majorPart, minorPart, patchPart]
      _ -> False

-- | Property: isGoVersionSupported checks version compatibility
prop_isGoVersionSupported :: Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_isGoVersionSupported major minor patch minMajor minMinor minPatch = 
  major >= 0 && minor >= 0 && patch >= 0 &&
  minMajor >= 0 && minMinor >= 0 && minPatch >= 0 ==>
  let version = GoVersion major minor patch
      minVersion = GoVersion minMajor minMinor minPatch
      supported = isGoVersionSupported version minVersion
      expected = compare version minVersion /= LT
  in supported === expected

-- | Property: findGoTool finds go tool
prop_findGoTool :: Property
prop_findGoTool = 
  let result = findGoTool
  in isJust result
  where
    isJust Nothing = False
    isJust (Just _) = True

-- Helper operator for composing properties
(.&&.) :: Property -> Property -> Property
(.&&.) = (&&)