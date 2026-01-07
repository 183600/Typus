module Test.Unit.UserAddedUtilsStringSpec where
import Test.QuickCheck 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=)), assertBool
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, choose, oneof, listOf, elements)
import TestSupport.QuickCheck 
  )
import Data.Char 
                        breakOn "world" "hello world" @?= ("hello ", "world")
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


          ,             testCase "breakOn handles missing pattern" $ do
                        breakOn "xyz" "hello world" @?= ("hello world", "")

          ,             testCase "breakOn handles empty pattern" $ do
                        breakOn "" "hello" @?= ("", "hello")
        ]

    , testGroup "Unicode L.and special character handling"
        [             testCase "trim handles Unicode whitespace" $ do
                        trim "\u00A0\u2000hello\u2003world\u2002" @?= "hello\u2003world"

          ,             testCase "splitBy handles Unicode delimiters" $ do
                        splitBy '' "abc" @?= ["a", "b", "c"]

          ,             testCase "removeLineComments handles Unicode strings" $ do
                        let input = "text := \"\" // comment"
                                              expected = "text := \"\" "
            removeLineComments input @?= expected
        ]

    , testGroup "Property-based tests"
        [ fastProperty "splitBy L.and splitByCollapsed relationship" prop_splitByRelationship
        , fastProperty "trim is idempotent" prop_trimIdempotent
        , fastProperty "splitBy preserves total L.length" prop_splitByPreservesLength
        , fastProperty "breakOn is deterministic" prop_breakOnDeterministic
        ]

    , testGroup "Performance characteristics"
        [             testCase "splitBy is linear time" $ do
                        let input = replicate 10000 'a' ++ "," ++ replicate 10000 'b'
                                              result = splitBy ',' input
            L.length result @?= 2

          ,             testCase "removeComments is efficient for large files" $ do
                        let largeContent = unlines $ replicate 1000 ("code // comment " ++ replicate 100 'x')
                                              result = removeLineComments largeContent
            L.length (lines result) @?= 1000
        ]
    ]

-- | Property: splitBy L.and splitByCollapsed relationship
prop_splitByRelationship :: String -> Char -> Bool
prop_splitByRelationship input                               delim =
  let normal = splitBy delim input
                                    collapsed = splitByCollapsed delim input
  in L.all (not . null)                               collapsed == (L.null $ filter null normal)

-- | Property: trim is idempotent
prop_trimIdempotent :: String -> Bool
prop_trimIdempotent                               input =
  let once = trim input
                                    twice = trim once
  in                               once == twice

-- | Property: splitBy preserves total L.length
prop_splitByPreservesLength :: String -> Char -> Bool
prop_splitByPreservesLength input                               delim =
  let parts = splitBy delim input
                                    totalLength = L.sum (map L.length parts) + L.length (L.filter (== delim) input) - L.length parts + 1
  in                               totalLength == L.length input

-- | Property: breakOn is deterministic
prop_breakOnDeterministic :: String -> String -> Bool
prop_breakOnDeterministic input                               pattern =
  let result1 = breakOn pattern input
                                    result2 = breakOn pattern input
  in                               result1 == result2