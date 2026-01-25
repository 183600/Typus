{-# LANGUAGE OverloadedStrings #-}
module Test.Unit.EmbedAssetsExtractionSpec where



import Test.Tasty.HUnit
import Test.Tasty

import Test.Tasty.QuickCheck
import Data.List (isPrefixOf, isSuffixOf, isInfixOf)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import Data.Char (isLetter, isDigit)

-- Test embedded assets extraction properties
tests :: TestTree
tests = testGroup "Embed Assets Extraction Tests"
  [ testGroup "Text asset properties"
    [ testProperty "extracted text content matches original" $
        \content -> extractTextAsset (createTextAsset content) === content
    
    , testProperty "text asset encoding is reversible" $
        \content -> decodeTextAsset (encodeTextAsset content) === content
    
    , testProperty "empty text asset is handled" $
        extractTextAsset (createTextAsset "") === ""
    
    , testProperty "text asset preserves length" $
        \content -> length (extractTextAsset (createTextAsset content)) === length content
    
    , testProperty "text asset preserves character order" $
        \content -> extractTextAsset (createTextAsset content) == content
    
    , testProperty "text asset handles Unicode" $
        \unicode -> extractTextAsset (createTextAsset unicode) == unicode
    
    , testProperty "text asset handles special characters" $
        \special -> extractTextAsset (createTextAsset special) == special
    
    , testProperty "text asset handles whitespace" $
        \ws -> extractTextAsset (createTextAsset ws) == ws
    
    , testProperty "text asset handles multiline content" $
        \line1 line2 -> extractTextAsset (createTextAsset (line1 ++ "\n" ++ line2)) == line1 ++ "\n" ++ line2
    
    , testProperty "text asset handles repeated content" $
        \content n -> n > 0 ==> 
          extractTextAsset (createTextAsset (concat (replicate n content))) === concat (replicate n content)
    ]
  
  , testGroup "Binary asset properties"
    [ testProperty "extracted binary content matches original" $
        \bytes -> extractBinaryAsset (createBinaryAsset bytes) === bytes
    
    , testProperty "binary asset encoding is reversible" $
        \bytes -> decodeBinaryAsset (encodeBinaryAsset bytes) === bytes
    
    , testProperty "empty binary asset is handled" $
        extractBinaryAsset (createBinaryAsset []) === []
    
    , testProperty "binary asset preserves length" $
        \bytes -> length (extractBinaryAsset (createBinaryAsset bytes)) === length bytes
    
    , testProperty "binary asset preserves byte order" $
        \bytes -> extractBinaryAsset (createBinaryAsset bytes) == bytes
    
    , testProperty "binary asset handles all byte values" $
        \bytes -> extractBinaryAsset (createBinaryAsset bytes) == bytes
    
    , testProperty "binary asset handles repeated bytes" $
        \byte n -> n > 0 ==> 
          extractBinaryAsset (createBinaryAsset (replicate n byte)) === replicate n byte
    
    , testProperty "binary asset handles sequential bytes" $
        \start n -> n > 0 ==> 
          extractBinaryAsset (createBinaryAsset (take n [start..255])) === take n [start..255]
    
    , testProperty "binary asset handles null bytes" $
        \n -> n > 0 ==> 
          extractBinaryAsset (createBinaryAsset (replicate n 0)) === replicate n 0
    
    , testProperty "binary asset handles large content" $
        \bytes -> extractBinaryAsset (createBinaryAsset (take 1000 bytes)) === take 1000 bytes
    ]
  
  , testGroup "Asset management properties"
    [ testProperty "asset lookup is deterministic" $
        \name content -> lookupAsset name (createAssetMap [(name, content)]) === Just content
    
    , testProperty "asset lookup fails for missing assets" $
        \name -> lookupAsset name (createAssetMap []) === Nothing
    
    , testProperty "asset lookup returns first match" $
        \name content1 content2 -> 
          lookupAsset name (createAssetMap [(name, content1), (name, content2)]) === Just content1
    
    , testProperty "asset map preserves all entries" $
        \pairs -> all (\(name, content) -> lookupAsset name (createAssetMap pairs) === Just content) pairs
    
    , testProperty "asset map handles empty list" $
        null (createAssetMap [])
    
    , testProperty "asset map handles duplicate names" $
        \name content1 content2 -> 
          length (filter ((== name) . fst) (createAssetMap [(name, content1), (name, content2)])) >= 2
    
    , testProperty "asset map preserves order" $
        \pairs -> map fst (createAssetMap pairs) == map fst pairs
    
    , testProperty "asset map handles empty asset names" $
        \content -> lookupAsset "" (createAssetMap [("", content)]) === Just content
    
    , testProperty "asset map handles empty asset content" $
        \name -> lookupAsset name (createAssetMap [(name, "")]) === Just ""
    
    , testProperty "asset map handles special characters in names" $
        \name -> lookupAsset name (createAssetMap [(name, "content")]) === Just "content"
    ]
  ]

-- Helper functions
createTextAsset :: String -> String
createTextAsset content = "text:" ++ content

extractTextAsset :: String -> String
extractTextAsset asset = if "text:" `isPrefixOf` asset then drop 5 asset else ""

encodeTextAsset :: String -> String
encodeTextAsset content = "encoded:" ++ content

decodeTextAsset :: String -> String
decodeTextAsset encoded = if "encoded:" `isPrefixOf` encoded then drop 7 encoded else ""

createBinaryAsset :: [Int] -> String
createBinaryAsset bytes = "binary:" ++ concat (map show bytes)

extractBinaryAsset :: String -> [Int]
extractBinaryAsset asset = if "binary:" `isPrefixOf` asset 
                           then read (drop 7 asset) 
                           else []

encodeBinaryAsset :: [Int] -> String
encodeBinaryAsset bytes = "binary_encoded:" ++ concat (map show bytes)

decodeBinaryAsset :: String -> [Int]
decodeBinaryAsset encoded = if "binary_encoded:" `isPrefixOf` encoded
                           then read (drop 14 encoded)
                           else []

createAssetMap :: [(String, String)] -> [(String, String)]
createAssetMap = id

lookupAsset :: String -> [(String, String)] -> Maybe String
lookupAsset _ [] = Nothing
lookupAsset name ((n, content):rest) = if name == n then Just content else lookupAsset name rest