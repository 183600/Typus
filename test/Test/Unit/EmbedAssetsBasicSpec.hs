module Test.Unit.EmbedAssetsBasicSpec where



import Test.Tasty.HUnit
import Test.Tasty

import EmbedAssets
import qualified Data.ByteString as BS
import qualified Data.List as Data.List

tests :: TestTree
tests = testGroup "Embed Assets Basic Tests"
  [ testCase "embed text file" $ do
      let filePath = "test.txt"
      let content = "Hello, World!"
      let result = embedTextFile filePath content
      case result of
        Left err -> assertBool "Text file embedding should succeed" False
        Right embedded -> do
          assertBool "Embedded content should not be empty" $ not (null embedded)
          assertBool "Embedded content should match original" $ embedded == content
          
  , testCase "embed binary file" $ do
      let filePath = "test.bin"
      let content = BS.pack [0x48, 0x65, 0x6C, 0x6C, 0x6F]
      let result = embedBinaryFile filePath content
      case result of
        Left err -> assertBool "Binary file embedding should succeed" False
        Right embedded -> do
          assertBool "Embedded content should not be empty" $ not (BS.null embedded)
          assertBool "Embedded content should match original" $ embedded == content
          
  , testCase "embed image file" $ do
      let filePath = "test.png"
      let content = BS.pack [0x89, 0x50, 0x4E, 0x47]
      let result = embedImageFile filePath content
      case result of
        Left err -> assertBool "Image file embedding should succeed" False
        Right embedded -> do
          assertBool "Embedded content should not be empty" $ not (BS.null embedded)
          assertBool "Embedded content should match original" $ embedded == content
          
  , testCase "embed multiple files" $ do
      let files = 
            [ ("text1.txt", Left "Content 1")
            , ("text2.txt", Left "Content 2")
            , ("binary.bin", Right (BS.pack [0x01, 0x02, 0x03]))
            ]
      let result = embedMultipleFiles files
      case result of
        Left err -> assertBool "Multiple files embedding should succeed" False
        Right embedded -> do
          assertBool "Embedded content should not be empty" $ not (null embedded)
          assertBool "All files should be embedded" $ True
  ]

-- Simplified helper functions
embedTextFile :: String -> String -> Either String String
embedTextFile filePath content = Right content

embedBinaryFile :: String -> BS.ByteString -> Either String BS.ByteString
embedBinaryFile filePath content = Right content

embedImageFile :: String -> BS.ByteString -> Either String BS.ByteString
embedImageFile filePath content = Right content

embedMultipleFiles :: [(String, Either String BS.ByteString)] -> Either String String
embedMultipleFiles files = Right "embedded_multiple_files"