module Test.Unit.EmbedAssetsConsistencySpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import EmbedAssets

-- Test asset embedding consistency
prop_asset_embedding_consistent :: String -> Property
prop_asset_embedding_consistent assetPath =
  let embedded1 = embedAsset assetPath
      embedded2 = embedAsset assetPath
  in property $ embedded1 === embedded2

-- Test asset extraction
prop_asset_extraction_roundtrip :: String -> String -> Property
prop_asset_extraction_roundtrip assetPath content =
  let embedded = embedAssetContent assetPath content
      extracted = extractAssetContent embedded
  in property $ extracted === content

-- Test asset path resolution
prop_asset_path_resolution :: String -> Property
prop_asset_path_resolution relativePath =
  let resolved = resolveAssetPath relativePath
  in property $ not (null resolved)

-- Test asset metadata preservation
prop_asset_metadata_preservation :: String -> [(String, String)] -> Property
prop_asset_metadata_preservation assetPath metadata =
  let asset = createAssetWithMetadata assetPath metadata
      preservedMetadata = getAssetMetadata asset
  in property $ preservedMetadata === metadata

-- Test asset compression
prop_asset_compression_lossless :: String -> Property
prop_asset_compression_lossless content =
  let compressed = compressAsset content
      decompressed = decompressAsset compressed
  in property $ content === decompressed

tests :: TestTree
tests = testGroup "EmbedAssets Consistency Tests"
  [ testProperty "asset embedding consistent" prop_asset_embedding_consistent
  , testProperty "asset extraction roundtrip" prop_asset_extraction_roundtrip
  , testProperty "asset path resolution" prop_asset_path_resolution
  , testProperty "asset metadata preservation" prop_asset_metadata_preservation
  , testProperty "asset compression lossless" prop_asset_compression_lossless
  ]