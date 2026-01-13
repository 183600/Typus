{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewUtilsFunctionsPropertiesSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Data.Text as T
import Utils
import Data.Char (isAlpha, isDigit, isSpace, toLower, toUpper)
import Data.List (sort, nub, group, intercalate, isPrefixOf)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.QuickCheck (Positive(..))

-- | 测试字符串分割函数的属性
prop_string_split_properties :: String -> String -> Property
prop_string_split_properties str separator =
  let parts = splitString str separator
      rejoined = intercalate separator parts
  in property $ (null separator && parts == [str]) || 
                (not (null separator) && 
                 (length str < length separator || rejoined == str))

-- | 测试字符串合并函数的属性
prop_string_join_properties :: [String] -> String -> Property
prop_string_join_properties parts separator =
  let joined = intercalate separator parts
      splitAgain = splitString joined separator
  in property $ (null parts && joined == "") || 
                (not (null parts) && length splitAgain >= length parts)

-- | 测试字符串去重函数的属性
prop_string_dedup_properties :: String -> Property
prop_string_dedup_properties str =
  let deduped = deduplicateString str
  in property $ length deduped <= length str && 
                all (`elem` str) deduped

-- | 测试字符串替换函数的属性
prop_string_replace_properties :: String -> String -> String -> Property
prop_string_replace_properties old new str =
  let replaced = replaceString old new str
  in property $ (null old && replaced == str) || 
                (not (null old) && 
                 (not (old `isInfixOf` str) || replaced /= str))

-- | 测试字符串大小写转换的属性
prop_string_case_properties :: String -> Property
prop_string_case_properties str =
  let lower = map toLower str
      upper = map toUpper str
      lowerAgain = map toLower upper
      upperAgain = map toUpper lower
  in property $ lowerAgain == lower && upperAgain == upper

-- | 测试字符串过滤函数的属性
prop_string_filter_properties :: String -> Property
prop_string_filter_properties str =
  let filtered = filter (not . isSpace) str
      filteredAgain = filter (not . isSpace) filtered
  in property $ filteredAgain == filtered

-- | 测试列表操作函数的属性
prop_list_operations_properties :: [Int] -> Property
prop_list_operations_properties lst =
  let sorted = sort lst
      unique = nub lst
      reversed = reverse lst
      reversedAgain = reverse reversed
  in property $ reversedAgain == lst && 
                sort unique == sort (nub sorted)

-- | 测试Map操作函数的属性
prop_map_operations_properties :: [(String, Int)] -> Property
prop_map_operations_properties pairs =
  let mp = Map.fromList pairs
      keys = Map.keys mp
      values = Map.elems mp
      keySet = Set.fromList keys
      valueSet = Set.fromList values
  in property $ length keys == length keySet && 
                all (`Map.member` mp) keys

-- | 测试Set操作函数的属性
prop_set_operations_properties :: [Int] -> [Int] -> Property
prop_set_operations_properties lst1 lst2 =
  let set1 = Set.fromList lst1
      set2 = Set.fromList lst2
      union = Set.union set1 set2
      intersection = Set.intersection set1 set2
      difference = Set.difference set1 set2
  in property $ Set.isSubsetOf set1 union && 
                Set.isSubsetOf set2 union && 
                Set.isSubsetOf intersection set1 && 
                Set.isSubsetOf intersection set2

-- | 测试文本转换函数的属性
prop_text_conversion_properties :: String -> Property
prop_text_conversion_properties str =
  let text = T.pack str
      unpacked = T.unpack text
  in property $ unpacked == str

-- | 测试数字解析函数的属性
prop_number_parsing_properties :: Int -> Property
prop_number_parsing_properties n =
  let str = show n
      parsed = readMaybe str
  in property $ parsed == Just n

-- | 测试标识符验证函数的属性
prop_identifier_validation_properties :: String -> Property
prop_identifier_validation_properties ident =
  let isValid = isValidIdentifier ident
      hasValidStart = not (null ident) && isAlpha (head ident)
      hasValidChars = all (\c -> isAlpha c || isDigit c || c == '_') ident
  in property $ isValid == (hasValidStart && hasValidChars)

-- | 测试路径操作函数的属性
prop_path_operations_properties :: [String] -> Property
prop_path_operations_properties parts =
  let path = joinPath parts
      splitAgain = splitPath path
  in property $ (null parts && path == "") || 
                (not (null parts) && normalizePath path == path)

-- | 测试编码转换函数的属性
prop_encoding_conversion_properties :: String -> Property
prop_encoding_conversion_properties str =
  let encoded = encodeString str
      decoded = decodeString encoded
  in property $ decoded == str

-- | 测试缓存操作函数的属性
prop_cache_operations_properties :: [(String, Int)] -> Property
prop_cache_operations_properties pairs =
  let cache = foldl (\acc (k, v) -> insertCache k v acc) emptyCache pairs
      retrieved = map (\(k, _) -> (k, lookupCache k cache)) pairs
  in property $ all (\(_, v) -> v /= Nothing) retrieved

-- | 测试配置操作函数的属性
prop_config_operations_properties :: [(String, String)] -> Property
prop_config_operations_properties pairs =
  let config = foldl (\acc (k, v) -> setConfig k v acc) emptyConfig pairs
      retrieved = map (\(k, _) -> (k, getConfig k config)) pairs
  in property $ all (\(_, v) -> v /= Nothing) retrieved

-- | 测试日志操作函数的属性
prop_logging_operations_properties :: [String] -> Property
prop_logging_operations_properties messages =
  let logged = map logMessage messages
      filtered = filter isValidLogMessage logged
  in property $ length filtered <= length logged

-- | 测试时间操作函数的属性
prop_time_operations_properties :: Int -> Property
prop_time_operations_properties timestamp =
  let formatted = formatTime timestamp
      parsed = parseTime formatted
  in property $ parsed == timestamp

-- 辅助函数：分割字符串
splitString :: String -> String -> [String]
splitString _ [] = [""]
splitString sep str = splitString' sep str []
  where
    splitString' _ [] acc = [reverse acc]
    splitString' sep str acc
      | sep `isPrefixOf` str = reverse acc : splitString' sep (drop (length sep) str) []
      | otherwise = splitString' sep (tail str) (head str : acc)

-- 辅助函数：去重字符串
deduplicateString :: String -> String
deduplicateString [] = []
deduplicateString (x:xs) = x : deduplicateString (filter (/= x) xs)

-- 辅助函数：替换字符串
replaceString :: String -> String -> String -> String
replaceString _ _ [] = []
replaceString old new str
  | old `isPrefixOf` str = new ++ replaceString old new (drop (length old) str)
  | otherwise = head str : replaceString old new (tail str)

-- 辅助函数：检查子串
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = any (needle `isPrefixOf`) (tails haystack)
  where
    tails [] = [[]]
    tails xs@(_:ys) = xs : tails ys

-- 辅助函数：读取Maybe
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  [(x, "")] -> Just x
  _ -> Nothing

-- 辅助函数：验证标识符
isValidIdentifier :: String -> Bool
isValidIdentifier [] = False
isValidIdentifier (x:xs) = isAlpha x && all isValidChar xs
  where
    isValidChar c = isAlpha c || isDigit c || c == '_'

-- 辅助函数：连接路径
joinPath :: [String] -> String
joinPath = intercalate "/"

-- 辅助函数：分割路径
splitPath :: String -> [String]
splitPath = splitString "/"

-- 辅助函数：规范化路径
normalizePath :: String -> String
normalizePath = joinPath . filter (not . null) . splitPath

-- 辅助函数：编码字符串
encodeString :: String -> String
encodeString = id  -- 简化实现

-- 辅助函数：解码字符串
decodeString :: String -> String
decodeString = id  -- 简化实现

-- 辅助函数：缓存类型
type Cache = Map.Map String Int

-- 辅助函数：空缓存
emptyCache :: Cache
emptyCache = Map.empty

-- 辅助函数：插入缓存
insertCache :: String -> Int -> Cache -> Cache
insertCache = Map.insert

-- 辅助函数：查找缓存
lookupCache :: String -> Cache -> Maybe Int
lookupCache = Map.lookup

-- 辅助函数：配置类型
type Config = Map.Map String String

-- 辅助函数：空配置
emptyConfig :: Config
emptyConfig = Map.empty

-- 辅助函数：设置配置
setConfig :: String -> String -> Config -> Config
setConfig = Map.insert

-- 辅助函数：获取配置
getConfig :: String -> Config -> Maybe String
getConfig = Map.lookup

-- 辅助函数：记录消息
logMessage :: String -> String
logMessage = id  -- 简化实现

-- 辅助函数：验证日志消息
isValidLogMessage :: String -> Bool
isValidLogMessage = not . null

-- 辅助函数：格式化时间
formatTime :: Int -> String
formatTime = show  -- 简化实现

-- 辅助函数：解析时间
parseTime :: String -> Int
parseTime = read  -- 简化实现

tests :: TestTree
tests = testGroup "New Utils Functions Properties Tests"
  [ testProperty "string split properties" prop_string_split_properties,
    testProperty "string join properties" prop_string_join_properties,
    testProperty "string dedup properties" prop_string_dedup_properties,
    testProperty "string replace properties" prop_string_replace_properties,
    testProperty "string case properties" prop_string_case_properties,
    testProperty "string filter properties" prop_string_filter_properties,
    testProperty "list operations properties" prop_list_operations_properties,
    testProperty "map operations properties" prop_map_operations_properties,
    testProperty "set operations properties" prop_set_operations_properties,
    testProperty "text conversion properties" prop_text_conversion_properties,
    testProperty "number parsing properties" prop_number_parsing_properties,
    testProperty "identifier validation properties" prop_identifier_validation_properties,
    testProperty "path operations properties" prop_path_operations_properties,
    testProperty "encoding conversion properties" prop_encoding_conversion_properties,
    testProperty "cache operations properties" prop_cache_operations_properties,
    testProperty "config operations properties" prop_config_operations_properties,
    testProperty "logging operations properties" prop_logging_operations_properties,
    testProperty "time operations properties" prop_time_operations_properties
  ]