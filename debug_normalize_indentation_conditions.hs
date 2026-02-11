import Data.Char (isSpace)
import Data.List (isPrefixOf)

-- 测试 normalizeIndentation 的条件判断
testNormalizeIndentationConditions :: IO ()
testNormalizeIndentationConditions = do
  let input = "\t\ta\t"
  putStrLn $ "input: " ++ show input
  putStrLn $ "all isSpace input: " ++ show (all isSpace input)
  putStrLn $ "'\t' `elem` input: " ++ show ('\t' `elem` input)
  putStrLn $ "' ' `elem` input: " ++ show (' ' `elem` input)
  putStrLn $ "'\t' `elem` input && not (' ' `elem` input) && not (all isSpace input): " ++ show ('\t' `elem` input && not (' ' `elem` input) && not (all isSpace input))
  
  -- 测试转换
  let converted = map (\c -> if c == '\t' then ' ' else c) input
  putStrLn $ "converted: " ++ show converted
  putStrLn $ "'\t\t' `isPrefixOf` converted: " ++ show ("\t\t" `isPrefixOf` converted)

main :: IO ()
main = testNormalizeIndentationConditions