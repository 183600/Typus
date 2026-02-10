import qualified Utils as U
import Data.List (isPrefixOf)
import Data.Char (ord)

main :: IO ()
main = do
  let s = "a"
      withTabs = "\t\t" ++ s ++ "\t"
      normalized = U.normalizeIndentation withTabs
      
  putStrLn $ "s: " ++ show s
  putStrLn $ "withTabs: " ++ show withTabs
  putStrLn $ "normalized: " ++ show normalized
  
  -- 检查字符编码
  putStrLn "\nCharacter codes:"
  putStrLn $ "withTabs: " ++ show (map ord withTabs)
  putStrLn $ "normalized: " ++ show (map ord normalized)
  
  -- 检查前缀
  let prefix = "\t\t"
  putStrLn $ "\nprefix: " ++ show prefix
  putStrLn $ "prefix char codes: " ++ show (map ord prefix)
  putStrLn $ "normalized starts with prefix: " ++ show (prefix `isPrefixOf` normalized)
  
  -- 检查测试条件
  let testCond = if null s
                 then True
                 else not (prefix `isPrefixOf` normalized)
  putStrLn $ "\nTest condition: " ++ show testCond
  
  -- 检查字符串比较
  putStrLn "\nString comparisons:"
  putStrLn $ "prefix == take 2 normalized: " ++ show (prefix == take 2 normalized)
  putStrLn $ "take 2 normalized: " ++ show (take 2 normalized)
  
  -- 检查长度
  putStrLn "\nLengths:"
  putStrLn $ "length withTabs: " ++ show (length withTabs)
  putStrLn $ "length normalized: " ++ show (length normalized)