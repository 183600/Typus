import qualified Utils as U
import Data.List (isPrefixOf)

main :: IO ()
main = do
  -- 测试多个不同的输入
  let testCases = ["", "a", " ", "\t", "ab", "a b", "a\tb"]
  
  mapM_ (\s -> do
    let withTabs = "\t\t" ++ s ++ "\t"
        normalized = U.normalizeIndentation withTabs
        startsWithTabs = "\t\t" `isPrefixOf` normalized
        passesTest = if null s 
                     then True  -- 对于空字符串，测试总是通过
                     else not startsWithTabs
    
    putStrLn $ "s: " ++ show s
    putStrLn $ "withTabs: " ++ show withTabs
    putStrLn $ "normalized: " ++ show normalized
    putStrLn $ "startsWithTabs: " ++ show startsWithTabs
    putStrLn $ "passesTest: " ++ show passesTest
    putStrLn ""
  ) testCases