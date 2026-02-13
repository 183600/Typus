-- 这个脚本将修改Utils.hs，添加调试输出
import System.IO
import Data.List

main :: IO ()
main = do
  content <- readFile "src/Utils.hs"
  
  -- 查找normalizeIndentation函数的开始
  let startMarker = "normalizeIndentation :: String -> String"
  let startIdx = findIndex (isPrefixOf startMarker) (lines content)
  
  case startIdx of
    Nothing -> putStrLn "Could not find normalizeIndentation function"
    Just idx -> do
      let functionLines = take 100 (drop idx (lines content))
      putStrLn "First 100 lines of normalizeIndentation function:"
      mapM_ putStrLn functionLines