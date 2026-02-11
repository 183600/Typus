import Data.List (lines)

-- Check what lines returns
main :: IO ()
main = do
  let code = "a

"
  let inputLines = lines code
  putStrLn $ "code: " ++ show code
  putStrLn $ "inputLines: " ++ show inputLines
