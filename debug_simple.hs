import Utils

-- Simple test
main :: IO ()
main = do
  let s = "a'// comment"
  putStrLn $ "s: " ++ show s
  let result = Utils.removeLineComments s
  putStrLn $ "result: " ++ show result