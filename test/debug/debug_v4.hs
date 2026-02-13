import Data.Char (isSpace)

main :: IO ()
main = do
  let input = "\t\t\v\t"
  
  putStrLn $ "Input: " ++ show input
  putStrLn $ "all isSpace input: " ++ show (all isSpace input)
  mapM_ (\c -> putStrLn $ show c ++ " isSpace: " ++ show (isSpace c)) input