import Utils
import Data.Char (isSpace)
import Data.List (isPrefixOf)

main :: IO ()
main = do
  let s = "a"
      withTabs = "\t\t" ++ s ++ "\t"
      normalized = Utils.normalizeIndentation withTabs
  
  putStrLn $ "Input: " ++ show s
  putStrLn $ "With tabs: " ++ show withTabs
  putStrLn $ "Normalized: " ++ show normalized
  putStrLn $ "Starts with tabs: " ++ show ("\t\t" `isPrefixOf` normalized)
  putStrLn $ "isSpace '\t': " ++ show (isSpace '\t')
  putStrLn $ "dropWhile isSpace tab string: " ++ show (dropWhile isSpace "\t\ta")