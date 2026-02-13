import Utils
import Data.List (isPrefixOf)

main :: IO ()
main = do
  let s = "a"
      withTabs = concat ["	", "	", s, "	"]
      normalized = normalizeIndentation withTabs
  putStrLn $ "Input: " ++ show withTabs
  putStrLn $ "Output: " ++ show normalized
  putStrLn $ "Starts with tabs: " ++ show ("		" `isPrefixOf` normalized)