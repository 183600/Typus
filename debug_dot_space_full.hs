import Utils
import Data.List (isPrefixOf)

-- Test the full test case
main :: IO ()
main = do
  let s = " ."
  let withTabs = "\t\t" ++ s ++ "\t"
  putStrLn $ "s: " ++ show s
  putStrLn $ "withTabs: " ++ show withTabs
  let normalized = normalizeIndentation withTabs
  putStrLn $ "normalized: " ++ show normalized
  putStrLn $ "starts with \t\t: " ++ show ("\t\t" `isPrefixOf` normalized)
  putStrLn ""