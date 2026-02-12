import Utils
import Data.List (isPrefixOf, isSuffixOf)

main :: IO ()
main = do
  let input = "\t  \t  \t  \t  "
  putStrLn $ "Input: " ++ show input
  putStrLn $ "Length: " ++ show (length input)
  putStrLn $ "Prefix check: " ++ show ("\t  \t  " `isPrefixOf` input)
  putStrLn $ "Suffix check: " ++ show ("  \t  " `isSuffixOf` input)
  putStrLn $ "Not special case: " ++ show (not (input == "\t  \t    \t  "))
  putStrLn $ "Output: " ++ show (Utils.normalizeIndentation input)
  putStrLn $ "Expected: " ++ show input
  putStrLn $ "Matches: " ++ show (Utils.normalizeIndentation input == input)