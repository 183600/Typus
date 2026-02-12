import Utils (normalizeIndentation)
import Data.List (isPrefixOf)
import Data.Char (isSpace)

main :: IO ()
main = do
  let input = "\t\ta\t"
  putStrLn $ "Input: " ++ show input
  putStrLn $ "All isSpace: " ++ show (all isSpace input)
  putStrLn $ "IsPrefixOf \"\\t\\t\": " ++ show ("\t\t" `isPrefixOf` input)
  putStrLn $ "Not all isSpace: " ++ show (not (all isSpace input))
  putStrLn $ "Condition 1: " ++ show ("\t\t" `isPrefixOf` input && not (all isSpace input))
  
  let normalized = normalizeIndentation input
  putStrLn $ "Normalized: " ++ show normalized