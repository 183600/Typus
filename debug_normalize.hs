import Utils
import Data.List (isPrefixOf)

main :: IO ()
main = do
  let input = " "
  let result = normalizeIndentation input
  putStrLn $ "Input: " ++ show input
  putStrLn $ "Result: " ++ show result
  putStrLn $ "Input lines: " ++ show (lines input)
  putStrLn $ "All space: " ++ show (all (\c -> c == ' ' || c == '\t') input)