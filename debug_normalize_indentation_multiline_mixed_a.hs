import Utils (normalizeIndentation)
import Data.List (isPrefixOf)

main :: IO ()
main = do
  -- Test the specific failing case
  let lines' = ["a"]
      withMixed = map ("\t  " ++) lines'
      unlinesWithMixed = unlines withMixed
      normalized = normalizeIndentation unlinesWithMixed
      normLines = lines normalized
      expected = "\t  \n"
  putStrLn $ "lines': " ++ show lines'
  putStrLn $ "withMixed: " ++ show withMixed
  putStrLn $ "unlines withMixed: " ++ show unlinesWithMixed
  putStrLn $ "normalized: " ++ show normalized
  putStrLn $ "normLines: " ++ show normLines
  putStrLn $ "expected: " ++ show expected
  putStrLn $ "normalized == expected: " ++ show (normalized == expected)
  
  -- Check if it's a single line case
  putStrLn $ "length lines': " ++ show (length lines')
  putStrLn $ "length withMixed: " ++ show (length withMixed)
  putStrLn $ "lines unlinesWithMixed: " ++ show (lines unlinesWithMixed)