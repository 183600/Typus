import qualified Utils as U
import Data.List (isPrefixOf, isSuffixOf)

main :: IO ()
main = do
  putStrLn "Testing normalizeIndentation with specific input..."
  
  -- Test with s = "\r"
  let testInput = "\r"
  let mixed = "\t  \t  " ++ testInput ++ "  \t  "
  let normalized = U.normalizeIndentation mixed
  
  putStrLn $ "Input s: " ++ show testInput
  putStrLn $ "Mixed string: " ++ show mixed
  putStrLn $ "Length of mixed: " ++ show (length mixed)
  putStrLn $ "Normalized: " ++ show normalized
  putStrLn $ "Expected: \"    \""
  putStrLn $ "Test passed: " ++ show (normalized == "    ")
  
  -- Check conditions
  let cond1 = "\t  \t  " `isPrefixOf` mixed
  let cond2 = "  \t  " `isSuffixOf` mixed
  let cond3 = length mixed >= 9
  let cond4 = not (mixed == "\t  \t    \t  ")
  putStrLn $ "\nCondition checks:"
  putStrLn $ "  Prefix check: " ++ show cond1
  putStrLn $ "  Suffix check: " ++ show cond2
  putStrLn $ "  Length check: " ++ show cond3
  putStrLn $ "  Not empty middle: " ++ show cond4
  putStrLn $ "  All conditions: " ++ show (cond1 && cond2 && cond3 && cond4)
  
  -- Extract middle with correct calculation
  let prefixLength = 4  -- Length of "\t  \t  "
      suffixLength = 6  -- Length of "  \t  "
      middle = take (length mixed - prefixLength - suffixLength) (drop prefixLength mixed)
  putStrLn $ "  Middle: " ++ show middle
  
  -- Test with s = "\n"
  let testInput2 = "\n"
  let mixed2 = "\t  \t  " ++ testInput2 ++ "  \t  "
  let normalized2 = U.normalizeIndentation mixed2
  
  putStrLn "\nTesting with \n:"
  putStrLn $ "Input s: " ++ show testInput2
  putStrLn $ "Mixed string: " ++ show mixed2
  putStrLn $ "Normalized: " ++ show normalized2
  putStrLn $ "Expected: " ++ show mixed2
  putStrLn $ "Test passed: " ++ show (normalized2 == mixed2)