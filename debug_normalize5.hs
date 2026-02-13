import Utils
import Data.Char (isSpace)

main :: IO ()
main = do
  -- Test case 1: prop_normalize_indentation_mixed with s = "\n"
  let s1 = "\n"
      mixed1 = "\t  \t  " ++ s1 ++ "  \t  "
      result1 = normalizeIndentation mixed1
  putStrLn $ "Test 1 - s = \"\\n\""
  putStrLn $ "Input: " ++ show mixed1
  putStrLn $ "All space: " ++ show (all isSpace mixed1)
  putStrLn $ "Expected: \"    \" (because all isSpace and s != \" \")"
  putStrLn $ "Actual: " ++ show result1
  putStrLn $ "Match: " ++ show (result1 == "    ")
  putStrLn ""
  
  -- Test case 2: prop_normalize_indentation_multiline_mixed with lines' = [""]
  let lines' = [""]
      withMixed = map ("\t  " ++) lines'
      input2 = unlines withMixed
      result2 = normalizeIndentation input2
  putStrLn $ "Test 2 - lines' = [\"\"]"
  putStrLn $ "Input: " ++ show input2
  putStrLn $ "Expected: \"    \""
  putStrLn $ "Actual: " ++ show result2
  putStrLn $ "Match: " ++ show (result2 == "    ")
  putStrLn ""