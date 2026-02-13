import Utils
import Data.List (isPrefixOf)
import Data.Char (isSpace)

main :: IO ()
main = do
  -- Test with "\r"
  let s1 = "\r"
  let mixed1 = "\t  \t  " ++ s1 ++ "  \t  "
  let normalized1 = Utils.normalizeIndentation mixed1
  putStrLn $ "Test with s = \"\\r\":"
  putStrLn $ "  mixed: " ++ show mixed1
  putStrLn $ "  normalized: " ++ show normalized1
  putStrLn $ "  all isSpace mixed: " ++ show (all isSpace mixed1)
  putStrLn $ "  s == \"\\r\": " ++ show (s1 == "\r")
  putStrLn $ "  Expected (when s == \"\\r\"): \"    \""
  putStrLn $ "  Test passes: " ++ show (normalized1 == "    ")
  
  -- Test with "\n"
  let s2 = "\n"
  let mixed2 = "\t  \t  " ++ s2 ++ "  \t  "
  let normalized2 = Utils.normalizeIndentation mixed2
  putStrLn $ "\nTest with s = \"\\n\":"
  putStrLn $ "  mixed: " ++ show mixed2
  putStrLn $ "  normalized: " ++ show normalized2
  putStrLn $ "  all isSpace mixed: " ++ show (all isSpace mixed2)
  putStrLn $ "  s == \"\\n\": " ++ show (s2 == "\n")
  putStrLn $ "  Expected: mixed (since it's not handled specially)"
  putStrLn $ "  Test passes: " ++ show (normalized2 == mixed2)