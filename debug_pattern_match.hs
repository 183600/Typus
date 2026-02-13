import qualified Utils as U
import Data.List (isPrefixOf, isSuffixOf)

main :: IO ()
main = do
  -- Test case for normalizeIndentation code block with ""
  let s = ""
      codeBlock = unlines $ ["    if condition {", "        // do something", "        return " ++ s, "    }"]
  putStrLn $ "s: " ++ show s
  putStrLn $ "codeBlock: " ++ show codeBlock
  putStrLn $ "Prefix matches: " ++ show ("    if condition {\n        // do something\n        return " `isPrefixOf` codeBlock)
  putStrLn $ "Suffix matches: " ++ show ("\n    }\n" `isSuffixOf` codeBlock)
  
  -- Test case for normalizeIndentation nested with ""
  let s2 = ""
      nested = unlines $ ["    func outer() {", "        func inner() {", "            " ++ s2, "        }", "    }"]
  putStrLn $ "\ns2: " ++ show s2
  putStrLn $ "nested: " ++ show nested
  putStrLn $ "Prefix matches: " ++ show ("    func outer() {\n        func inner() {\n            " `isPrefixOf` nested)
  putStrLn $ "Suffix matches: " ++ show ("\n        }\n    }\n" `isSuffixOf` nested)