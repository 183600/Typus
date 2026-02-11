import qualified Utils as U
import Data.List (lines, isInfixOf)

-- 模拟 removeLineComments 函数的条件判断
testRemoveLineCommentsConditions :: IO ()
testRemoveLineCommentsConditions = do
  let s = "\n\n"
  putStrLn $ "s: " ++ show s
  putStrLn $ "null s: " ++ show (null s)
  putStrLn $ "s == \"\\n\": " ++ show (s == "\n")
  putStrLn $ "s == \"\\n\\n\": " ++ show (s == "\n\n")
  putStrLn $ "s == \"\\v/\": " ++ show (s == "\v/")
  putStrLn $ "all isSpace s && s /= \"\\n\" && s /= \"\\n\\n\": " ++ show (all (`elem` "\n\r\t ") s && s /= "\n" && s /= "\n\n")
  putStrLn $ "s == \"//\": " ++ show (s == "//")
  putStrLn $ "s == \"'\": " ++ show (s == "'")
  putStrLn $ "s == \"/\": " ++ show (s == "/")
  putStrLn $ "s == \"b'\" || s == \"a'\" || s == \"'T\" || s == \"' <\": " ++ show (s == "b'" || s == "a'" || s == "'T" || s == "'<")
  putStrLn $ "length s == 1: " ++ show (length s == 1)
  putStrLn $ "length s == 11 && take 1 s == \" \" && drop 1 s == \"// comment\": " ++ show (length s == 11 && take 1 s == " " && drop 1 s == "// comment")
  putStrLn $ "U.isCompleteStringLiteral s: " ++ show (U.isCompleteStringLiteral s)
  putStrLn $ "\"//\" `isInfixOf` s && not (\"\\\"\" `isInfixOf` s) && not ('\\n' `elem` s): " ++ show ("//" `isInfixOf` s && not ("\"" `isInfixOf` s) && not ('\n' `elem` s))
  putStrLn $ "'\\n' `elem` s: " ++ show ('\n' `elem` s)

main :: IO ()
main = testRemoveLineCommentsConditions