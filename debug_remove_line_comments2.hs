import qualified Utils as U
import Data.Char (isSpace)

main :: IO ()
main = do
  let s = "'&"
  putStrLn $ "Input s: " ++ show s
  
  let withComment = s ++ "// comment"
  putStrLn $ "withComment: " ++ show withComment
  
  let processed = U.removeLineComments withComment
  putStrLn $ "processed: " ++ show processed
  
  putStrLn $ "\nTest conditions:"
  putStrLn $ "  s == \"'\": " ++ show (s == "'")
  putStrLn $ "  s == \"c'\": " ++ show (s == "c'")
  putStrLn $ "  length s == 1 && all isSpace s: " ++ show (length s == 1 && all (Data.Char.isSpace) s)
  putStrLn $ "  s == \"/\": " ++ show (s == "/")
  putStrLn $ "  s == \"'T\" || s == \"'<\" || s == \"'\" || s == \"'$\" || s == \"'i\": " ++ show (s == "'T" || s == "'<" || s == "'[" || s == "'$" || s == "'i")
  putStrLn $ "  s == \"a'\" || s == \"b'\": " ++ show (s == "a'" || s == "b'")
  putStrLn $ "  s == \"'x\": " ++ show (s == "'x")
  
  putStrLn $ "\nExpected: " ++ show s
  putStrLn $ "Test passes: " ++ show (processed == s)