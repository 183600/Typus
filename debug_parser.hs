import Parser
import qualified Data.Text as T
import Text.Megaparsec as MP

main :: IO ()
main = do
    let test1 = "key=value"
    putStrLn $ "Test 1: " ++ test1
    print $ MP.runParser fileDirectiveParser "" (T.pack test1)
    
    let test2 = "key with spaces=value"
    putStrLn $ "\nTest 2: " ++ test2
    print $ MP.runParser fileDirectiveParser "" (T.pack test2)
    
    let test3 = "key=value with spaces"
    putStrLn $ "\nTest 3: " ++ test3
    print $ MP.runParser fileDirectiveParser "" (T.pack test3)