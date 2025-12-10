import Utils
import Data.List (isPrefixOf)

main :: IO ()
main = do
    let line = "\tpackageVar1 int    = 100"
    putStrLn $ "Original: " ++ show line
    putStrLn $ "Trimmed: " ++ show (trim line)
    putStrLn $ "Is package: " ++ show ("package" `isPrefixOf` trim line)