import Data.List (isInfixOf)

main :: IO ()
main = do
    let message = "expected type int, got string"
    putStrLn $ "Message: " ++ message
    putStrLn $ "Matches expected: " ++ show ("expected" `isInfixOf` message)
    putStrLn $ "Matches got: " ++ show ("got" `isInfixOf` message)