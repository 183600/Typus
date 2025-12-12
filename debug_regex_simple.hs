import Data.List (isInfixOf)

main :: IO ()
main = do
    putStrLn "Running regex tests..."
    let message = "expected int, got string"
    putStrLn $ "Message: " ++ message
    putStrLn $ "Matches expected.*int: " ++ show ("expected.*int" `isInfixOf` message)
    putStrLn $ "Matches got.*string: " ++ show ("got.*string" `isInfixOf` message)