import Data.Char (isSpace)

-- Utility function to trim whitespace
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

main :: IO ()
main = do
    let testLine = "\t\"runtime\""
    let trimmed = trim testLine
    putStrLn $ "Original: " ++ show testLine
    putStrLn $ "Trimmed: " ++ show trimmed
    putStrLn $ "Is \"runtime\": " ++ show (trimmed == "\"runtime\"")
    putStrLn $ "Is runtime: " ++ show (trimmed == "runtime")