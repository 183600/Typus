import Data.Char (isSpace)

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

main :: IO ()
main = do
    let line = ")"
    let trimmed = trim line
    putStrLn $ "Original: '" ++ line ++ "'"
    putStrLn $ "Trimmed: '" ++ trimmed ++ "'"
    putStrLn $ "Is equal to \")\": " ++ show (trimmed == ")")