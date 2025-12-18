import Utils
import Data.List (isInfixOf)

main :: IO ()
main = do
  let content = ""
      safeContent = if null content then "a" else content
      nestedContent = "/* outer " ++ safeContent ++ " /* inner " ++ safeContent ++ " */ " ++ safeContent ++ " */"
      processed = removeComments nestedContent
  putStrLn $ "nestedContent: " ++ show nestedContent
  putStrLn $ "processed: " ++ show processed
  putStrLn $ "not (\"/*\" `isInfixOf` processed): " ++ show (not ("/*" `isInfixOf` processed))
  putStrLn $ "not (\"*/\" `isInfixOf` processed): " ++ show (not ("*/" `isInfixOf` processed))
