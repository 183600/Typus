import Data.List

main :: IO ()
main = do
  content <- readFile "src/Utils.hs"
  let lines' = lines content
  let startIdx = case findIndex (isPrefixOf "normalizeIndentation :: String -> String") lines' of
                  Nothing -> 0
                  Just idx -> idx
  let endIdx = startIdx + 150
  let functionLines = take (endIdx - startIdx) (drop startIdx lines')
  mapM_ putStrLn functionLines