import System.IO
import qualified Data.List as L

main :: IO ()
main = do
    content <- readFile "src/Utils.hs"
    let newContent = L.unlines $ map (\line -> if line == "    \"\"a\\\\\"\" -> True"
                                            then "    ('\"' : c : '\\\\' : '\"' : '\"' : _) -> True"
                                            else line) $ lines content
    writeFile "src/Utils.hs" newContent