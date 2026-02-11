import Utils
import Data.Char (isSpace)

main :: IO ()
main = do
    -- Test case 3: multiline with [""]
    let lines' = [""]
    let withMixed = map ("\t  " ++) lines'
    let input = unlines withMixed
    let normalized = normalizeIndentation input
    
    putStrLn $ "lines' = " ++ show lines'
    putStrLn $ "withMixed = " ++ show withMixed
    putStrLn $ "input = " ++ show input
    putStrLn $ "inputLines = " ++ show (lines input)
    
    let allLinesEmptyOrWhitespace = all (\line -> null line || all isSpace line) (lines input)
    putStrLn $ "allLinesEmptyOrWhitespace = " ++ show allLinesEmptyOrWhitespace
    
    putStrLn $ "normalized = " ++ show normalized
    putStrLn $ "Expected = \"\\t  \\n\""