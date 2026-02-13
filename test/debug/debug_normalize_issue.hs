import Utils (normalizeIndentation)
import Data.Char (ord, isSpace)
import Data.List (isInfixOf)

main :: IO ()
main = do
    let lines' = ["\n\ETB"]
    let withMixed = map ("\t  " ++) lines'
    let input = unlines withMixed
    putStrLn $ "Input: " ++ show input
    
    let normalized = normalizeIndentation input
    let normLines = lines normalized
    putStrLn $ "Normalized: " ++ show normalized
    putStrLn $ "Normalized lines: " ++ show normLines
    putStrLn $ "Number of lines: " ++ show (length normLines)
    putStrLn $ "Expected: 2 lines"
    
    -- Test the actual test condition
    let result = if lines' == ["\n\ETB"]
                 then length normLines === 2
                 else property True
    putStrLn $ "Test passes: " ++ show result
  where
    (===) :: Int -> Int -> Bool
    (===) = (==)
    property :: Bool -> Bool
    property = id