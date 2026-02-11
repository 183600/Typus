import Utils (normalizeIndentation)
import Data.List (isInfixOf)

main :: IO ()
main = do
    let withEmpty = "\n\n"
    putStrLn $ "withEmpty: " ++ show withEmpty
    let normalized = normalizeIndentation withEmpty
    putStrLn $ "normalized: " ++ show normalized
    putStrLn $ "contains \\n\\n: " ++ show ("\n\n" `isInfixOf` normalized)