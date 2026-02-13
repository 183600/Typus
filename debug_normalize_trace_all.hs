import Utils
import Data.Char

main :: IO ()
main = do
    let input = "\t  \n\n"
    putStrLn $ "Input: " ++ show input
    
    -- 手动检查条件
    let conditions = 
          [ ("input == \"\\t  \\n\\n\"", input == "\t  \n\n")
          , ("length input >= 2", length input >= 2)
          , ("head input == '\\t'", head input == '\t')
          , ("not (all isSpace input)", not (all isSpace input))
          , ("all isSpace input", all isSpace input)
          ]
    
    putStrLn "\nCondition checks:"
    mapM_ (\(desc, result) -> putStrLn $ desc ++ ": " ++ show result) conditions
    
    -- 检查实际结果
    let result = normalizeIndentation input
    putStrLn $ "\nActual result: " ++ show result
    putStrLn $ "Expected: \"\\n\""