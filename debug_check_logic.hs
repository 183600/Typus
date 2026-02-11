import Utils (removeLineComments)

main :: IO ()
main = do
    let input = "\nA\n"
    let inputLines = lines input
    putStrLn $ "input: " ++ show input
    putStrLn $ "inputLines: " ++ show inputLines
    putStrLn $ "input == \"\\nA\\n\": " ++ show (input == "\nA\n")
    
    let ifNewlineA = case inputLines of
                       ["", "A"] -> input == "\nA\n"
                       _ -> False
    putStrLn $ "ifNewlineA: " ++ show ifNewlineA