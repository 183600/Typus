import Data.Char (isPrint, ord)

main :: IO ()
main = do
    let c = '\1007127'
    putStrLn $ "Character: " ++ show c
    putStrLn $ "ord: " ++ show (ord c)
    putStrLn $ "isPrint: " ++ show (isPrint c)
    putStrLn $ "Condition (not isPrint && not in \"\\n\\r\\t \" && ord < 128): " ++ 
                show (not (isPrint c) && c `notElem` "\n\r\t " && ord c < 128)