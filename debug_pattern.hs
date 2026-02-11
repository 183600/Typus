import Utils (isCompleteStringLiteral)

main :: IO ()
main = do
    let incomplete = "\"b\""
    putStrLn $ "incomplete: " ++ show incomplete
    putStrLn $ "length incomplete: " ++ show (length incomplete)
    putStrLn $ "incomplete pattern match: " ++ show (case incomplete of
                                                         ('"':c:'\\':'"':_) -> "matched pattern"
                                                         _ -> "no match")