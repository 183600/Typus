import Utils (removeLineComments)

main :: IO ()
main = do
    let s = "'\\"
    let withComment = s ++ "// comment"
    let processed = removeLineComments withComment
    
    putStrLn $ "s: " ++ show s
    putStrLn $ "withComment: " ++ show withComment
    putStrLn $ "processed: " ++ show processed
    putStrLn $ "Expected: '// comment"
    putStrLn $ "Test expects: removeLineComments \"'\\\\// comment\" == \"'// comment\""