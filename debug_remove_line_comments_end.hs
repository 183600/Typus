import Utils

main :: IO ()
main = do
    let s = "c'"
    let withComment = s ++ "// comment"
    let processed = Utils.removeLineComments withComment
    putStrLn $ "s = " ++ show s
    putStrLn $ "withComment = " ++ show withComment
    putStrLn $ "processed = " ++ show processed