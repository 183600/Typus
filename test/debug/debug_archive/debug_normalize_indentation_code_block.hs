-- Test for normalizeIndentation code block
import Utils (normalizeIndentation)

main :: IO ()
main = do
    let codeBlock = "    if condition {\n        // do something\n        return \n    }"
    putStrLn "Input code block:"
    putStrLn codeBlock
    putStrLn "\nNormalized:"
    putStrLn $ normalizeIndentation codeBlock
    putStrLn "\nLines:"
    mapM_ putStrLn $ lines $ normalizeIndentation codeBlock