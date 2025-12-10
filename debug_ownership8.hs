import System.Process
import System.Exit
import System.Directory (doesFileExist)

main :: IO ()
main = do
    -- Create a simple test file
    writeFile "/tmp/test_ownership.typus" $ unlines
        [ "//! ownership: on"
        , ""
        , "package main"
        , ""
        , "func ownershipDemo() {"
        , "    s1 := \"hello\""
        , "    s2 := s1"
        , "    _ = s2"
        , "    println(s1)"
        , "}"
        ]
    
    -- Try to compile it using stack exec
    (exitCode, stdout, stderr) <- readProcessWithExitCode "stack" ["exec", "typus", "--", "convert", "/tmp/test_ownership.typus", "-o", "/tmp/test_ownership.go"] ""
    
    putStrLn "=== Exit code ==="
    print exitCode
    putStrLn ""
    
    putStrLn "=== Stdout ==="
    putStrLn stdout
    putStrLn ""
    
    putStrLn "=== Stderr ==="
    putStrLn stderr
    putStrLn ""
    
    -- Try to read the generated Go file if it exists
    exist <- doesFileExist "/tmp/test_ownership.go"
    if exist
        then do
            goContent <- readFile "/tmp/test_ownership.go"
            putStrLn "=== Generated Go code ==="
            putStrLn goContent
        else putStrLn "Go file was not generated"