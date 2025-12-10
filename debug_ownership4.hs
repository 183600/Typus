import System.Process
import System.Exit
import System.Directory (doesFileExist)

main :: IO ()
main = do
    -- Read the ownership.typus file
    content <- readFile "test/fixtures/full_project/ownership.typus"
    putStrLn "=== ownership.typus content ==="
    putStrLn content
    putStrLn ""
    
    -- Try to compile it using stack exec
    (exitCode, stdout, stderr) <- readProcessWithExitCode "stack" ["exec", "typus", "--", "convert", "test/fixtures/full_project/ownership.typus", "-o", "/tmp/ownership.go"] ""
    
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
    exist <- doesFileExist "/tmp/ownership.go"
    if exist
        then do
            goContent <- readFile "/tmp/ownership.go"
            putStrLn "=== Generated Go code ==="
            putStrLn goContent
        else putStrLn "Go file was not generated"