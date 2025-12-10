import System.Directory
import System.FilePath ((</>))
import CompilerUtils
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.IORef

main :: IO ()
main = do
    -- Create a logger that captures output
    outputRef <- newIORef []
    let logger msg = modifyIORef' outputRef (msg :)
    
    -- Read the ownership.typus file
    content <- readFile "test/fixtures/full_project/ownership.typus"
    putStrLn "=== ownership.typus content ==="
    putStrLn content
    putStrLn ""
    
    -- Create a compiler context with a logger
    ctx <- newCompilerContextWithExecutor logger defaultGoExecutor
    
    -- Try to check the file
    result <- runExceptT $ checkSingleFile ctx "test/fixtures/full_project/ownership.typus"
    case result of
        Left err -> do
            putStrLn "Error:"
            print err
        Right _ -> do
            putStrLn "Check successful"
    
    -- Print captured logs
    logs <- readIORef outputRef
    putStrLn "\n=== Logs ==="
    mapM_ putStrLn (reverse logs)