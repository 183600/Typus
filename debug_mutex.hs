import Ownership

main :: IO ()
main = do
  let code = unlines
        [ "import \"sync\""
        , "var mu sync.Mutex"
        , "var shared []int"
        , "func main() {"
        , "    mu.Lock()"
        , "    shared = []int{1, 2, 3}"
        , "    mu.Unlock()"
        , "    fmt.Println(shared)"
        , "}"
        ]
  let errors = analyzeOwnership code
  putStrLn $ "Errors found: " ++ show errors
  
  -- Also debug with debug mode
  let (debugErrors, debugLog) = analyzeOwnershipDebug True code
  putStrLn $ "Debug errors: " ++ show debugErrors
  putStrLn "Debug log:"
  mapM_ putStrLn debugLog