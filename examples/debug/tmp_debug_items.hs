module Main where
import qualified Ownership

code :: String
code = unlines [
    "type Container struct {",
    "    items []int",
    "}",
    "func main() {",
    "    c := Container{items: []int{1, 2, 3}}",
    "    ref := &c.items",
    "    (*ref) = append((*ref), 4)",
    "    fmt.Println(c.items)",
    "}"]

main :: IO ()
main = do
  let (errs, logs) = Ownership.analyzeOwnershipDebug True code
  putStrLn ("Errors: " ++ show errs)
  mapM_ putStrLn logs
