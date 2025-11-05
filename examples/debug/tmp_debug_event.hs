module Main where
import qualified Ownership

code :: String
code = unlines [
  "type Event struct {",
  "    data []int",
  "}",
  "func handleEvent(e Event) {",
  "    fmt.Println(e.data)",
  "}",
  "func main() {",
  "    event := Event{data: []int{1,2,3}}",
  "    handleEvent(event)",
  "    fmt.Println(event.data)",
  "}"]

main :: IO ()
main = do
  let (errs, logs) = Ownership.analyzeOwnershipDebug True code
  print errs
  mapM_ putStrLn logs
