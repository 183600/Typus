import qualified Data.Text as T
import qualified Text.Megaparsec as MP
import Text.Megaparsec (Parsec, errorBundlePretty)
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)

type MegaParser = Parsec Void String

-- Simple test to check if our parser handles basic imports
parseImport :: MegaParser ()
parseImport = do
    MC.string "import"
    MC.space
    MC.char '"'
    MP.some (MP.satisfy (\c -> c /= '"'))
    MC.char '"'
    pure ()

main :: IO ()
main = do
    let testInput = "import \"a\""
    putStrLn $ "Testing input: " ++ testInput
    case MP.runParser parseImport "<test>" testInput of
        Right _ -> putStrLn "Success: Basic import parsing works"
        Left err -> putStrLn $ "Error: " ++ errorBundlePretty err