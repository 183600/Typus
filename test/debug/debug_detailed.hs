import qualified SyntaxValidator as SV
import qualified Data.Text as T
import qualified Text.Megaparsec as MP
import Text.Megaparsec (Parsec, errorBundlePretty)
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)

type MegaParser = Parsec Void String

-- Test tokenization
testTokenization :: String -> IO ()
testTokenization input = do
    putStrLn $ "Testing tokenization of: " ++ input
    let tokens = SV.tokenize input
    putStrLn $ "Tokens: " ++ show tokens
    putStrLn $ "Has import token: " ++ show (any isImport tokens)
    putStrLn $ "Has other Go code: " ++ show (any isOtherGoSpecific tokens)
    where
        isImport (SV.TKeyword "import" _ _) = True
        isImport _ = False
        isOtherGoSpecific (SV.TKeyword k _ _) = k `elem` ["func", "package", "var", "const", "type"]
        isOtherGoSpecific _ = False

main :: IO ()
main = do
    let testInput = "import \"a\""
    testTokenization testInput
    putStrLn $ "Syntax errors: " ++ show (SV.validateSyntax testInput)