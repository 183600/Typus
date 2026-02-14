import qualified SyntaxValidator as SV

main :: IO ()
main = do
    let testInput = "import \"a\""
    putStrLn $ "Testing input: " ++ testInput
    let errors = SV.validateSyntax testInput
    putStrLn $ "Syntax errors: " ++ show errors