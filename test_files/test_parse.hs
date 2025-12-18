import Compiler.TypeChecker (parseFunctionInfo, FunctionInfo(..))
import Compiler.GoAst (FuncDecl(..))
import qualified Data.Text as T

main :: IO ()
main = do
    let funcA = FuncDecl ["func a() { b() }"]
    let funcB = FuncDecl ["func b() { a() }"]
    let infoA = parseFunctionInfo funcA
    let infoB = parseFunctionInfo funcB
    putStrLn $ "Function a: " ++ show infoA
    putStrLn $ "Function b: " ++ show infoB