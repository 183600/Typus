import Compiler.TypeChecker
import Parser
import Compiler.IR
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    let source = "package main\nfunc main() {\n    println(undefinedVar)\n}"
    let typusFile = TypusFile (T.pack source) "test.typus"
    case IR.moduleFromTypus typusFile of
        Left err -> print err
        Right goModule -> do
            let env = buildTypeEnv goModule
            let errors = gatherTypeErrors env goModule
            print errors