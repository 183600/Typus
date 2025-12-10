import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map
import Parser
import Compiler.TypeChecker
import qualified Compiler.IR as IR

main :: IO ()
main = do
  content <- TIO.readFile "debug_compile_test.typus"
  case parseTypus (T.unpack content) of
    Left err -> putStrLn $ "Parse error: " ++ err
    Right typusFile -> do
      putStrLn "Parse successful!"
      let goModule = IR.moduleFromTypus typusFile
      case goModule of
        Left err -> putStrLn $ "Failed to build Go module: " ++ show err
        Right goMod -> do
          let typeEnv = buildTypeEnv goMod
          putStrLn $ "All functions in type environment: " ++ show (Map.toList (functionTypes typeEnv))
          putStrLn $ "Looking for println: " ++ show (Map.lookup "println" (functionTypes typeEnv))
          
          case diagnoseTypeErrors typusFile of
            Left errs -> putStrLn $ "Type errors: " ++ show errs
            Right [] -> putStrLn "No type errors!"
            Right diagnostics -> putStrLn $ "Diagnostics: " ++ show diagnostics