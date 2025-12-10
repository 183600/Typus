import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map
import Parser
import Compiler.TypeChecker (buildTypeEnv, extractCallExpressions, TypeEnv(..), functionTypes)
import qualified Compiler.IR as IR

main :: IO ()
main = do
  content <- TIO.readFile "debug_undefined_var.typus"
  case parseTypus (T.unpack content) of
    Left err -> putStrLn $ "Parse error: " ++ err
    Right typusFile -> do
      putStrLn "Parse successful!"
      let goModule = IR.moduleFromTypus typusFile
      case goModule of
        Left err -> putStrLn $ "Failed to build Go module: " ++ show err
        Right goMod -> do
          let typeEnv = buildTypeEnv goMod
          putStrLn $ "Type environment: " ++ show (Map.keys (functionTypes typeEnv))
          
          let calls = extractCallExpressions "println(undefinedVar)"
          putStrLn $ "Extracted calls: " ++ show calls