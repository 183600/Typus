import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Parser
import Compiler

main :: IO ()
main = do
  content <- TIO.readFile "debug_error_types.typus"
  case parseTypus (T.unpack content) of
    Left err -> putStrLn $ "Parse error: " ++ err
    Right typusFile -> do
      putStrLn "Parse successful!"
      case compile typusFile of
        Left errs -> do
          putStrLn "Compilation failed:"
          putStrLn $ renderCompilationError errs
        Right goCode -> do
          putStrLn "Compilation successful!"
          putStrLn goCode