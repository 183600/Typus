import qualified Utils as U

main :: IO ()
main = do
  let s = "\""
  let withSlash = "\"" ++ s ++ "// not comment\""
  let processed = U.removeLineComments withSlash
  
  putStrLn $ "Input s: " ++ show s
  putStrLn $ "withSlash: " ++ show withSlash
  putStrLn $ "Processed: " ++ show processed
  putStrLn $ "Is complete string literal: " ++ show (U.isCompleteStringLiteral withSlash)
  putStrLn $ "Expected: \"// not comment\" should be preserved"
  putStrLn $ "Test passes: " ++ show ("// not comment" `isInfixOf` processed)