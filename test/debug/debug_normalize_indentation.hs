-- Debug script for normalizeIndentation with ["\ng"]
import qualified Utils as U

main :: IO ()
main = do
  let lines' = ["\ng"]
  let withMixed = map ("\t  " ++) lines'
  let normalized = U.normalizeIndentation (unlines withMixed)
  let normLines = lines normalized
  putStrLn $ "lines' = " ++ show lines'
  putStrLn $ "withMixed = " ++ show withMixed
  putStrLn $ "unlines withMixed = " ++ show (unlines withMixed)
  putStrLn $ "normalized = " ++ show normalized
  putStrLn $ "normLines = " ++ show normLines
  putStrLn $ "length lines' = " ++ show (length lines')
  putStrLn $ "length normLines = " ++ show (length normLines)
