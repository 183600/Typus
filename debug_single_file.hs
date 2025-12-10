-- 测试单个文件的类型检查
module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Compiler.TypeChecker
import Compiler.IR
import Parser

main :: IO ()
main = do
    -- 读取文件内容
    utilContent <- TIO.readFile "test/fixtures/full_project/util.typus"
    mainContent <- TIO.readFile "test/fixtures/full_project/main.typus"
    
    -- 转换为TypusFile
    let utilTypusFile = TypusFile [] [T.unpack utilContent]
    let mainTypusFile = TypusFile [] [T.unpack mainContent]
    
    -- 测试单个文件的类型检查
    putStrLn "=== 单独检查 main.typus ==="
    result1 <- return $ diagnoseTypeErrors mainTypusFile
    case result1 of
        Left errs -> do
            putStrLn "错误:"
            mapM_ print errs
        Right diagnostics -> do
            putStrLn "诊断:"
            mapM_ print diagnostics
    
    putStrLn "\n=== 单独检查 util.typus ==="
    result2 <- return $ diagnoseTypeErrors utilTypusFile
    case result2 of
        Left errs -> do
            putStrLn "错误:"
            mapM_ print errs
        Right diagnostics -> do
            putStrLn "诊断:"
            mapM_ print diagnostics