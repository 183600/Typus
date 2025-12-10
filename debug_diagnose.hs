-- 调试 diagnoseTypeErrorsWithPackage
module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map as Map
import Compiler.TypeChecker
import Compiler.IR
import Parser

main :: IO ()
main = do
    -- 读取文件内容
    utilContent <- TIO.readFile "test/fixtures/full_project/util.typus"
    mainContent <- TIO.readFile "test/fixtures/full_project/main.typus"
    
    -- 转换为TypusFile
    let utilTypusFile = TypusFile [] (T.unpack utilContent)
    let mainTypusFile = TypusFile [] (T.unpack mainContent)
    
    -- 创建包文件列表
    let packageFiles = 
            [ ("test/fixtures/full_project/util.typus", utilTypusFile)
            , ("test/fixtures/full_project/main.typus", mainTypusFile)
            ]
    
    -- 调用 diagnoseTypeErrorsWithPackage
    result <- return $ diagnoseTypeErrorsWithPackage mainTypusFile packageFiles
    
    case result of
        Left errs -> do
            putStrLn "类型检查错误:"
            mapM_ print errs
        Right diagnostics -> do
            putStrLn "类型检查诊断:"
            mapM_ print diagnostics