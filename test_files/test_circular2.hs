import Compiler.TypeChecker
import Compiler.GoAst
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
    let source = "package main\nfunc a() { b() }\nfunc b() { a() }\nfunc main() { a() }"
    case parseGoModule (lines source) of
        Left err -> putStrLn $ "Parse error: " ++ err
        Right goModule -> do
            let functionInfos = mapMaybe parseFunctionInfoFromDecl (gmDecls goModule)
            putStrLn $ "Function infos: " ++ show functionInfos
            let callGraph = Map.fromList $ map (\FunctionInfo{..} -> (fiName, map callName (extractCallExpressions fiBody))) functionInfos
            putStrLn $ "Call graph: " ++ show callGraph
            let cycles = findCycles callGraph
            putStrLn $ "Cycles: " ++ show cycles
  where
    findCycles graph = 
        let visited = Set.empty
            recStack = Set.empty
        in dfsAll graph visited recStack []
    
    dfsAll graph visited recStack acc =
        case Map.keys graph \\ Set.toList visited of
            [] -> acc
            (node:_) ->
                let (cycles', visited', recStack') = dfs node visited recStack [] graph
                in dfsAll graph visited' recStack' (acc ++ cycles')
    
    dfs node visited recStack path graph
        | node `Set.member` recStack = 
            let cyclePath = dropWhile (/= node) (reverse path) ++ [node]
            in ([cyclePath], visited, recStack)
        | node `Set.member` visited = ([], visited, recStack)
        | otherwise = 
            let visited' = Set.insert node visited
                recStack' = Set.insert node recStack
                neighbors = Map.findWithDefault [] node graph
                (allCycles, visited'', recStack'') = 
                    foldl (\(cycles, vis, rs) neighbor ->
                        let (newCycles, vis', rs') = dfs neighbor vis rs (node:path) graph
                        in (cycles ++ newCycles, vis', rs')
                    ) ([], visited', recStack') neighbors
                recStackFinal = Set.delete node recStack''
            in (allCycles, visited'', recStackFinal)