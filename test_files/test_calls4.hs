import Compiler.TypeChecker (extractCallExpressions, CallExpr(..))

main :: IO ()
main = do
    let bodyA = "b()"
    let bodyB = "a()"
    let callsA = extractCallExpressions bodyA
    let callsB = extractCallExpressions bodyB
    putStrLn $ "Body A: " ++ show bodyA
    putStrLn $ "Calls in A: " ++ show callsA
    putStrLn $ "Body B: " ++ show bodyB
    putStrLn $ "Calls in B: " ++ show callsB
    
    -- Test with more complex body
    let bodyA2 = "{ b() }"
    let bodyB2 = "{ a() }"
    let callsA2 = extractCallExpressions bodyA2
    let callsB2 = extractCallExpressions bodyB2
    putStrLn $ "Body A2: " ++ show bodyA2
    putStrLn $ "Calls in A2: " ++ show callsA2
    putStrLn $ "Body B2: " ++ show bodyB2
    putStrLn $ "Calls in B2: " ++ show callsB2