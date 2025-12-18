import Compiler.TypeChecker (extractCallExpressions)

main :: IO ()
main = do
    let bodyA = "b()"
    let bodyB = "a()"
    let callsA = extractCallExpressions bodyA
    let callsB = extractCallExpressions bodyB
    putStrLn $ "Calls in a(): " ++ show callsA
    putStrLn $ "Calls in b(): " ++ show callsB
    putStrLn $ "Length of calls in a(): " ++ show (length callsA)
    putStrLn $ "Length of calls in b(): " ++ show (length callsB)