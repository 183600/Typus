import Compiler.TypeChecker (extractCallExpressions)
import qualified Data.Text as T

main :: IO ()
main = do
    let bodyA = "b()"
    let bodyB = "a()"
    let callsA = extractCallExpressions bodyA
    let callsB = extractCallExpressions bodyB
    putStrLn $ "Calls in a(): " ++ show callsA
    putStrLn $ "Calls in b(): " ++ show callsB