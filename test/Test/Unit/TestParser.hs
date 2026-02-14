import Parser; import System.Environment; main = getArgs >>= \args -> case args of [] -> print $ parseTypus "type Invalid = int where { invalid }"; [arg] -> print $ parseTypus arg
