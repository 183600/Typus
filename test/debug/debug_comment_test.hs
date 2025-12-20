import Utils (removeComments)

main :: IO ()
main = do
    let testInput = "'"
    let commentedContent = "/* first */ " ++ testInput ++ " /* second */"
    putStrLn $ "Original content: " ++ show testInput
    putStrLn $ "Commented content: " ++ show commentedContent
    let result = removeComments commentedContent
    putStrLn $ "After removeComments: " ++ show result
    
    -- Check if comment markers are removed
    putStrLn $ "Contains /*: " ++ show ("/*" `isInfixOf` result)
    putStrLn $ "Contains */: " ++ show ("*/" `isInfixOf` result)
    putStrLn $ "Contains original: " ++ show (testInput `isInfixOf` result)

isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
  where
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
    tails [] = [[]]
    tails xs@(x:xs') = xs : tails xs'