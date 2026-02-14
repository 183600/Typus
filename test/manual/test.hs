import Data.Char (isAlpha)

validVar1 = take 5 $ filter isAlpha "a"
validVar2 = take 5 $ filter isAlpha ""

main = do
    print (validVar1, validVar2, null validVar1, null validVar2, null validVar1 && null validVar2, not (null validVar1 && null validVar2))
