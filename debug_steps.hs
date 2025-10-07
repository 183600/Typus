-- Simple debug program to test transformation steps
import Data.List (isInfixOf)

-- Mock transformation functions (simplified versions)
fixVariableDeclarations :: String -> String
fixVariableDeclarations line = line

fixArrayDeclarations :: String -> String
fixArrayDeclarations line = line

fixTypeDefinitions :: String -> String
fixTypeDefinitions line = line

fixFunctionSignatures :: String -> String
fixFunctionSignatures line =
  if "func " `isInfixOf` line && " where " `isInfixOf` line
    then line ++ " {"
    else if "func (" `isInfixOf` line && "[" `isInfixOf` line && "]" `isInfixOf` line
      then "func (" ++ drop 5 (dropWhile (/= '(') line)  -- This might be the issue!
      else line

fixMethodSignatures :: String -> String
fixMethodSignatures line =
  if "func (" `isInfixOf` line && " where " `isInfixOf` line
    then line ++ " {"
    else line

fixInterfaceDefinitions :: String -> String
fixInterfaceDefinitions line = line

fixTypeAliasBlocks :: String -> String
fixTypeAliasBlocks line = line

fixGenericSyntax :: String -> String
fixGenericSyntax line = line

-- Main transformation function
transformLine :: String -> String
transformLine line =
  let
    -- Apply transformations in order
    step1 = fixVariableDeclarations line
    step2 = fixArrayDeclarations step1
    step3 = fixTypeDefinitions step2
    step4 = fixFunctionSignatures step3
    step5 = fixMethodSignatures step4
    step6 = fixInterfaceDefinitions step5
    step7 = fixTypeAliasBlocks step6
    step8 = fixGenericSyntax step7
  in
    step8

main :: IO ()
main = do
  let testLine = "func (f *File) Read(buf []byte) (int, error) {"
  putStrLn $ "Original: " ++ testLine
  let step1 = fixVariableDeclarations testLine
  putStrLn $ "Step1: " ++ step1
  let step2 = fixArrayDeclarations step1
  putStrLn $ "Step2: " ++ step2
  let step3 = fixTypeDefinitions step2
  putStrLn $ "Step3: " ++ step3
  let step4 = fixFunctionSignatures step3
  putStrLn $ "Step4: " ++ step4
  let step5 = fixMethodSignatures step4
  putStrLn $ "Step5: " ++ step5
  let step6 = fixInterfaceDefinitions step5
  putStrLn $ "Step6: " ++ step6
  let step7 = fixTypeAliasBlocks step6
  putStrLn $ "Step7: " ++ step7
  let step8 = fixGenericSyntax step7
  putStrLn $ "Step8: " ++ step8
  putStrLn $ "Final: " ++ transformLine testLine