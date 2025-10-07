-- Generate code blocks as source code
generateCodeBlocks :: [CodeBlock] -> String
generateCodeBlocks blocks = 
  intercalate "\n" (map generateCodeBlock blocks)
  where
    generateCodeBlock :: CodeBlock -> String
    generateCodeBlock block = 
      let dirs = cbDirectives block
          content = cbContent block
      in if bdOwnership dirs || bdDependentTypes dirs || bdConstraints dirs
         then "{//! " ++ generateBlockDirectiveLine dirs ++ "}\n" ++ content ++ "\n}"
         else content
    
    generateBlockDirectiveLine :: BlockDirectives -> String
    generateBlockDirectiveLine dirs = 
      let parts = filter (not . null) [
            if bdOwnership dirs then "ownership: on" else "",
            if bdDependentTypes dirs then "dependent_types: on" else "",
            if bdConstraints dirs then "constraints: on" else ""
          ]
      in intercalate ", " parts