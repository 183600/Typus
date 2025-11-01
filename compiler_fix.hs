-- Generate code blocks as source code
import Data.List (intercalate)
import Parser (CodeBlock(..), BlockDirectives(..))
import SourceLocation (Located, locatedValue)

flagEnabled :: Maybe (Located Bool) -> Bool
flagEnabled = maybe False locatedValue

generateCodeBlocks :: [CodeBlock] -> String
generateCodeBlocks blocks =
  intercalate "\n" (map generateCodeBlock blocks)
  where
    generateCodeBlock :: CodeBlock -> String
    generateCodeBlock block =
      let dirs = cbDirectives block
          content = cbContent block
          ownership = flagEnabled (bdOwnership dirs)
          dependent = flagEnabled (bdDependentTypes dirs)
          constraints = flagEnabled (bdConstraints dirs)
      in if ownership || dependent || constraints
         then "{//! " ++ generateBlockDirectiveLine dirs ++ "}\n" ++ content ++ "\n}"
         else content
    
    generateBlockDirectiveLine :: BlockDirectives -> String
    generateBlockDirectiveLine dirs =
      let ownership = flagEnabled (bdOwnership dirs)
          dependent = flagEnabled (bdDependentTypes dirs)
          constraints = flagEnabled (bdConstraints dirs)
          parts = filter (not . null)
            [ if ownership then "ownership: on" else ""
            , if dependent then "dependent_types: on" else ""
            , if constraints then "constraints: on" else ""
            ]
      in intercalate ", " parts
