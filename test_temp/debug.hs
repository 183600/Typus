import AnalyzerIntegration

sampleCode :: String
sampleCode = unlines
  [ "//! ownership: on"
  , "//! dependent_types: on"
  , "package main"
  , ""
  , "func transfer(x owned String) owned String {"
  , "    return x"
  , "}"
  , ""
  , "func main() {"
  , "    var s owned String = \"hello\""
  , "    var t owned String = transfer(s)"
  , "    println(t)"
  , "}"
  ]

main :: IO ()
main = do
  let state = newIntegratedAnalyzer True True
  result <- runIntegratedAnalysis sampleCode state
  print result
