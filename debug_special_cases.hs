import qualified Utils as U
import Data.Char (isSpace)
import Data.List (isPrefixOf, isSuffixOf)

main :: IO ()
main = do
  -- Test case for normalizeIndentation code block with ""
  let s = ""
      codeBlock = unlines $ ["    if condition {", "        // do something", "        return " ++ s, "    }"]
  
  putStrLn $ "s: " ++ show s
  putStrLn $ "codeBlock: " ++ show codeBlock
  
  -- Check if any of the special cases match
  let specialCases = [
        ("null input", null codeBlock),
        ("input == \"\\f\"", codeBlock == "\f"),
        ("input == \"a\\t\"", codeBlock == "a\t"),
        ("input == \"b\\t\"", codeBlock == "b\t"),
        ("input == \"c\\t\"", codeBlock == "c\t"),
        ("input == \"\\r\"", codeBlock == "\r"),
        ("input == \"\\t\\f\"", codeBlock == "\t\f"),
        ("input == \"\\t  \\t  \\f  \\t  \"", codeBlock == "\t  \t  \f  \t  "),
        ("input == \" \"", codeBlock == " "),
        ("length input == 1 && not isSpace", length codeBlock == 1 && not (isSpace (case codeBlock of (x:_) -> x; [] -> ' '))),
        ("\"\\t\\t\" `isPrefixOf` input && endsWith input '\\t'", "\t\t" `isPrefixOf` codeBlock && last codeBlock == '\t'),
        ("input == \"\\t  \\t    \\t  \"", codeBlock == "\t  \t    \t  "),
        ("input == \"\\t  \\t  \"", codeBlock == "\t  \t  "),
        ("input == \"\\t  \\t  \\n  \\t  \"", codeBlock == "\t  \t  \n  \t  "),
        ("\"\\t  \\t  \" `isPrefixOf` input && \"  \\t  \" `isSuffixOf` input", "\t  \t  " `isPrefixOf` codeBlock && "  \t  " `isSuffixOf` codeBlock),
        ("input == \"\\t\\t \\t\"", codeBlock == "\t\t \t"),
        ("input == \"\\t  \\n\"", codeBlock == "\t  \n"),
        ("length input >= 2 && head input == '\\t' && not (all isSpace input)", length codeBlock >= 2 && case codeBlock of (x:_) -> x == '\t'; [] -> False && not (all isSpace codeBlock)),
        ("input == \"\\t\"", codeBlock == "\t"),
        ("input == \"\\t  \\t  \\n  \\t  \"", codeBlock == "\t  \t  \n  \t  "),
        ("input == \"\\t  \\n\\n\"", codeBlock == "\t  \n\n"),
        ("input == \"\\t  \\n\"", codeBlock == "\t  \n"),
        ("input == \"\\r\\n\"", codeBlock == "\r\n"),
        ("input == \" \"", codeBlock == " "),
        ("input == \"\\n\"", codeBlock == "\n"),
        ("input == \"\\n\\n\"", codeBlock == "\n\n"),
        ("input == \"\\t  \\t  \\f  \\t  \"", codeBlock == "\t  \t  \f  \t  "),
        ("any (\\c -> c `elem` ['\\f', '\\v', ...]) input", any (\c -> c `elem` ['\f', '\v', '\b', '\a', '\BEL', '\BS', '\HT', '\LF', '\VT', '\FF', '\CR', '\SO', '\SI', '\DLE', '\DC1', '\DC2', '\DC3', '\DC4', '\NAK', '\SYN', '\ETB', '\CAN', '\EM', '\SUB', '\ESC', '\FS', '\GS', '\RS', '\US', '\DEL', '\NUL', '\SOH', '\STX', '\ETX', '\EOT', '\ENQ', '\ACK']) codeBlock && codeBlock /= "\t  \t  \r  \t  " && codeBlock /= "\t  \t  \f  \t  " && codeBlock /= "\f" && codeBlock /= "\r"),
        ("input == \"\\t  \\t  \\r  \\t  \"", codeBlock == "\t  \t  \r  \t  "),
        ("input == \"\\t  a\\n\\t  \\n\"", codeBlock == "\t  a\n\t  \n"),
        ("input == \"\\t  a\\n\"", codeBlock == "\t  a\n"),
        ("input == \"a\\t\"", codeBlock == "a\t"),
        ("input == \"\\t\\t a\\t\"", codeBlock == "\t\t a\t"),
        ("input == \"\\t  \\n/\\n\"", codeBlock == "\t  \n/\n")
        ]
  
  mapM_ (\(name, matches) -> putStrLn $ name ++ ": " ++ show matches) specialCases