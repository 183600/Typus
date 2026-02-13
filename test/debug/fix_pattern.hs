main = do
    content <- readFile "src/Utils.hs"
    let newContent = unlines $ map fixLine $ lines content
    writeFile "src/Utils.hs.new" newContent
  where
    fixLine line = if line == "    ('\"':c:'\\':'\"':_) -> False"
                  then "    ('\"':c:'\\':'\"':_) -> False"
                  else if line == "    \"\"a\\\\\"\" -> True"
                  then "    \"\"a\\\\\"\" -> True\n    -- 特殊情况：双引号 + 任意字符 + 反斜杠 + 双引号 + 双引号是完整的字符串字面量\n    ('\"' : _ : '\\\\' : '\"' : '\"' : _) -> True"
                  else line