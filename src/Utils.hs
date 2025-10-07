module Utils
  ( -- Whitespace
    trim
    -- Split
  , splitBy               -- 保留空段（推荐）
  , splitByCollapsed      -- 折叠连续分隔符（兼容旧行为）
  , splitByComma
  , splitByCommaCollapsed
    -- Comments
  , removeLineComments    -- 仅移除 //（忽略字符串/字符字面量）
  , removeComments        -- 移除 // 与 /* ... */（忽略字符串/字符字面量）
    -- Indentation
  , normalizeIndentation  -- 保留相对缩进，去掉公共前缀（推荐）
  , forceSingleTabIndentation -- 旧行为（不推荐）
  , fixIndentation        -- 兼容名 = normalizeIndentation
    -- Search
  , breakOn               -- 更高效的实现
  ) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd, findIndex, isPrefixOf, tails)

-- | 去掉字符串两端的空白字符。
trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

--------------------------------------------------------------------------------
-- Split
--------------------------------------------------------------------------------

-- | 按分隔字符切分，保留空段。
--   例子：
--     splitBy ',' "a,,b"   == ["a", "", "b"]
--     splitBy ',' ",a,"    == ["", "a", ""]
--     splitBy ',' ""       == [""]
splitBy :: Char -> String -> [String]
splitBy _ "" = [""]
splitBy delim s =
  let (chunk, rest) = break (== delim) s
  in case rest of
       []      -> [chunk]
       (_:xs)  -> chunk : splitBy delim xs

-- | 按分隔字符切分，并折叠连续分隔符（丢弃空段）。
--   兼容你现有的旧行为：
--     splitByCollapsed ',' "a,,b" == ["a", "b"]
--     splitByCollapsed ',' ",a,"  == ["a"]
--     splitByCollapsed ',' ""     == []
splitByCollapsed :: Char -> String -> [String]
splitByCollapsed delim = filter (not . null) . splitBy delim

-- | 按逗号切分（保留空段）。
splitByComma :: String -> [String]
splitByComma = splitBy ','

-- | 按逗号切分并折叠连续分隔符（丢弃空段）。
splitByCommaCollapsed :: String -> [String]
splitByCommaCollapsed = splitByCollapsed ','

--------------------------------------------------------------------------------
-- Comments
--------------------------------------------------------------------------------

-- | 仅移除以 // 开始的单行注释，且会正确忽略字符串/字符字面量中的 //。
--   不处理块注释。
removeLineComments :: String -> String
removeLineComments = unlines . map removeFromLine . lines
  where
    removeFromLine :: String -> String
    removeFromLine = goNormal
      where
        goNormal [] = []
        goNormal ('/':'/':_) = [] -- 开始单行注释，丢弃后续
        goNormal ('"':xs)    = '"' : goInString xs
        goNormal ('\'':xs)   = '\'' : goInChar xs
        goNormal (c:cs)      = c : goNormal cs

        goInString []           = [] -- 非严格：未闭合字符串直接结束
        goInString ('\\':x:xs)  = '\\' : x : goInString xs
        goInString ('"':xs)     = '"' : goNormal xs
        goInString (c:cs)       = c : goInString cs

        goInChar []             = []
        goInChar ('\\':x:xs)    = '\\' : x : goInChar xs
        goInChar ('\'':xs)      = '\'' : goNormal xs
        goInChar (c:cs)         = c : goInChar cs

-- | 移除 // 与 /* ... */ 两类注释，忽略字符串/字符字面量中的注释标记。
--   特性与限制：
--   - 支持跨行的块注释；块注释内的换行会保留（以尽量保持行号）。
--   - 不支持嵌套的块注释（与大多数 C 风格语言一致）。
--   - 未闭合的字符串/字符或注释将按“到文件结尾”的方式处理。
removeComments :: String -> String
removeComments = goNormal
  where
    goNormal [] = []
    goNormal ('/':'/':xs) = skipLine xs
    goNormal ('/':'*':xs) = skipBlock xs
    goNormal ('"':xs)     = '"' : goInString xs
    goNormal ('\'':xs)    = '\'' : goInChar xs
    goNormal (c:cs)       = c : goNormal cs

    -- 跳过行注释直到换行，保留换行
    skipLine []         = []
    skipLine ('\n':xs)  = '\n' : goNormal xs
    skipLine (_:xs)     = skipLine xs

    -- 跳过块注释直到 */
    -- 期间遇到换行则保留
    skipBlock []            = []  -- 未闭合块注释
    skipBlock ('*':'/':xs)  = goNormal xs
    skipBlock ('\n':xs)     = '\n' : skipBlock xs
    skipBlock (_:xs)        = skipBlock xs

    -- 字符串字面量（保留内容与转义）
    goInString []           = []  -- 非严格：未闭合字符串
    goInString ('\\':x:xs)  = '\\' : x : goInString xs
    goInString ('"':xs)     = '"' : goNormal xs
    goInString (c:cs)       = c : goInString cs

    -- 字符字面量（保留内容与转义）
    goInChar []             = []  -- 非严格：未闭合字符
    goInChar ('\\':x:xs)    = '\\' : x : goInChar xs
    goInChar ('\'':xs)      = '\'' : goNormal xs
    goInChar (c:cs)         = c : goInChar cs

--------------------------------------------------------------------------------
-- Indentation
--------------------------------------------------------------------------------

-- | 保留相对缩进，仅移除所有非空行的“公共前缀缩进”（空格/Tab 均视为缩进）。
--   这能把整段代码“左移”到合适位置，而不会破坏层级关系。
--   例：
--     "    foo\\n      bar\\n" -> "foo\\n  bar\\n"
normalizeIndentation :: String -> String
normalizeIndentation input =
  let ls = lines input
      nonEmpty = filter (not . all isSpace) ls
      commonPrefix :: Int
      commonPrefix =
        case [length (takeWhile isSpace l) | l <- nonEmpty] of
          [] -> 0
          xs -> minimum xs
      trimLeft n l =
        let leading = takeWhile isSpace l
            dropN   = min n (length leading)
        in drop dropN l
  in unlines (map (trimLeft commonPrefix) ls)

-- | 保留旧行为：将所有非空行强制为“单个制表符 + 去两端空白”的形式。
--   该函数几乎总是破坏性的，不建议使用，仅用于兼容或特殊需求。
forceSingleTabIndentation :: String -> String
forceSingleTabIndentation = unlines . map step . lines
  where
    step line =
      let t = trim line
      in if null t then "" else '\t' : t

-- | 兼容名，等同于 'normalizeIndentation'。
fixIndentation :: String -> String
fixIndentation = normalizeIndentation

--------------------------------------------------------------------------------
-- Search
--------------------------------------------------------------------------------

-- | 在字符串中查找子串并按首次出现处分割。
--   返回：(匹配前缀, 匹配后缀-去掉匹配串)。
--   若未找到，返回：(原串, "")。
--   特殊：当模式串为空时，返回：("", 原串)。
--   例：
--     breakOn "ll" "hello" == ("he", "o")
breakOn :: String -> String -> (String, String)
breakOn "" s  = ("", s)
breakOn pat s =
  case findIndex (isPrefixOf pat) (tails s) of
    Just i  -> (take i s, drop (i + length pat) s)
    Nothing -> (s, "")