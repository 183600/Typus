module Cli (Args(..), parseArgs, parseArgsFromList) where

import Options.Applicative
import System.Environment (getArgs, withArgs)

data Args
    = Convert FilePath FilePath
    | Check FilePath
    | Build Bool [String]
    | Run Bool [String]
    | DebugMode [String]
    | Version
    deriving (Eq, Show)

convertOptions :: Parser Args
convertOptions = Convert
    <$> argument str (metavar "INPUT" <> help "Input file or directory")
    <*> strOption (long "output" <> short 'o' <> metavar "OUTPUT" <> help "Output file or directory")

checkOptions :: Parser Args
checkOptions = Check
    <$> argument str (metavar "INPUT" <> help "Input file or directory")

strictEmbedSwitch :: Parser Bool
strictEmbedSwitch = switch
    ( long "strict-embed"
   <> help "Fail when embedded files referenced via //go:embed cannot be located" )

buildOptions :: Parser Args
buildOptions = Build
    <$> strictEmbedSwitch
    <*> many (argument str (metavar "GO_ARGS..."))

runOptions :: Parser Args
runOptions = Run
    <$> strictEmbedSwitch
    <*> many (argument str (metavar "FILE [ARGS...]"))

debugOptions :: Parser Args
debugOptions = DebugMode
    <$> many (argument str (metavar "DEBUG_ARGS..."))

versionOption :: Parser Args
versionOption = flag' Version
    (long "version" <> short 'v' <> help "Show version information")

argsParser :: Parser Args
argsParser = subparser
    ( command "convert" (info convertOptions (progDesc "Convert Typus files to Go"))
   <> command "check" (info checkOptions (progDesc "Check Typus syntax"))
   <> command "build" (info buildOptions (progDesc "Build a Typus project (calls go build)"))
   <> command "run" (info runOptions (progDesc "Run a Typus project (calls go run)"))
   <> command "debug" (info debugOptions (progDesc "Enter debug mode"))
    ) <|> versionOption

parseArgs :: IO Args
parseArgs = getArgs >>= parseArgsFromList

parseArgsFromList :: [String] -> IO Args
parseArgsFromList rawArgs =
  withArgs (normalizeArgs rawArgs) (execParser parserInfo)

parserInfo :: ParserInfo Args
parserInfo = info (argsParser <**> helper)
  ( fullDesc
 <> progDesc "Typus compiler and toolchain"
 <> header "typus - A Go extension with ownership and dependent types" )

normalizeArgs :: [String] -> [String]
normalizeArgs args =
  case args of
    ("build":rest) -> "build" : adjustCommandArgs rest
    ("run":rest)   -> "run" : adjustCommandArgs rest
    xs             -> xs

adjustCommandArgs :: [String] -> [String]
adjustCommandArgs args =
  let (strictFlags, remainder) = consumeStrictEmbed args
  in strictFlags ++ addSentinel remainder

consumeStrictEmbed :: [String] -> ([String], [String])
consumeStrictEmbed = go [] [] False
  where
    go strictFlags acc _ [] = (reverse strictFlags, reverse acc)
    go strictFlags acc seen (arg:rest)
      | arg == "--" = go strictFlags (arg : acc) True rest
      | arg == "--strict-embed" && not seen = go ("--strict-embed" : strictFlags) acc seen rest
      | otherwise = go strictFlags (arg : acc) seen rest

addSentinel :: [String] -> [String]
addSentinel [] = []
addSentinel xs@("--":_) = xs
addSentinel xs = "--" : xs
