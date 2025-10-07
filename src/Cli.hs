module Cli (Args(..), parseArgs) where

import Options.Applicative

data Args
    = Convert FilePath FilePath
    | Check FilePath
    | Build [String]
    | Run [String]
    | Version
    deriving (Show)

convertOptions :: Parser Args
convertOptions = Convert
    <$> argument str (metavar "INPUT" <> help "Input file or directory")
    <*> strOption (long "output" <> short 'o' <> metavar "OUTPUT" <> help "Output file or directory")

checkOptions :: Parser Args
checkOptions = Check
    <$> argument str (metavar "INPUT" <> help "Input file or directory")

buildOptions :: Parser Args
buildOptions = Build
    <$> many (argument str (metavar "GO_ARGS..."))

runOptions :: Parser Args
runOptions = Run
    <$> many (argument str (metavar "FILE [ARGS...]"))

versionOption :: Parser Args
versionOption = flag' Version
    (long "version" <> short 'v' <> help "Show version information")

argsParser :: Parser Args
argsParser = subparser
    ( command "convert" (info convertOptions (progDesc "Convert Typus files to Go"))
   <> command "check" (info checkOptions (progDesc "Check Typus syntax"))
   <> command "build" (info buildOptions (progDesc "Build a Typus project (calls go build)"))
   <> command "run" (info runOptions (progDesc "Run a Typus project (calls go run)"))
    ) <|> versionOption

parseArgs :: IO Args
parseArgs = execParser opts
  where
    opts = info (argsParser <**> helper)
      ( fullDesc
     <> progDesc "Typus compiler and toolchain"
     <> header "typus - A Go extension with ownership and dependent types" )