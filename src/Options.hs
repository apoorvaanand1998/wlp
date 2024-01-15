module Options where

import Options.Applicative



data Opts = Opts
    { maxDepth :: Int
    , heuristic :: Int
    , invariants :: Bool
    , simply :: Bool
    , showTree :: Bool
    , showWlp :: Bool
    , path :: FilePath
    } deriving (Show)

parser :: Parser Opts
parser =
    Opts
    <$> option auto
        ( long "max-depth"
        <> value 10
        <> short 'K'
        <> help "Maximum depth of program paths" )
    <*> option auto
        ( long "heuristic"
        <> value 100
        <> short 'H'
        <> help "Heuristic level (0 to turn off)" )
    <*> switch
        ( long "invariants"
        <> short 'I'
        <> help "Check invariants" )
    <*> switch
        ( long "simply"
        <> short 'S'
        <> help "Simplify the program before verification" )
    <*> switch
        ( long "show-tree"
        <> short 'T'
        <> help "Print the program tree (only use with tiny max-depth!)" )
    <*> switch
        ( long "show-wlp"
        <> short 'W'
        <> help "Print the WLP (might be very large!)" )
    <*> strArgument
        ( metavar "PATH"
        <> help "The file to verify" )

parser' :: ParserInfo Opts
parser' = info (helper <*> parser) (fullDesc <> header "WLP solver")

getOpts :: IO Opts
getOpts = execParser parser'
