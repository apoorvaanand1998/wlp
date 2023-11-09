module Options where

import Options.Applicative

import Heuristics (Heuristic(..))

data Opts = Opts
    { maxDepth :: Int
    , heuristics :: Maybe String
    , simply :: Bool
    , path :: FilePath
    } deriving (Show)

parser :: Parser Opts
parser =
    Opts
    <$> option auto
        ( long "max-depth"
        <> value 50
        <> short 'K'
        <> help "Maximum depth of program paths" )
    <*> optional (option auto
        ( long "heuristics"
        <> short 'H'
        <> help "Heuristics to use" ))
    <*> switch
        ( long "simply"
        <> short 'S'
        <> help "Simplify the program before verification" )
    <*> strArgument
        ( metavar "PATH"
        <> help "The file to verify" )

parser' :: ParserInfo Opts
parser' = info (helper <*> parser) (fullDesc <> header "WLP solver")

getOpts :: IO Opts
getOpts = execParser parser'
