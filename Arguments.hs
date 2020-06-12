module Arguments where

import Options.Applicative
import Data.Semigroup ((<>))
import Network.Socket (HostName)

data Args = Args
  { host    :: HostName 
  , from    :: Int
  , to      :: Int
  , scanTCP :: Bool
  , scanUDP :: Bool }

args :: Parser Args
args = Args
    <$> argument str  (metavar "hostname")
    <*> argument auto (metavar "N1")
    <*> argument auto (metavar "N2")
    <*> switch
        ( short 't'
        <> help "Scan TCP ports" )
    <*> switch
        ( short 'u'
        <> help "Scan UDP ports" )

opts :: ParserInfo Args
opts = info (args <**> helper)
    ( fullDesc
    <> progDesc "Scan for open ports" )

getArgs :: IO Args
getArgs = execParser opts
