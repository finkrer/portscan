module Arguments where

import Options.Applicative
import Data.Semigroup ((<>))
import Network.Socket (HostName)

data Args = Args
  { host    :: HostName 
  , from    :: Int
  , to      :: Int
  , scanTCP :: Bool
  , scanUDP :: Bool
  , threadsPerCPU :: Int }

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
    <*> option auto
        ( long "threads"
        <> metavar "N"
        <> help "Threads per CPU. High values may cause your machine to halt and catch fire."
        <> value 32
        <> showDefault )

opts :: ParserInfo Args
opts = info (args <**> helper)
    ( fullDesc
    <> progDesc "Scan for open ports" )

getArgs :: IO Args
getArgs = execParser opts
