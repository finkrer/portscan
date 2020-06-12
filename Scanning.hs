{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Scanning where

import Prelude hiding (getContents)
import Control.Exception
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (send, recv)
import System.Timeout
import qualified Data.ByteString.Char8 as B8

data Protocol = TCP | UDP deriving Show

data ProtoName = NTP 
               | DNS
               | SMTP
               | POP3 
               | IMAP
               | HTTP
               | SSH
               | Unknown
               deriving Show

data Result = Reply ProtoName
            | NoReply
            deriving Show

withTimeout :: IO a -> IO (Maybe a)
withTimeout = timeout $ 200 * 1000

scan :: Protocol -> HostName -> Int -> Int -> IO ()
scan proto host from to = do
    let length = to - from + 1
    cpus <- getNumCapabilities
    let positions = [from,from+cpus..to]
    forConcurrently_ [1..cpus] (\thread ->
        forM_ positions (\pos ->
            check proto host (pos+thread) >>= displayResult proto (pos+thread)))

check :: Protocol -> HostName -> Int -> IO Result
check proto host port = withSocketsDo $ do
    let sockType = case proto of
                    TCP -> Stream
                    UDP -> Datagram
    addrInfo <- catch (getAddrInfo Nothing (Just host) (Just $ show port)) handler1
    case addrInfo of
        [] -> return NoReply
        serverAddr:_ -> do
            sock <- socket (addrFamily serverAddr) sockType defaultProtocol
            connected <- catch (withTimeout $ connect sock (addrAddress serverAddr)) handler2
            case connected of
                Nothing -> close sock >> return NoReply
                _       -> do
                            result <- getResult sock
                            close sock
                            return result
    where
        handler1 :: IOException -> IO [AddrInfo]
        handler1 err = return []
        handler2 :: IOException -> IO (Maybe ())
        handler2 err = return Nothing

getResult :: Socket -> IO Result
getResult sock = do
    let msg = B8.pack "hi"
    send sock msg
    reply <- withTimeout $ recv sock 1024
    case reply of
        Nothing -> return NoReply
        Just bstr -> return (Reply $ getProtocol bstr)

getProtocol :: B8.ByteString -> ProtoName
getProtocol bstr
    | B8.isInfixOf "HTTP" bstr = HTTP
    | B8.isInfixOf "SSH"  bstr = SSH
    | B8.isInfixOf "SMTP" bstr = SMTP
    | B8.isInfixOf "IMAP" bstr = IMAP
    | B8.isInfixOf "POP3" bstr = POP3
    | otherwise = Unknown

displayResult :: Protocol -> Int -> Result -> IO ()
displayResult proto port result =
    case result of
        NoReply -> return ()
        Reply p -> putStrLn $ show proto ++ " " ++ show port ++ detectedProto
            where detectedProto = case p of
                    Unknown -> ""
                    _       -> " " ++ show p
                