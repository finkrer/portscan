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
import Data.Char
import Data.Bits
import Text.Printf
import qualified Data.ByteString.Char8 as B8

data Protocol = TCP | UDP deriving (Show, Eq)

data ProtoName = NTP 
               | DNS
               | SMTP
               | POP3 
               | IMAP
               | HTTP
               | SSH
               | Unknown
               deriving (Show, Eq)

data Result = Reply ProtoName Port
            | NoReply
            deriving (Show, Eq)

type Port = Int
type ThreadCount = Int

withTimeout :: IO a -> IO (Maybe a)
withTimeout = timeout $ 200 * 1000

scan :: Protocol -> HostName -> Port -> Port -> ThreadCount -> IO ()
scan proto host from to threads = do
    caps <- getNumCapabilities
    let length = to - from + 1
    let cpus = minimum [caps * threads, length, 256]
    let positions = [from,from+cpus..to]
    forConcurrently_ [0..cpus-1] (\thread ->
        forM_ positions (\pos ->
            displayPort $ pos + thread))
            where
                displayPort = check proto host >=> displayResult proto

check :: Protocol -> HostName -> Port -> IO Result
check proto host port = withSocketsDo $ do
    let sockType = case proto of
                    TCP -> Stream
                    UDP -> Datagram
    catch (getAddrInfo Nothing (Just host) (Just $ show port)) handler1 >>=
        \case
            [] -> return NoReply
            serverAddr:_ -> do
                sock <- socket (addrFamily serverAddr) sockType defaultProtocol
                catch (withTimeout $ connect sock (addrAddress serverAddr)) handler2 >>=
                    \case
                        Nothing -> close sock >> return NoReply
                        _       -> do
                                    result <- getResult sock port
                                    close sock
                                    if result /= NoReply || proto == UDP
                                        then return result
                                        else return $ Reply Unknown port
    where
        handler1 :: IOException -> IO [AddrInfo]
        handler1 err = return []
        handler2 :: IOException -> IO (Maybe ())
        handler2 err = return Nothing

packet :: B8.ByteString
packet = B8.pack $ [chr 35]
                ++ (replicate 39 (chr 0))
                ++ (replicate 7 (chr 44))
                ++ [chr 10]

getResult :: Socket -> Port -> IO Result
getResult sock port = do
    send sock packet
    catch (withTimeout $ recv sock 1024) handler >>=
        \case
            Nothing -> return NoReply
            Just bstr -> return $ Reply (getProtocol bstr) port
    where
        handler :: IOException -> IO (Maybe B8.ByteString)
        handler err = return Nothing

getProtocol :: B8.ByteString -> ProtoName
getProtocol reply
    | B8.isInfixOf "HTTP" reply = HTTP
    | B8.isInfixOf "SSH"  reply = SSH
    | B8.isInfixOf "SMTP" reply = SMTP
    | B8.isInfixOf "IMAP" reply = IMAP
    | B8.isInfixOf "POP3" reply = POP3
    | B8.length reply > 3
        && slice 0 2 reply == slice 0 2 packet 
        && charAt 3 reply .&. 1 == 1
        = DNS
    | B8.length reply > 39
        && mode == 4 
        && version == 2 || version == 3 || version == 4 
        && slice 24 32 reply == slice 40 48 packet
        = NTP
    | otherwise = Unknown
    where
        slice x y = B8.take (y-x) . B8.drop x
        charAt n = ord . (flip B8.index) n
        mode = charAt 0 reply .&. 7
        version = charAt 0 reply `shift` (-3) .&. 7

displayResult :: Protocol -> Result -> IO ()
displayResult proto =
    \case
        NoReply -> return ()
        Reply name port  -> putStrLn $ printf "%s %d %s" (show proto) port (display name)
            where display = \case
                    Unknown -> ""
                    n       -> show n
                