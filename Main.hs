{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Arguments
import Scanning

main :: IO ()
main = getArgs >>= run

run :: Args -> IO ()
run Args{..} = do
        when scanTCP $ scan TCP host from to threadsPerCPU
        when scanUDP $ scan UDP host from to threadsPerCPU
        unless (scanTCP || scanUDP) $ putStrLn "No protocols selected"