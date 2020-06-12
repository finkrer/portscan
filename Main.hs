{-# LANGUAGE RecordWildCards #-}

module Main where

import Arguments
import Scanning

main :: IO ()
main = getArgs >>= run

run :: Args -> IO ()
run Args{..} = do
        if scanTCP then scan TCP host from to else return ()
        if scanUDP then scan UDP host from to else return ()
        if not (scanTCP || scanUDP) then putStrLn "No protocols selected" else return ()