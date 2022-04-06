module Main where

import GetProxy

import System.Environment (getArgs)
import Text.Read (readMaybe)
import Network.Socket (PortNumber)

main :: IO ()
main = do 
    let helpMsg = "Usage: getproxy <port [0-65535]>" 
    let succMsg = "Starting proxy..."
    let exitMsg = "Exiting..."

    arg <- head <$> getArgs

    case readMaybe arg :: Maybe PortNumber of 
        Just port -> putStrLn succMsg >> startProxy port >> putStrLn exitMsg
        Nothing -> putStrLn helpMsg