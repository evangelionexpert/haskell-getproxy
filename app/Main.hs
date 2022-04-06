module Main where

import GetProxy

import System.Environment (getArgs)
import Network.Socket (PortNumber)
import Text.Read (readMaybe)
import Control.Monad (join)
import Control.Applicative (liftA)
import Data.Maybe (listToMaybe)

main :: IO ()
main = do 
    let helpMsg = "Usage: getproxy <port [0-65535]>" 
    let succMsg = "Starting proxy..."
    let exitMsg = "Exiting..."

    args <- getArgs

    case join $ liftA readMaybe $ listToMaybe args :: Maybe PortNumber of 
        Just port -> putStrLn succMsg >> startProxy port >> putStrLn exitMsg
        Nothing -> putStrLn helpMsg