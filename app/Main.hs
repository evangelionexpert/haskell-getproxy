module Main where

import GetProxy
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Network.Socket (PortNumber)
import Control.Monad

main :: IO ()
main = do 
    let helpMsg = "Usage: getproxy <port>" 
    let succMsg = "Starting proxy..."

    startProxy 33333

        
 
