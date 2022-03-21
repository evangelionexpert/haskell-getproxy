module Main where

import GetProxy
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Network.Socket (PortNumber)

main :: IO ()
main = do 
    -- тоже переделать, но потом
    let helpMsg = "Usage: getproxy <port>" 

    args <- getArgs

    case readMaybe $ head (args ++ [""]) :: Maybe PortNumber of 
        Just port -> startProxy port 
        Nothing -> putStrLn helpMsg

        

