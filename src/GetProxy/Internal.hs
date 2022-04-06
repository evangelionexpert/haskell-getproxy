{-# LANGUAGE ScopedTypeVariables #-}

module GetProxy.Internal where

import GetProxy.Types

import Network.Socket
import Network.Socket.ByteString
import Control.Exception


listenAddr :: AddrInfo -> Int -> IO (Maybe Socket) 
listenAddr addr pendingConnections = 
    handle (\(e :: SomeException) -> return Nothing) $ do
        socket <- openSocket addr
        setSocketOption socket ReuseAddr 1 

        bind socket $ addrAddress addr
        listen socket pendingConnections

        return $ Just socket


connectToAddr :: AddrInfo -> IO (Maybe Socket)
connectToAddr addr = 
    handle (\(e :: SomeException) -> return Nothing) $ do
        socket <- openSocket addr
        setSocketOption socket ReuseAddr 1 

        connect socket $ addrAddress addr
        return $ Just socket


getResponseFromSocket :: Socket -> Request -> IO Response
getResponseFromSocket socket request = do 
    send socket request

    let maxResponseSize = 67108864 -- max == 64mb
    response <- recv socket maxResponseSize 

    close socket
    return response
