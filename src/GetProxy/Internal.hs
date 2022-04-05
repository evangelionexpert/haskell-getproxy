module GetProxy.Internal where

import GetProxy.Types
import Network.Socket
import Network.Socket.ByteString

listenAddr :: AddrInfo -> Int -> IO Socket 
listenAddr addr pendingConnections = do
    socket <- openSocket addr
    setSocketOption socket ReuseAddr 1 

    bind socket $ addrAddress addr
    listen socket pendingConnections

    return socket


connectToAddr :: AddrInfo -> IO Socket 
connectToAddr addr = do
    socket <- openSocket addr
    setSocketOption socket ReuseAddr 1 

    connect socket $ addrAddress addr
    return socket


getResponseFromSocket :: Socket -> Request -> IO Response
getResponseFromSocket socket request = do 
    send socket request

    let maxResponseSize = 67108864 -- max == 64mb
    response <- recv socket maxResponseSize 

    close socket
    return response


toMaybe :: Either e a -> Maybe a 
toMaybe = either (\e -> Nothing) (\a -> Just a)
