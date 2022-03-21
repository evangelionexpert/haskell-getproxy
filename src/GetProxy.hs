module GetProxy (startProxy) where

-- лишние либы????
import GetProxy.LittleParser
import System.IO
import Network.Socket.ByteString
import Network.Socket

import StmContainers.Map as Map
import Control.Concurrent
import Control.Monad.STM

import Data.ByteString.Char8 as Str 

type Request  = ByteString 
type Response = ByteString
-- переделать в дату или не стоит???

listenPort :: PortNumber -> IO (Socket)
listenPort port = do 
    let clientHints = Just $ defaultHints {addrFlags = [AI_PASSIVE], 
                                           addrSocketType = Stream}

    addr:_ <- getAddrInfo clientHints Nothing $ Just $ show $ port
    socket <- openSocket addr
    setSocketOption socket ReuseAddr 1 

    bind socket $ addrAddress addr
    listen socket 0

    (socketConn, _) <- accept socket
    close socket
    
    return socketConn


connectToServer :: (Maybe HostName, Maybe ServiceName) -> IO (Socket)
connectToServer hostName = do 
    let serverHints = Just $ defaultHints {addrSocketType = Stream}

    addr:_ <- getAddrInfo serverHints (fst hostName) (snd hostName)
    socket <- openSocket addr
    setSocketOption socket ReuseAddr 1 

    connect socket $ addrAddress addr

    return socket


getResponseFromServer :: Map Request Response -> Request -> IO (Response)
getResponseFromServer cache request = do 
    socketServer <- connectToServer $ host $ parseHTTP request 
    send socketServer request

    let maxResponseSize = 67108864 -- max == 64mb
    response <- recv socketServer maxResponseSize 

    atomically $ insert response request cache

    close socketServer
    return response

    
getResponse :: Map Request Response -> Request -> IO (Response)
getResponse cache request = do
    cachedResponse <- atomically $ Map.lookup request cache

    case cachedResponse of -- скорее всего убрать кейс оф
        Nothing -> getResponseFromServer cache request >>= return
        Just response -> return response


proxyThread :: Map Request Response -> Socket -> IO ()
proxyThread cache socket = do 
    let maxRequestSize = 1048576
    recv socket maxRequestSize >>= getResponse cache >>= send socket

    close socket


cachedProxy :: Map Request Response -> PortNumber -> IO ()
cachedProxy cache port = do 
    socket <- listenPort port
    forkIO $ proxyThread cache socket -- работает ли многопоток как надо?
    cachedProxy cache port


startProxy :: PortNumber -> IO ()
startProxy port = withSocketsDo $ do 
    cache <- newIO 
    cachedProxy cache port







