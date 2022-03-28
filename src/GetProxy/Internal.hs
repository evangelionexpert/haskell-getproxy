module GetProxy.Internal where

import Network.Socket
import Data.ByteString.Char8 (ByteString)
import Network.Socket.ByteString

import Control.Monad.Trans.Maybe
import StmContainers.Map as Map

import Control.Exception

import Control.Monad.STM

type Request  = ByteString 
type Response = ByteString

--раскидать функции логически

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


maybeSend :: Socket -> Maybe ByteString -> IO Int
maybeSend socket maybeMsg = 
    case maybeMsg of
        Just msg -> send socket msg
        Nothing  -> return 0


cacheData :: Map Request Response -> (Maybe Request, Maybe Response) -> IO ()
cacheData cache maybeEntry = do
    case maybeEntry of 
        (Just request, Just response) -> atomically $ insert response request cache
        (_, _)                        -> return ()


listenPort :: PortNumber -> IO Socket
listenPort port = do 
    let pendingConnections = 5
    let hints = Just $ defaultHints {addrFlags = [AI_PASSIVE], 
                                     addrSocketType = Stream}

    addr:_ <- getAddrInfo hints Nothing $ Just $ show $ port
    listenAddr addr pendingConnections


connectToServer :: (Maybe HostName, Maybe ServiceName) -> MaybeT IO Socket
connectToServer host = MaybeT $ do 
    let hints = Just $ defaultHints {addrSocketType = Stream}

    addrInfo <- try $ getAddrInfo hints (fst host) (snd host)
                        :: IO (Either IOException [AddrInfo])

    case addrInfo of 
        Right addr -> connectToAddr (head addr) >>= return . Just
        Left  _    -> return Nothing


acceptConn :: Socket -> IO Socket
acceptConn socket = accept socket >>= return . fst