module GetProxy (startProxy) where

-- лишние либы????
import GetProxy.LittleParser
import GetProxy.Internal

import System.IO
import Network.Socket.ByteString
import Network.Socket

import StmContainers.Map as Map
import Control.Concurrent
import Control.Monad.STM
import Control.Monad

import Data.Either

import Control.Exception
import Control.Monad.Trans.Maybe


getResponseFromServer :: Request -> MaybeT IO Response
getResponseFromServer request = MaybeT $ do 
    maybeSocket <- runMaybeT $ connectToServer $ host $ parseHTTP request 

    case maybeSocket of -- закинуть в интернал
        Just socket -> getResponseFromSocket socket request >>= return . Just
        Nothing     -> return Nothing


getResponse :: Map Request Response -> Request -> MaybeT IO Response
getResponse cache request = MaybeT $ do
    cachedResponse <- atomically $ Map.lookup request cache

    case cachedResponse of -- это ужас потом переписать
        Nothing -> do
            response <- runMaybeT $ getResponseFromServer request
            cacheData cache (Just request, response)
            return response
        Just response -> 
            return $ Just response


proxyThread :: Map Request Response -> Socket -> IO ()
proxyThread cache socket = do 
    let maxRequestSize = 1048576
    recv socket maxRequestSize >>= runMaybeT . getResponse cache >>= maybeSend socket

    close socket


cachedProxy :: Map Request Response -> Socket -> IO ()
cachedProxy cache socket = forever $
    acceptConn socket >>= forkIO . proxyThread cache


startProxy :: PortNumber -> IO ()
startProxy port = withSocketsDo $ do 
    cache <- newIO 
    listenPort port >>= cachedProxy cache







