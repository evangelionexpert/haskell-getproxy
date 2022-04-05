module GetProxy (startProxy) where

import GetProxy.Socket
import GetProxy.Cache
import GetProxy.Types

import Network.Socket
import Network.Socket.ByteString

import Control.Concurrent (forkIO)
import Control.Monad
import Control.Monad.Trans.Maybe


getResponse :: Map Request Response -> Request -> MaybeT IO Response
getResponse cache request = MaybeT $ do
    cachedResponse <- lookupMsg cache request
    
    if cachedResponse == Nothing then do
        response <- runMaybeT $ getResponseFromServer request
        sequence_ $ cacheMsg <$> (Just cache) <*> (Just request) <*> response
        return response 
    else 
        return cachedResponse


proxyThread :: Map Request Response -> Socket -> IO ()
proxyThread cache socket = do 
    let maxRequestSize = 1048576
    recv socket maxRequestSize >>= runMaybeT . getResponse cache >>= mapM (send socket)

    close socket


cachedProxy :: Map Request Response -> Socket -> IO ()
cachedProxy cache socket = forever $
    liftM fst (accept socket) >>= forkIO . proxyThread cache


startProxy :: PortNumber -> IO ()
startProxy port = withSocketsDo $ do 
    cache <- newIO 
    listenPort port >>= cachedProxy cache
