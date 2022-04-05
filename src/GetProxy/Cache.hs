module GetProxy.Cache (Map (..), 
                       cacheMsg, 
                       lookupMsg,
                       newIO) where

import GetProxy.Types

import Control.Monad.STM (atomically)
import StmContainers.Map as Cache


cacheMsg :: Map Request Response -> Request -> Response -> IO ()
cacheMsg cache request response =
    atomically $ Cache.insert response request cache

lookupMsg :: Map Request Response -> Request -> IO (Maybe Response)
lookupMsg cache request = 
    atomically $ Cache.lookup request cache