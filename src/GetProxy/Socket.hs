module GetProxy.Socket where

import GetProxy.Internal
import GetProxy.Types
import GetProxy.LittleParser

import Network.Socket
import Network.Socket.ByteString

import Control.Exception

import Control.Monad
import Control.Monad.Trans.Maybe


listenPort :: PortNumber -> MaybeT IO Socket
listenPort port = MaybeT $ do 
    let pendingConnections = 5
    let hints = Just $ defaultHints {addrFlags = [AI_PASSIVE], 
                                     addrSocketType = Stream}

    addr:_ <- getAddrInfo hints Nothing $ Just $ show $ port
    listenAddr addr pendingConnections


connectToServer :: Maybe HostName -> Maybe ServiceName -> MaybeT IO Socket
connectToServer host port = MaybeT $ do 
    let hints = Just $ defaultHints {addrSocketType = Stream}
    
    addr <- try $ head <$> getAddrInfo hints host port
                   :: IO (Either IOException AddrInfo)

    liftM join $ mapM connectToAddr (toMaybe addr)


getResponseFromServer :: Request -> MaybeT IO Response
getResponseFromServer request = MaybeT $ do 
    socket <- runMaybeT $ uncurry connectToServer $ parseHost request
    sequence $ getResponseFromSocket <$> socket <*> (Just request)