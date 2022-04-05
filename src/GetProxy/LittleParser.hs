-- готовую либу не нашел ... . . .. . . .  . . ....... .. . . . . . 
{-# LANGUAGE OverloadedStrings #-} 

module GetProxy.LittleParser (LittleRequest (..), 
                              parseHTTP, 
                              methodIsGet, 
                              parseHost) where 

import Data.List as List
import Data.ByteString.Char8 (unpack)
import Network.Socket (HostName, ServiceName)
import GetProxy.Types


data LittleRequest = LittleRequest { method :: String,
                                     path :: String,
                                     host :: (HostName, Maybe ServiceName)
                                   } deriving (Show, Eq)

parseHTTP :: Request -> LittleRequest
parseHTTP request = LittleRequest { method = currMethod,
                                    path = currPath,
                                    host = (currHost, currPort)
                                  } 
    where tokens = words $ unpack request 
          currMethod = tokens !! 0 
          currPath = tokens !! 1

          hostHeader = case elemIndex "Host:" tokens of
                           Nothing -> ""
                           Just n -> tokens !! (n+1)                      
          
          {- 
          Однажды Эрнест Хемингуэй поспорил, 
          что сможет написать самый короткий рассказ, 
          способный растрогать любого. 
          Он выиграл спор: 
          -}
          currHost = takeWhile (/=':') hostHeader
          currPort = snd <$> (uncons $ dropWhile (/=':') hostHeader)


methodIsGet :: Request -> Bool
methodIsGet request = "GET" == method (parseHTTP request)


parseHost :: Request -> (Maybe HostName, Maybe ServiceName)
parseHost request = 
    case host $ parseHTTP request of 
        ("", _) -> (Nothing, Nothing)
        (hostname, Nothing) -> (Just hostname, Just "80")
        (hostname, Just servicename) -> (Just hostname, Just servicename)