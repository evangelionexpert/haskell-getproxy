-- образцово-показательный говнокод для первичной проверки, 
-- обязательно!!!!!!! переписать или найти готовую либу

{-# LANGUAGE OverloadedStrings #-} 


module GetProxy.LittleParser (LittleRequest (..), parseHTTP) where 

import Data.List as List
import Data.ByteString.Char8 as Str

data LittleRequest = LittleRequest { method :: Maybe String,
                                     path :: Maybe String,
                                     version :: Maybe String,
                                     host :: (Maybe String, Maybe String)
                                   } deriving (Show, Eq)

parseHTTP request = LittleRequest { method = currMethod,
                                    path = currPath,
                                    version = currVersion,
                                    host = currHost
                                  } 
    where tokens = Str.words request
          currMethod = Just $ unpack $ tokens !! 0 
          currPath = Just $ unpack $ tokens !! 1
          currVersion = Just $ unpack $ Str.dropWhile (/= '/') $ tokens !! 2

          hostHeader = case List.elemIndex "Host:" tokens of
                           Nothing -> ""
                           Just n -> tokens !! (n+1)                      
          
          {- 
          Однажды Эрнест Хемингуэй поспорил, 
          что сможет написать самый короткий рассказ, 
          способный растрогать любого. 
          Он выиграл спор: 
          -}
          currHost = fixPort $ (Just (unpack $ Str.takeWhile (/=':') hostHeader), Just (unpack $ Str.dropWhile (/=':') hostHeader))

          fixPort (Just host, Just "") = (Just host, Just "80")
          fixPort (Just host, Just port) = (Just host, Just $ List.tail port)
        



          







