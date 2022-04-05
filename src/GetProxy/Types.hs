module GetProxy.Types (ByteString, Request, Response) where

import Data.ByteString.Char8 (ByteString)

type Request  = ByteString 
type Response = ByteString
