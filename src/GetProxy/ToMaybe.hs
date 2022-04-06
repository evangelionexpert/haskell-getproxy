module GetProxy.ToMaybe where

toMaybe :: Either e a -> Maybe a 
toMaybe = either (\e -> Nothing) (\a -> Just a)