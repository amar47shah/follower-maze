module Config (
    eventListenerPort
  , clientListenerPort
  , concurrencyLevel
  ) where

eventListenerPort, clientListenerPort :: Int
eventListenerPort  = 9090
clientListenerPort = 9099

concurrencyLevel :: Int
concurrencyLevel = 100
