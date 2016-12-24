module Config (
    eventListenerPort
  , clientListenerPort
  , concurrencyLevel
  , timeout
  ) where

eventListenerPort, clientListenerPort :: Int
eventListenerPort  = 9090
clientListenerPort = 9099

concurrencyLevel :: Int
concurrencyLevel = 100

timeout :: Int
timeout = 20000
