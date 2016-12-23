module Config (
    eventListenerPort
  , clientListenerPort
  , totalEvents
  , concurrencyLevel
  , numberOfUsers
  , maxEventSourceBatchSize
  , timeout
  ) where

eventListenerPort, clientListenerPort :: Int
eventListenerPort  = 9090
clientListenerPort = 9099

concurrencyLevel, numberOfUsers :: Int
concurrencyLevel = 100
numberOfUsers    = concurrencyLevel * 10

totalEvents, maxEventSourceBatchSize :: Int
totalEvents             = 10000000
maxEventSourceBatchSize = 1

timeout :: Int
timeout = 20000
