module Config (
    eventListenerPort
  , clientListenerPort
  , totalEvents
  , concurrencyLevel
  , numberOfUsers
  , maxEventSourceBatchSize
  ) where

eventListenerPort, clientListenerPort :: Int
eventListenerPort  = 9090
clientListenerPort = 9099

concurrencyLevel, numberOfUsers :: Int
concurrencyLevel = 10
numberOfUsers    = concurrencyLevel * 10

totalEvents, maxEventSourceBatchSize :: Int
totalEvents             = 500
maxEventSourceBatchSize = 1
