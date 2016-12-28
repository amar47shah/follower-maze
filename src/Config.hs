-- | Module
module Config (
    eventListenerPort
  , clientListenerPort
  , numberOfUsers
  , totalEvents
  ) where

-- | Exported
eventListenerPort, clientListenerPort :: Int
eventListenerPort  = 9090
clientListenerPort = 9099

-- | Exported
totalEvents :: Int
totalEvents = 10000000

-- | Exported
numberOfUsers :: Int
numberOfUsers = concurrencyLevel * 10

concurrencyLevel :: Int
concurrencyLevel = 100
