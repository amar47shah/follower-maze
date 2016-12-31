-- | Configuration settings.
--
-- `eventListenerPort` and `clientListenerPort` are used by the application.
--
-- `numberOfUsers` and `totalEvents` are only used in tests.
module Config
  ( eventListenerPort
  , clientListenerPort
  , numberOfUsers
  , totalEvents
  ) where

eventListenerPort, clientListenerPort :: Int
-- | The port used by the event source. Used in `main`.
eventListenerPort  = 9090
-- | The port used to register clients. Used in `main`.
clientListenerPort = 9099

-- | Number of messages to send.
-- Used to generate realistic test input.
totalEvents :: Int
totalEvents = 10000000

-- | Total number of users (connected or not).
-- Used to generate realistic test input.
numberOfUsers :: Int
numberOfUsers = concurrencyLevel * 10

-- | Number of connected users.
concurrencyLevel :: Int
concurrencyLevel = 100
