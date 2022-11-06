module Mqtt where

import Data.Aeson (ToJSON, encode)
import Data.Maybe (fromMaybe)
import Network.MQTT.Client
  ( connectURI
  , mqttConfig
  , publish
  )
import Network.URI (URI, parseURI)

uri :: URI
uri = fromMaybe (error "bad uri") $ parseURI "mqtt://localhost"

send :: ToJSON a => a -> IO ()
send a = do
  mc <- connectURI mqttConfig uri
  publish mc "earth" (encode a) False
