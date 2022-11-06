module Mqtt where

import Data.Aeson (ToJSON, encode)
import Data.Maybe (fromMaybe)
import Network.MQTT.Client
  ( connectURI
  , mqttConfig
  , publish
  , normalDisconnect
  , MQTTClient
  )
import Control.Exception (bracket)
import Network.URI (URI, parseURI)

uri :: URI
uri = fromMaybe (error "bad uri") $ parseURI "mqtt://localhost"

withMqtt :: (MQTTClient -> IO c) -> IO c
withMqtt = bracket (connectURI mqttConfig uri) normalDisconnect

send :: ToJSON a => MQTTClient -> a -> IO ()
send mc a = publish mc "earth" (encode a) False
