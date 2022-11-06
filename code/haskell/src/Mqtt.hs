module Mqtt where

import Control.Exception (bracket)
import Data.Aeson (ToJSON, encode)
import Data.Maybe (fromMaybe)
import Network.MQTT.Client
  ( MQTTClient
  , connectURI
  , mqttConfig
  , normalDisconnect
  , publish
  )
import Network.URI (URI, parseURI)

uri :: URI
uri = fromMaybe (error "bad uri") $ parseURI "mqtt://localhost"

withMqtt :: (MQTTClient -> IO c) -> IO c
withMqtt = bracket (connectURI mqttConfig uri) normalDisconnect

send :: ToJSON a => MQTTClient -> a -> IO ()
send mc a = publish mc "earth" (encode a) False
