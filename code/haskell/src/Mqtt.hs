module Mqtt where

import "base" Control.Exception (bracket)
import "aeson" Data.Aeson (ToJSON, encode)
import "base" Data.Maybe (fromMaybe)
import "net-mqtt" Network.MQTT.Client
  ( MQTTClient
  , connectURI
  , mqttConfig
  , normalDisconnect
  , publish
  )
import "network-uri" Network.URI (URI, parseURI)

uri :: URI
uri = fromMaybe (error "bad uri") $ parseURI "mqtt://localhost"

withMqtt :: (MQTTClient -> IO c) -> IO c
withMqtt = bracket (connectURI mqttConfig uri) normalDisconnect

send :: ToJSON a => MQTTClient -> a -> IO ()
send mc a = publish mc "earth" (encode a) False
