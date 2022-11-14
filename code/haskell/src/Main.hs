module Main where

-- mosquitto_pub -t 'earth' -m '100'

import "base" Control.Monad (forM_)
import Misc (sleep)
import Mqtt (send, withMqtt)
import "net-mqtt" Network.MQTT.Client (MQTTClient)


sendAll :: MQTTClient -> [Double] -> IO ()
sendAll mc xs
  = forM_  xs $ \i -> do
      sleep 2
      send mc i


main :: IO ()
main = do
  let rs :: [Double]
      rs = [ 0.005 * k | k <- [0 .. (1 / 0.005)] ]
  withMqtt $ \mc -> do sendAll mc rs
