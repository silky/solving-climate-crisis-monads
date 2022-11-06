module Main where

import Mqtt (send)

main :: IO ()
main = do
  send (1.0 :: Double)
  send (2.0 :: Double)
