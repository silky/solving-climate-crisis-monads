module Main where

import "warp" Network.Wai.Handler.Warp (run)
import Server (app)

main :: IO ()
main = run 8080 $ app
