module Misc where

import "base" Control.Concurrent (threadDelay)

sleep :: Int -> IO ()
sleep n = threadDelay (10_000 * n)
