module Server where

import "base" Control.Exception (SomeException, handle)
import "base" Control.Monad.IO.Class (liftIO)
import "base" GHC.Generics (Generic)
import "wai" Network.Wai (Application)
import "websockets" Network.WebSockets qualified as WS
import "websockets" Network.WebSockets.Connection
  ( PendingConnection
  )
import "servant-server" Servant
  ( Context (EmptyContext)
  , Handler
  )
import "servant" Servant.API (type (:>))
import "servant" Servant.API.Generic ((:-))
import "servant-websockets" Servant.API.WebSocket
import "servant-server" Servant.Server.Generic
  ( AsServerT
  , genericServeTWithContext
  )
import "aeson" Data.Aeson (decode, encode)

data Routes mode = Routes
  { channel :: mode :- "channel" :> WebSocketPending
  }
  deriving stock Generic


server :: Routes (AsServerT Handler)
server = Routes
  { channel = runChannel
  }


runChannel :: PendingConnection -> Handler ()
runChannel pending = liftIO $ do
  c <- WS.acceptRequest pending
  handle disconnect . WS.withPingThread c 30 (pure ()) $ do
    undefined
    -- Just auditRequest <- decode <$> WS.receiveData c
    -- view <- runAudit auditRequest
    -- WS.sendTextData c (encode view)
  where
    disconnect :: SomeException -> IO ()
    disconnect _ = pure ()


app :: Application
app = genericServeTWithContext id server EmptyContext
