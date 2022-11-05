module Server where

import "base" GHC.Generics (Generic)
import "wai" Network.Wai (Application)
import "websockets" Network.WebSockets.Connection
  ( Connection
  , PendingConnection
  )
import "servant-server" Servant
  ( Context (EmptyContext)
  , Handler
  )
import "servant" Servant.API (NamedRoutes, type (:>))
import "servant" Servant.API.Generic ((:-))
import "servant-websockets" Servant.API.WebSocket
import "servant-server" Servant.Server.Generic
  ( AsServerT
  , genericServeTWithContext
  )


data Routes mode = Routes
  { channel :: mode :- "channel" :> WebSocketPending
  }
  deriving stock Generic


server :: Routes (AsServerT Handler)
server = Routes
  { channel = runChannel
  }


runChannel :: PendingConnection -> Handler ()
runChannel = undefined


app :: Application
app = genericServeTWithContext id server EmptyContext
