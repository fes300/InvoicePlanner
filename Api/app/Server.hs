module Server where

import Control.Monad.IO.Class
import Servant
import Data.Pool
import Database.PostgreSQL.Simple

type Message = String

type API = ReqBody '[PlainText] String :> Post '[JSON] NoContent
      :<|> Get '[JSON] [Message]

api :: Proxy API
api = Proxy

server :: Pool Connection -> Server API
server conns = postMessage :<|> getMessages
  where postMessage :: Message -> Handler NoContent
        postMessage msg = do
          liftIO . withResource conns $ \conn ->
            execute conn
                    "INSERT INTO messages VALUES (?)"
                    (Only msg)
          return NoContent

        getMessages :: Handler [Message]
        getMessages = fmap (map fromOnly) . liftIO $
          withResource conns $ \conn ->
            query_ conn "SELECT msg FROM messages"
