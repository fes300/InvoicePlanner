module Main where

import Network.Wai.Handler.Warp
import Servant
import Control.Monad.IO.Class
import Data.Pool
import LoadEnv
import System.Environment (getEnv)
import Database.PostgreSQL.Simple
import Database
import Data.ByteString.UTF8 as BSU

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

runApp :: Pool Connection -> IO ()
runApp conns = run 8081 (serve api $ server conns)

getDBConnectionString :: IO DBConnectionString
getDBConnectionString = do
  loadEnv
  host <- getEnv "PG_HOST"
  password <- getEnv "PG_PASSWORD"
  user <- getEnv "PG_USER"
  db <- getEnv "PG_DBNAME"
  return $ BSU.fromString $ "host=" <> host <> " dbname=" <> db <> " user=" <> user <> " password=" <> password <> " connect_timeout=10"

main :: IO ()
main = do
  loadEnv
  connection <- getDBConnectionString
  pool  <- initConnectionPool connection
  initDB connection
  runApp pool
