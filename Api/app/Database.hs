module Database where

import Data.ByteString (ByteString)
import Data.Aeson ()
import Control.Exception (bracket)
import Data.Pool
import Database.PostgreSQL.Simple
import LoadEnv
import System.Environment (getEnv)
import Data.ByteString.UTF8 as BSU

type DBConnectionString = ByteString

initDB :: DBConnectionString -> IO ()
initDB connstr = bracket (connectPostgreSQL connstr) close $ \conn -> do
  execute_ conn
    "CREATE TABLE IF NOT EXISTS messages (msg text not null)"
  return ()

initConnectionPool :: DBConnectionString -> IO (Pool Connection)
initConnectionPool connStr =
  createPool (connectPostgreSQL connStr)
              close
              2 -- stripes
              60 -- unused connections are kept open for a minute
              10 -- max. 10 connections open per stripe

getDBConnectionString :: IO DBConnectionString
getDBConnectionString = do
  loadEnv
  host <- getEnv "PG_HOST"
  password <- getEnv "PG_PASSWORD"
  user <- getEnv "PG_USER"
  db <- getEnv "PG_DBNAME"
  return $ BSU.fromString $ "host=" <> host <> " dbname=" <> db <> " user=" <> user <> " password=" <> password <> " connect_timeout=10"
