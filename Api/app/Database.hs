module Database where

import Data.ByteString (ByteString)
import Data.Aeson ()
import Control.Exception (bracket)
import Data.Pool
import Database.PostgreSQL.Simple

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
