module Main where

import Network.Wai.Handler.Warp
import Servant
import Database.PostgreSQL.Simple
import Data.Pool
import Database
import Server

runApp :: Pool Connection -> IO ()
runApp conns = run 8081 (serve api $ server conns)

main :: IO ()
main = do
  connection <- getDBConnectionString
  pool  <- initConnectionPool connection
  initDB connection
  runApp pool
