module Main where

import Network.Wai.Handler.Warp
import Servant
import Database.PostgreSQL.Simple
import Data.Pool
import Database
import Server

port :: Int
port = 8081

runApp :: Pool Connection -> IO ()
runApp conns = run port (serve api $ server conns)

main :: IO ()
main = do
  connection <- getDBConnectionString
  _ <- putStrLn $ "listening to port" <> show port
  pool  <- initConnectionPool connection
  runApp pool
