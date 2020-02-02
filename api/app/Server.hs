module Server where

import Servant
import Data.Pool
import GHC.TypeLits
import GHC.Generics
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Controller.User

type Message = String
type MessageId = T.Text

-- Simple Utility to create a basic crud API. Three endpoints are created:
--   - GET /<name>
--   - GET /<name>/<some 'i'>
--   - POST /<name>
type SimpleAPI (name :: Symbol) a b i = name :>
  (                         Get '[JSON] [a]
  :<|> Capture "id" i    :> Get '[JSON] a
  :<|> ReqBody '[JSON] b :> Post '[JSON] NoContent
  )

simpleServer :: Handler [a]
  -> (i -> Handler a)
  -> (b -> Handler NoContent)
  -> Server (SimpleAPI name a b i)
simpleServer listAs getA postA =
  listAs :<|> getA :<|> postA

type API = SimpleAPI "messages" Message Message MessageId
      :<|> SimpleAPI "users" User CreateUser UserId

api :: Proxy API
api = Proxy

userServer :: Pool Connection -> Server (SimpleAPI "users" User CreateUser UserId)
userServer conns = simpleServer
  (getUsers conns)
  (getUser conns)
  (postUser conns)

messagesServer :: Pool Connection -> Server (SimpleAPI "messages" Message Message MessageId)
messagesServer conns = simpleServer
  (return ["Great stuff"])
  (\_productid -> return "Great stuff")
  (\_product -> return NoContent)

server :: Pool Connection -> Server API
server conn = (messagesServer conn) :<|> (userServer conn)
