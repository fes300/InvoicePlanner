module Server where

import Control.Monad.IO.Class
import Servant
import Data.Aeson
import Data.Pool
import Data.Time
import GHC.TypeLits
import GHC.Generics
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

type Message = String
type MessageId = T.Text
type Email = T.Text
type Password = T.Text
type UserId = T.Text

data User = User {
  password :: Password,
  email :: Email,
  registration_date :: UTCTime
}
  deriving Generic

instance FromJSON User
instance ToJSON   User
instance FromField User where
  fromField = fromJSONField
instance ToField User where
  toField = toJSONField

-- Simple Utility to create a basic crud API. Three endpoints are created:
--   - GET /<name>
--   - GET /<name>/<some 'i'>
--   - POST /<name>
type SimpleAPI (name :: Symbol) a i = name :>
  (                         Get '[JSON] [a]
  :<|> Capture "id" i    :> Get '[JSON] a
  :<|> ReqBody '[JSON] a :> Post '[JSON] NoContent
  )

simpleServer :: Handler [a]
  -> (i -> Handler a)
  -> (a -> Handler NoContent)
  -> Server (SimpleAPI name a i)
simpleServer listAs getA postA =
  listAs :<|> getA :<|> postA

type API = SimpleAPI "messages" Message MessageId
      :<|> SimpleAPI "users" User UserId

api :: Proxy API
api = Proxy

getUsers :: Pool Connection -> Handler [User]
getUsers conns = fmap (map fromOnly) . liftIO $
  withResource conns $ \conn ->
    query_ conn "SELECT * FROM users"

getUser :: Pool Connection ->  UserId -> Handler User
getUser conns userid = fmap (head . map fromOnly) . liftIO $
  withResource conns $ \conn ->
    query conn "SELECT * FROM users WHERE uuid_=(?)"
                (Only userid)

postUser :: Pool Connection ->  User -> Handler NoContent
postUser conns user = do
  liftIO . withResource conns $ \conn ->
    execute conn "INSERT INTO users VALUES (?)"
                (Only user)
  return NoContent

userServer :: Pool Connection -> Server (SimpleAPI "users" User UserId)
userServer conns = simpleServer
  (getUsers conns)
  (getUser conns)
  (postUser conns)

messagesServer :: Pool Connection -> Server (SimpleAPI "messages" Message MessageId)
messagesServer conns = simpleServer
  (return ["Great stuff"])
  (\_productid -> return "Great stuff")
  (\_product -> return NoContent)

server :: Pool Connection -> Server API
server conn = (messagesServer conn) :<|> (userServer conn)
