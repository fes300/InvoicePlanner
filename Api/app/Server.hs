module Server where

import Control.Monad.IO.Class
import Servant
import Data.Aeson as JSON
import Data.Pool
import Data.Time
import Data.UUID
import GHC.TypeLits
import GHC.Generics
import qualified Data.Text as T
import Database.PostgreSQL.Simple

type Message = String
type MessageId = T.Text
type Email = T.Text
type Password = T.Text
type UserId = T.Text

data User = User { registration_date :: UTCTime
  , email :: Email
  , password :: Password
  , uuid_ :: UUID }
  deriving (Generic, ToRow, FromRow)

data CreateUser = CreateUser { rawemail :: Email
  , rawpassword :: Password }
  deriving (Generic, ToRow, FromRow)

instance FromJSON User
instance ToJSON   User

instance FromJSON CreateUser
instance ToJSON   CreateUser

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

getUsers :: Pool Connection -> Handler [User]
getUsers conns = liftIO . withResource conns $ \conn ->
  query_ conn "SELECT * FROM users"

getUser :: Pool Connection -> UserId -> Handler User
getUser conns userid = unwrapUser =<< result
  where
    result :: FromRow r => Handler [r]
    result = liftIO . withResource conns $ \conn ->
      query conn "SELECT * FROM users WHERE uuid_=(?)"
                  (Only userid)
    unwrapUser :: FromRow r => [r] -> Handler r
    unwrapUser u = case u of
      [u] -> return u
      [] ->  throwError err404 { errBody = "user with UUID not found." }

postUser :: Pool Connection ->  CreateUser -> Handler NoContent
postUser conns (CreateUser { rawemail=useremail, rawpassword=userpassword }) = do
  liftIO . withResource conns $ \conn ->
    execute conn "INSERT INTO users (email, password) VALUES ((?), (?))"
                (useremail, userpassword)
  return NoContent

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
