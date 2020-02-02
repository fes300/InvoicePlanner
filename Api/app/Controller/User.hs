module Controller.User where

import Data.Aeson
import Data.Time
import Data.UUID
import Servant
import GHC.Generics
import Control.Monad.IO.Class
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Data.Pool

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