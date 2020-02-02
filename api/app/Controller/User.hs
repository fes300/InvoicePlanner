module Controller.User where

-- import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Time (UTCTime)
import Servant
import GHC.Generics
import Control.Monad.IO.Class
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField
import Data.Pool
import Servant.Elm  (DefineElm (DefineElm), ElmOptions(urlPrefix),
                     Proxy (Proxy), UrlPrefix(Static), defaultOptions,
                     defElmImports, defElmOptions, deriveBoth,
                     generateElmModuleWith, deriveElmDef)

newtype Email = Email T.Text
deriveBoth defaultOptions ''Email
instance ToField Email where
  toField = toField
instance FromField Email where
  fromField = fromField

newtype Password = Password T.Text
deriveBoth defaultOptions ''Password
instance ToField Password where
  toField = toField
instance FromField Password where
  fromField = fromField

newtype UserId = UserId T.Text
deriveBoth defaultOptions ''UserId
instance ToField UserId where
  toField = toField
instance FromField UserId where
  fromField = fromField
instance FromHttpApiData UserId where
  parseUrlPiece = parseUrlPiece
  parseHeader = parseHeader
  parseQueryParam = parseQueryParam

data User = User { registration_date :: UTCTime
  , email :: Email
  , password :: Password
  , uuid_ :: UserId }
  deriving (Generic, ToRow, FromRow)

deriveBoth defaultOptions ''User

data CreateUser = CreateUser { rawemail :: Email
  , rawpassword :: Password }
  deriving (Generic, ToRow, FromRow)
deriveBoth defaultOptions ''CreateUser


getUsers :: Pool Connection -> Handler [User]
getUsers conns = liftIO . withResource conns $ \conn ->
  query_ conn "SELECT * FROM users"

getUser :: Pool Connection -> UserId -> Handler User
getUser conns (UserId userid) = unwrapUser =<< result
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


