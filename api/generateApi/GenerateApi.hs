module Main where

import Servant
import Controller.User (User, Password, Email, UserId, CreateUser)
import Server (API)
import Servant.Elm (DefineElm (DefineElm), ElmOptions(urlPrefix),
                    Proxy (Proxy), UrlPrefix(Static), defaultOptions, defaultElmToString, defaultTypeAlterations,
                    defElmImports, defElmOptions, deriveBoth, emptyResponseElmTypes, elmToString,
                    generateElmModuleWith, deriveElmDef, elmAlterations, elmTypeAlterations)
import Data.Time (UTCTime, Day, DiffTime)
import qualified Data.Text as TX
import Data.Fixed (Pico)
import Elm.Module (recAlterType, defaultAlterations)
import Elm.TyRep(EType(..), ETCon(..), ETypeDef(..), EAlias(..), ETypeName(..))

customTypeAlterations :: EType -> EType
customTypeAlterations t =
  case t of
    ETyCon (ETCon "PosixTime") -> ETyCon (ETCon "Posix")
    _ -> defaultTypeAlterations t

customToString :: EType -> TX.Text
customToString t =
  case t of
    (ETyCon (ETCon "PosixTime")) -> "(Time.posixToMillis >> String.fromInt)"
    _ -> defaultElmToString t

myElmOpts :: ElmOptions
myElmOpts = defElmOptions { urlPrefix = Static "http://localhost:8000"
  , elmAlterations = defaultAlterations
  , elmTypeAlterations = customTypeAlterations
  , elmToString = customToString
}

deriveElmDef defaultOptions ''NoContent

myImports :: TX.Text
myImports = TX.unlines ["import Time exposing (Posix)", defElmImports]

main :: IO ()
main =
  generateElmModuleWith
    myElmOpts
    [ "Generated"
    , "Api"
    ]
    myImports
    "../front/src"
    [ DefineElm (Proxy :: Proxy User)
      , DefineElm (Proxy :: Proxy CreateUser)
      , DefineElm (Proxy :: Proxy Password)
      , DefineElm (Proxy :: Proxy NoContent)
      , DefineElm (Proxy :: Proxy Email)
      , DefineElm (Proxy :: Proxy UserId)
    ]
    (Proxy :: Proxy API)