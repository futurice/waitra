-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Waitra
-- Copyright   :  (c) 2015 Futurice
-- License     :  MIT (see the file LICENSE)
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
-- Stability   :  experimental
--
-- @Network.Waitra@ is a very simple router.
-- It's useful for writing simple API web-services,
-- when you don't want to use the whole Yesod stack.
----------------------------------------------------------------------------
module Network.Waitra
  (
  -- * Types
    Path
  , Route(..)
  -- * Static paths routes
  , simpleRoute
  , simpleGet
  , simplePost
  , simplePut
  , simpleDelete
  -- * Regex paths routes
  , routeGet
  , routePost
  , routePut
  , routeDelete
  -- * JSON helper
  , jsonApp
  , jsonApp'
  -- * Compilation
  , routeMiddleware
  , waitraMiddleware
  ) where

import           Data.Aeson
import           Data.String (fromString)
import qualified Data.Text as T
import qualified Network.HTTP.Types as H
import           Network.Wai
import           Text.Regex.Applicative

-- | We use strings, as - unluckily - `Text.Regex.Applicative` doesn't work with `Text` directly.
type Path = String

data Route = Route H.Method (RE Char Application)

simpleRoute :: H.Method -> Path -> Application -> Route
simpleRoute method r app = Route method (const app <$> string r)

simpleGet :: Path -> Application -> Route
simpleGet = simpleRoute H.methodGet

simplePost :: Path -> Application -> Route
simplePost = simpleRoute H.methodPost

simplePut :: Path -> Application -> Route
simplePut = simpleRoute H.methodPut

simpleDelete :: Path -> Application -> Route
simpleDelete = simpleRoute H.methodDelete

routeGet :: RE Char Application -> Route
routeGet = Route H.methodGet

routePost :: RE Char Application -> Route
routePost = Route H.methodPost

routeDelete :: RE Char Application -> Route
routeDelete = Route H.methodDelete

routePut :: RE Char Application -> Route
routePut = Route H.methodPut

path :: Request -> Path
path req = T.unpack . T.intercalate (T.pack "/") $ T.pack "" : pathInfo req

routeMiddleware :: Route -> Middleware
routeMiddleware (Route method re) app req =
   case (requestMethod req == method, path req =~ re) of
     (True, Just routeApp) -> routeApp req
     _                     -> app req

-- | Turn the list of routes into `Middleware`
waitraMiddleware :: [Route] -> Middleware
waitraMiddleware = foldr ((.) . routeMiddleware) id

jsonHeader :: H.Header
jsonHeader = (H.hContentType, fromString "application/json")

jsonApp :: (FromJSON a, ToJSON b) => (a -> IO (H.Status, H.ResponseHeaders, b)) -> Application
jsonApp f req respond = do
  body <- strictRequestBody req
  case eitherDecode body of
    Left err  -> respond $ responseLBS H.status400 [] $ fromString err
    Right x   -> do (status, headers, y) <- f x
                    respond $ responseLBS status (jsonHeader : headers) $ encode y

jsonApp' :: ToJSON b => IO (H.Status, H.ResponseHeaders, b) -> Application
jsonApp' io _req respond = do
  (status, headers, y) <- io
  respond $ responseLBS status (jsonHeader : headers) $ encode y
