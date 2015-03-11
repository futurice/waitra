{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Waitra
-- Copyright   :  (c) 2015 Futurice
-- License     :  MIT (see the file LICENSE)
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
-- Stability   :  experimental
-- Portability :  GADTs and RankNTypes
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
  -- * Compilation
  , compile
  , compileRoute
  ) where

import qualified Data.Text as T
import qualified Network.HTTP.Types as H
import           Network.Wai
import           Text.Regex.Applicative

-- | We use strings, as - unluckily - `Text.Regex.Applicative` doesn't work with `Text` directly.
type Path = String

data Route where
  Route :: forall a. H.Method -> RE Char a -> (a -> Application) -> Route

simpleRoute :: H.Method -> Path -> Application -> Route
simpleRoute method r app = Route method (string r) (const app)

simpleGet :: Path -> Application -> Route
simpleGet = simpleRoute H.methodGet

simplePost :: Path -> Application -> Route
simplePost = simpleRoute H.methodPost

simplePut :: Path -> Application -> Route
simplePut = simpleRoute H.methodPut

simpleDelete :: Path -> Application -> Route
simpleDelete = simpleRoute H.methodDelete

routeGet :: RE Char a -> (a -> Application) -> Route
routeGet = Route H.methodGet

routePost :: RE Char a -> (a -> Application) -> Route
routePost = Route H.methodPost

routeDelete :: RE Char a -> (a -> Application) -> Route
routeDelete = Route H.methodDelete

routePut :: RE Char a -> (a -> Application) -> Route
routePut = Route H.methodPut

path :: Request -> Path
path req = T.unpack . T.intercalate "/" $ "" : pathInfo req

compileRoute :: Route -> Middleware
compileRoute (Route method re routeApp) app req =
   case (requestMethod req == method, path req =~ re) of
     (True, Just m')  -> routeApp m' req
     _                -> app req

-- | Turn the list of routes into `Middleware`
compile :: [Route] -> Middleware
compile = foldr (.) id . map compileRoute
