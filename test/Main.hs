{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Data.Aeson
import           Data.String (fromString)
import           Network.Wai
import           Network.Wai.Test
import           Network.Waitra
import           Test.Tasty
import           Network.HTTP.Types.Header
import           Test.Tasty.HUnit

echoRoute :: Route
echoRoute = routeGet (echoApp <$ string "/api/echo/" <*> many anySym)
  where echoApp msg _req respond = respond $ responseLBS status200 [] (fromString msg)

jsonRoute :: Route
jsonRoute = simpleGet "/api/json" $ jsonApp (return . f)
  where f (Object _) = (status200, [], "object" :: String)
        f (Array _)  = (status200, [], "array")
        f v          = (status404, [], show v)

emptyApp :: Application
emptyApp _ respond = respond $ responseLBS status404 [] (fromString "Fallback: Not found")

app :: Application
app = waitraMiddleware [echoRoute, jsonRoute] emptyApp

runApp :: Session a -> IO a
runApp s = runSession s app

echoCase1 :: IO ()
echoCase1 = runApp $ do
  let req = setPath defaultRequest "/api/echo/foobar"
  res <- request req
  assertStatus 200 res
  assertBody "foobar" res

echoCase2 :: IO ()
echoCase2 = runApp $ do
  let req = setPath defaultRequest "/not-found"
  res <- request req
  assertStatus 404 res

jsonCase1 :: IO ()
jsonCase1 = runApp $ do
  let req = setPath defaultRequest "/api/json"
  res <- srequest $ SRequest req "{}"
  assertStatus 200 res
  assertHeader hContentType "application/json" res
  assertBody "\"object\"" res

-- Cannot parse random string as json
jsonCase2 :: IO ()
jsonCase2 = runApp $ do
  let req = setPath defaultRequest "/api/json"
  res <- srequest $ SRequest req "foo"
  assertStatus 400 res

tests :: TestTree
tests = testGroup "Unit tests"
  [ testCase "echo case 1" echoCase1
  , testCase "echo case 2" echoCase2
  , testCase "json case 1" jsonCase1
  , testCase "json case 2" jsonCase2
  ]

main :: IO ()
main = defaultMain tests
