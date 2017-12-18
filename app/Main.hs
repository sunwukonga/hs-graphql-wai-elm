{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Data.Text (Text)
import Data.Monoid ((<>))
import qualified Data.ByteString as B hiding (pack)
import Data.ByteString.Lazy as L (fromStrict)

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

import GraphQL
import GraphQL.API (Object, Field, Argument, (:>), Union)
import GraphQL.Resolver (Handler, (:<>)(..), unionValue)

--Local import
import GraphQL.Wai (toApplication)

-- Define GraphQL Schema
type Hello = Object "Hello" '[]
  '[ Argument "who" Text :> Field "greeting" Text ]

-- Define Handler for Hello Schema
hello :: Handler IO Hello
hello = pure (\who -> pure ("Hello " <> who))

--type Hello {
--  greeting(who: String!): String!
--}

--{
--  greeting (who: "Tim")
--}

-- Define Routes
app2 :: Application
app2 request respond = do
  reqBS <- requestBody request
  print reqBS
  case rawPathInfo request of
    "/graphql/"   -> toApplication @Hello hello request respond
    "/graphql"    -> toApplication @Hello hello request respond
    "/check"      -> respond $ checkdata reqBS
    "/"           -> respond indexhtml

indexhtml :: Network.Wai.Response
indexhtml = responseFile
  status200
  [("Content-Type", "text/html")]
  "spa/index.html"
  Nothing

checkdata :: B.ByteString -> Network.Wai.Response
checkdata request = responseLBS
  status200
  [("Content-Type", "text/html")]
  (L.fromStrict request)

main :: IO ()
main = do
  putStrLn $ "localhost:8000"
  run 8000 app2
