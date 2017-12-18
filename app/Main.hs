{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Text (Text)
import Control.Monad ((>>=))
import Data.Maybe ( fromMaybe )
import Data.Monoid ((<>))
import qualified Data.ByteString as B hiding (pack)
import Data.ByteString.Lazy as L (fromStrict)
import qualified Data.HashMap.Strict as HM (HashMap, empty)
import Data.Aeson (FromJSON(parseJSON), decode, withObject, (.:), (.:?), (.!=))
import Data.Aeson.Types (parseMaybe, Value)


import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

import GraphQL
import GraphQL.API (Object, Field, Argument, (:>), Union)
import GraphQL.Resolver (Handler, (:<>)(..), unionValue)

import GraphQL.Wai (toApplication)

-- From http://graphql.org/learn/serving-over-http/#post-request
--    Well formed queries sent by POST, _with_ content-type=application/json
--    _and_ _not_ having a query string param "?query=" should look like this.
-- Root GraphQL Query --
data GraphQLPostRequest = GraphQLPostRequest { query          :: String
                                             , operationName  :: String
                                             , variables      :: HM.HashMap Text Text
                                             } deriving (Show)

instance FromJSON GraphQLPostRequest where
  parseJSON = withObject "graphqlpostrequest" $ \o -> do
    query         <- o .:  "query"
    operationName <- o .:? "operationName" .!= ""
    variables     <- o .:? "variables"     .!= HM.empty
    return GraphQLPostRequest{..}


--    GraphQLPostRequest <$> o .: "query" <*> o .: "operationName" <*> o .: "variables"

-- parseGraphQLPostRequest :: Value -> Parser GrapQLPostRequest

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

-- :tick: check path
-- check request method
--    POST
--

-- Define Routes
app2 :: Application
app2 request respond = do
  reqBS <- requestBody request
  print reqBS
  let (decoded :: Maybe Data.Aeson.Types.Value) = decode ( L.fromStrict reqBS )
  print decoded
  let (rootQuery :: Maybe GraphQLPostRequest) = parseMaybe parseJSON =<< decode ( L.fromStrict reqBS )
  print rootQuery
--  print reqBS
--  case lookup "Content-Type" (requestHeaders request) of
--    Just ctype   -> print ctype
--    _            -> return ()
--  case getRequestBodyType request of
--    Just UrlEncoded -> print bodytype
--    _              -> print "Nothing"
--  let pathinfo = pathInfo request
--  print pathinfo

--     _______________________________________________________________________________________
--     ( POST                  ,  application/json                               ,  /graphql )
  case ((requestMethod request), (lookup "Content-Type" (requestHeaders request)), (rawPathInfo request)) of
    ("POST", Just "application/json", "/graphql") -> respond $ responseLBS notImplemented501 [] "STATUS 501: That baby's not ready!"
    ("GET" , _                      , "/graphql") -> toApplication @Hello hello request respond
    ("GET" , _                      , "/check"  ) -> checkdata <$> requestBody request >>= respond
    ("GET" , _                      , "/"       ) -> respond indexhtml
    (_     , _                      , _         ) -> respond $ responseLBS status404 [] "That's not my baby!"

--      case rawPathInfo request of
--        "/graphql"    -> toApplication @Hello hello request respond
--        _             -> respond $ responseLBS status404 [] "That's not my baby!"
--
--    "GET"  ->
--      case rawPathInfo request of
--        "/check"      -> respond $ checkdata reqBS
--        "/"           -> respond indexhtml
--        _             -> respond $ responseLBS status404 [] "That's not my baby!"

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
