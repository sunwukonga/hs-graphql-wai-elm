{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)


app2 :: Application
app2 _ respond = respond index

index :: Response
index = responseFile
    status200
    [("Content-Type", "text/html")]
    "spa/index.html"
    Nothing

main :: IO ()
main = do
  putStrLn $ "localhost:8000"
  run 8000 app2
