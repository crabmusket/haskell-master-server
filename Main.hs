{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Data.Text.Lazy.Encoding (decodeUtf8)

main = scotty 3000 $ do
    get "/games" $ html "Hi there"
    post "/games" $ do
        stuff <- fmap decodeUtf8 body
        html stuff

