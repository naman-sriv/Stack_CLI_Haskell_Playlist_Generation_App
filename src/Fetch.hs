{- |
   Module     : Fetch

   Maintainer : Shashank Singh <ec22267@qmul.ac.uk>

This module handles connecting to the URLs of iTunes API. 
Also, this corraborates the  URL to be valid and invalid. 

Written by Shashank Singh, Naman Shrivastava, Vishal and Ziyad Shaikh
-}

module Fetch (
    download
) where

import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Simple

type URL = String

{- | Function to set a connection with the specified URL for fetching the data via the API. 
        Here, the URL is also being checked, if or not it is a Valid URL. 
            If not, it send back "Invalid URL" string packed as type Data.ByteString.Lazy.Char8 -}
download :: URL -> IO L8.ByteString
download url = do
    -- request <- parseRequest url
    -- response <- httpLBS request
    -- return $ getResponseBody response
    case (parseRequest url) of
        Left err -> do
            putStrLn ("Error: " ++ show(err))
            return (L8.pack "Invalid URL")
        Right request -> do
            response <- httpLBS request
            return $ getResponseBody response
