{-# LANGUAGE DeriveGeneric #-}

{- |
   Module     : Parse

   Maintainer : Shashank Singh <ec22267@qmul.ac.uk>

This module handles Parsing the JSON data using the Aeson library. 

Written by Shashank Singh, Naman Shrivastava, Vishal and Ziyad Shaikh
-}

module Parse (
    parseRecords,
) where

import Types
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8

{- | parseRecords function is used to parse the JSON data that was received using the Data.Aeson library. -}

parseRecords :: L8.ByteString -> Either String Results
parseRecords j = eitherDecode j :: Either String Results