{-# LANGUAGE DeriveGeneric #-}

{- |
   Module     : Types

   Maintainer : Shashank Singh <ec22267@qmul.ac.uk>

- This module has all the custom made data types used across the applciation. 
- FromRow and ToRow from the Database library are extensively used to modify the datatypes for interacting witht the database. 

Written by Shashank Singh, Naman Shrivastava, Vishal and Ziyad Shaikh
-}

module Types (
    Entry (..),
    SongCollectionArtistTrack (..),
    GenreTableValues (..),
    Record (..),
    Results (..),
    GenreValues (..),
    UserPlaylistValues(..),
    ForPlaylistAdd(..)
) where

import GHC.Generics

import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow

import Data.Aeson

{-- Datatype for Entries Table --}
{- | Datatype for Entries Table  -}
data Entry = Entry {
    date_ :: String,
    collectionName_ :: String,
    artistName_ :: String,
    kind_ :: String,
    trackName_ :: String,
    collectionId_ :: Int,
    artistId_ :: Int,
    trackTimeMillis_ :: Int,
    isStreamable_ :: Bool,
    fk_primaryGenreName :: Int
} deriving (Show)

{-- Datatype for extracting Collection, Artist and Track Name --}
{- | Datatype for extracting Collection, Artist and Track Name  -}
data SongCollectionArtistTrack = SongCollectionArtistTrack {
    collectionNameSong :: String,
    artistNameSong :: String,
    trackNameSong :: String
} deriving (Show)

data ForPlaylistAdd = ForPlaylistAdd {
    idForList :: Int,
    collectionNameForList :: String,
    artistNameForList :: String,
    trackNameForList :: String
} deriving (Show)

{-- Datatype for extracting details for Genre --}
{- | Datatype for extracting details for Genre  -}
data GenreTableValues = GenreTableValues {
    id_ :: Int,
    country_ :: String,
    currency_ ::  String,
    primaryGenreName_ :: String
} deriving (Show)

{-- Datatype for the whole details being extracted from the API call's Result values --}
{- | Datatype for the whole details being extracted from the API call's Result values -}
data Record = Record {
    date :: String,
    collectionName :: String,
    artistName :: String,
    kind :: String,
    trackCount :: Int,
    trackNumber :: Int,
    country :: String,
    currency ::  String,
    primaryGenreName :: String,
    artistId :: Int,
    collectionId :: Int,
    trackId :: Int,
    trackName :: String,
    trackTimeMillis :: Int,
    isStreamable :: Bool,
    extraField :: Maybe String
} deriving (Show, Generic)

{-- Datatype for extracting details from the API call --}
{- | Datatype for extracting details from the API call -}
data Results = Results {
    resultCount :: Int,
    results :: [Record]
} deriving (Show, Generic)

{-- Datatype for extracting details from userplaylist table --}
{- | Datatype for extracting details from userplaylist table -}
data UserPlaylistValues = UserPlaylistValues {
    id_USERPLAYLISTID :: Int,
    fk_FROMENTRIES :: Int
} deriving (Show, Generic)

data GenreValues = GenreValues { 
    genreId :: Int, 
    countryGenreValues :: String, 
    currencyGenreValues :: String, 
    primaryGenreGenreValues ::String
}deriving (Eq, Show)

{-- Making above datatype instances of FromRow type classes --}
{- | Making above datatype instances of FromRow type classes -}

instance FromRow Record where
    fromRow = Record <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance FromRow GenreTableValues where
    fromRow = GenreTableValues <$> field <*> field <*> field <*> field

instance FromRow GenreValues where
    fromRow = GenreValues <$> field <*> field <*> field <*> field 

instance FromRow SongCollectionArtistTrack where
    fromRow = SongCollectionArtistTrack <$> field <*> field <*> field

instance FromRow ForPlaylistAdd where
    fromRow = ForPlaylistAdd <$> field <*> field <*> field <*> field

instance FromRow UserPlaylistValues where
    fromRow = UserPlaylistValues <$> field <*> field

instance FromRow Entry where
        fromRow = Entry <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

{-- Making above datatype instances of ToRow type classes --}
{- | Making above datatype instances of ToRow type classes -}

instance ToRow GenreTableValues where
    toRow (GenreTableValues i coun cont pop)
        = toRow (i, coun, cont, pop)

instance ToRow ForPlaylistAdd where
    toRow (ForPlaylistAdd i coun cont pop)
        = toRow (i, coun, cont, pop)

instance ToRow Entry where
    toRow (Entry dt dy m y tn ci ai tms is fk_c)
        = toRow (dt, dy, m, y, tn, ci, ai, tms, is, fk_c)

instance ToRow SongCollectionArtistTrack where
    toRow (SongCollectionArtistTrack dt dy m )
        = toRow (dt, dy, m)

instance ToRow GenreValues where
    toRow (GenreValues dt dy m x)
        = toRow (dt, dy, m, x)

instance ToRow UserPlaylistValues where
    toRow (UserPlaylistValues idVal fkValue)
        = toRow (idVal, fkValue)

{-- Making above datatype instances of FromJSON type class --}
{- | Making above datatype instances of FromJSON type class -}

renameFields :: String -> String
renameFields "date" = "releaseDate"
renameFields other = other

customOptions :: Options
customOptions = defaultOptions {
    fieldLabelModifier = renameFields
}

instance FromJSON Record where
    parseJSON = genericParseJSON customOptions

instance FromJSON Results
