{-# LANGUAGE OverloadedStrings #-}

{- |
   Module     : Database

   Maintainer : Shashank Singh <ec22267@qmul.ac.uk>

- The module handles all the queries related to extraction from the database 
and also few of the queries realted to validation across the application. 
- The validation aids the application to print errors on the console, and avoid hard crashes. 

Written by Shashank Singh, Naman Shrivastava, Vishal and Ziyad Shaikh
-}

-- or, on GHCI:
-- > :set -XOverloadedStrings

module Database (
    initialiseDB,
    getOrCreateGenre,
    saveRecords,
    querySearchByAvailGenre,
    queryGetAllGenres,
    queryPlaylistGetValues,
    queryGetAllArtists,
    querySongsForPlaylist,
    queryDeleteSongFromPlaylist,
    querySearchByArtistName,
    queryAddSongToPlaylist
) where

import Types
import Database.SQLite.Simple
    ( Connection,
      execute,
      execute_,
      open,
      query,
      queryNamed,
      NamedParam((:=)) )

{- | Using the connection of a database, this function creates genre, entries and userplaylist
     tables if they don't already exist  -}
initialiseDB :: IO Connection
initialiseDB = do
        conn <- open "musicApp.sqlite"
        execute_ conn "CREATE TABLE IF NOT EXISTS genre (\
            \id INTEGER PRIMARY KEY AUTOINCREMENT,\
            \country VARCHAR(80) NOT NULL, \
            \currency VARCHAR(50) NOT NULL, \
            \primaryGenreName VARCHAR(50) DEFAULT NULL \
            \)"
        execute_ conn "CREATE TABLE IF NOT EXISTS entries (\
            \id INTEGER PRIMARY KEY AUTOINCREMENT,\
            \date VARCHAR(40) NOT NULL, \
            \collection_name VARCHAR(40) NOT NULL, \
            \artist_name VARCHAR(40) NOT NULL, \
            \kind VARCHAR(40) NOT NULL, \
            \trackName VARCHAR(200) DEFAULT NULL, \
            \collectionId INT DEFAULT NULL, \
            \artistId INT DEFAULT NULL, \
            \trackTimeMillis INT DEFAULT NULL, \
            \isStreamable INT DEFAULT NULL, \
            \fk_primaryGenreName INTEGER\
            \)"
        execute_ conn "CREATE TABLE IF NOT EXISTS userplaylist (\
            \id INTEGER PRIMARY KEY AUTOINCREMENT,\
            \fk_from_entries INTEGER\
            \)"
        return conn

-- | Inserts a only UNIQUE genres in the genre table, by checking if or not it already exists 
getOrCreateGenre :: Connection -> String -> String -> String -> IO GenreTableValues
getOrCreateGenre conn coun cont pop = do
    resultsValues <- queryNamed conn "SELECT * FROM genre WHERE country=:country AND primaryGenreName=:primaryGenreName" [":country" := coun, ":primaryGenreName" := pop]
    if length resultsValues > 0 then
        return . head $ resultsValues
    else do
        execute conn "INSERT INTO genre (country, currency, primaryGenreName) VALUES (?, ?, ?)" (coun, cont, pop)
        getOrCreateGenre conn coun cont pop

-- | Inserts all the values recieved from the API Call in the entries table 
createRecord :: Connection -> Record -> IO ()
createRecord conn record = do
    c <- getOrCreateGenre conn (country record) (currency record) (primaryGenreName record)
    let entry = Entry {
        date_ = date record,
        collectionName_ = collectionName record,
        artistName_ = artistName record,
        kind_ = kind record,
        trackName_ = trackName record,
        collectionId_ = collectionId record,
        artistId_ = artistId record,
        trackTimeMillis_ = trackTimeMillis record,
        isStreamable_ = isStreamable record,
        fk_primaryGenreName = id_ c
    }
    execute conn "INSERT INTO entries (date, collection_name, artist_name, kind, trackName, collectionId, artistId, trackTimeMillis, isStreamable, fk_primaryGenreName) VALUES (?,?,?,?,?,?,?,?,?,?)" entry

-- | Calls the INSERT for for the  values from the API Call 
saveRecords :: Connection -> [Record] -> IO ()
saveRecords conn = mapM_ (createRecord conn)

-- | Collect Collection, Artist and Track Name using the genre received 
querySearchByAvailGenre :: Connection -> String -> IO [SongCollectionArtistTrack]
querySearchByAvailGenre conn genVal = do
    putStrLn $ "Looking for " ++ genVal ++ " entries..."
    let genvalueForLike = "%" ++ genVal ++ "%"
    let sql = "SELECT collection_name, artist_name, trackName FROM entries inner join genre on entries.fk_primaryGenreName == genre.id WHERE primaryGenreName LIKE ?"
    query conn sql [genvalueForLike]

-- | Get all the Genre from the genre table
queryGetAllGenres :: Connection -> IO [GenreTableValues]
queryGetAllGenres conn = do
    let sql = "SELECT * FROM genre WHERE \"1\" = ?"
    let val = "1" :: String
    query conn sql [val]

-- | Collect ALL the Collection, Artist and Track Name from entries table, using "1" = "1" condition, which is always TRUE
queryGetAllArtists :: Connection -> IO [SongCollectionArtistTrack]
queryGetAllArtists conn = do
    let sql = "SELECT collection_name, artist_name, trackName FROM entries WHERE \"1\" = ? GROUP BY artist_name ORDER BY artist_name"
    let val = "1" :: String
    query conn sql [val]

-- | Collect ALL columns from userplaylist from userplaylist table, using "1" = "1" condition, which is always TRUE
queryPlaylistGetValues :: Connection -> IO [UserPlaylistValues]
queryPlaylistGetValues conn = do
    let sql = "SELECT * FROM userplaylist WHERE \"1\" = ?"
    let val = "1" :: String
    query conn sql [val]

-- | Collect the Collection, Artist and Track Name from a particular Genre Name 
querySongsForPlaylist :: Connection -> Int -> IO [SongCollectionArtistTrack]
querySongsForPlaylist conn fk_val = do
    let sql = "SELECT collection_name, artist_name, trackName FROM entries inner join genre on entries.fk_primaryGenreName == genre.id WHERE entries.id = ?"
    query conn sql [fk_val]

{- | Delete rows from the userplaylist table for the given id
     this handles the exceptionw here the provied id is not present i.e. was never inserted or has already been deleted  -}
queryDeleteSongFromPlaylist :: Connection -> Int -> IO()
queryDeleteSongFromPlaylist conn idForDel = do
    let sql = "DELETE FROM userplaylist WHERE id = ?"
    x <- queryHelperDeleteFromPlaylist conn idForDel
    if null x
        then do
            putStrLn ("No entry found for " ++ show(idForDel  :: Int))
    else do
        execute conn sql [idForDel]
        putStrLn ("Entry Deleted Successfully" :: String)

-- | Helper function for queryDeleteSongFromPlaylist to get ALL the data from userplaylist table 
queryHelperDeleteFromPlaylist :: Connection -> Int -> IO [UserPlaylistValues]
queryHelperDeleteFromPlaylist conn idForDel = do
    let sql = "SELECT * FROM userplaylist WHERE id = ?"
    query conn sql [idForDel]

-- | Collect the values fromt the entries table 
querySearchByArtistName :: Connection -> String -> IO [ForPlaylistAdd]
querySearchByArtistName conn artistVal = do
    putStrLn $ "Looking for " ++ artistVal ++ " entries..."
    let artistValueForLike = "%" ++ artistVal ++ "%"
    let sql = "SELECT entries.id, collection_name, artist_name, trackName FROM entries inner join genre on entries.fk_primaryGenreName == genre.id WHERE artist_name LIKE ?"
    query conn sql [artistValueForLike]

-- | Insert the fk_from_entries value in the userplaylist table, which references the entires table, which has all the Songs, Collection, Artist details
queryAddSongToPlaylist :: Connection -> Int -> String -> IO()
queryAddSongToPlaylist conn idForAdd artistNameVal = do
    let sql = "INSERT INTO userplaylist (fk_from_entries) VALUES (?)"
    x <- queryHelperAddToPlaylist conn idForAdd artistNameVal
    if null x
        then do
            putStrLn ("No entry found for " ++ show(idForAdd :: Int) ++ " and Artist: " ++ show(artistNameVal :: String))
    else do
        x2 <- queryHelperCheckExistingPlaylist conn idForAdd
        if null x2
            then do
                execute conn sql [idForAdd]
                putStrLn ("Entry Added Successfully" :: String)
        else do
            putStrLn ("Entry Already exists in the playlist for Serial Number: " ++ show(idForAdd :: Int))

-- | Helper function for queryAddSongToPlaylist to get the data from entries table 
queryHelperAddToPlaylist :: Connection -> Int -> String -> IO [ForPlaylistAdd]
queryHelperAddToPlaylist conn idForAdd artistVal = do
    let artistValueForLike = "%" ++ artistVal ++ "%"
    let sql = "SELECT entries.id, collection_name, artist_name, trackName FROM entries inner join genre on entries.fk_primaryGenreName == genre.id WHERE artist_name LIKE ? AND entries.id = ?"
    query conn sql (artistValueForLike, idForAdd)

{- | Helper function for queryHelperCheckExistingPlaylist to get the data from userplaylist table  
     this is used to check IF or NOT we are adding the same song to the playlist again.  -}
queryHelperCheckExistingPlaylist :: Connection -> Int -> IO [UserPlaylistValues]
queryHelperCheckExistingPlaylist conn idForAdd = do
    let sql = "SELECT * FROM userplaylist WHERE fk_from_entries = ?"
    query conn sql [idForAdd]