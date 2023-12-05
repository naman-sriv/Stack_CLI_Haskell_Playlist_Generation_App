{-# LANGUAGE OverloadedStrings #-}

{- |
   Module     : CustomMethodsCreated

   Maintainer : Shashank Singh <ec22267@qmul.ac.uk>

This module has all the methods that are used throughout the application, significant ones are the methods reponsible for: 

- First paragraph. Parsing the JSON data.
- Printing the details of songs. 
- Displaying all the details from the Playlist. 


Written by Shashank Singh, Naman Shrivastava, Vishal and Ziyad Shaikh
-}

-- or, on GHCI:
-- > :set -XOverloadedStrings

module CustomMethodsCreated (
        printAllGenreNames,
        printAllArtistNames,
        printSongsByGenreNames,
        printSongsInPlaylist,
        printSongsByArtistForPlaylist,
        fetchAndParseData
) where

import Types
import Database
import Database.SQLite.Simple
import Fetch
import Parse
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as L8

{- | Function to Fetch and Parse the API data, which uses the functions defined in Fetch.hs and Parse.hs file. 
        Here, the provided URL is also validated to check if or not it is empty, or returns a 404 error, and also if or not it is Valid.
                If any of the problems are found, same are displayed on the console along with a raw Error Message that the haskell application throws. -}
fetchAndParseData :: Connection -> String -> IO()
fetchAndParseData conn url = do
        json <- download url
        if T.isInfixOf "404 Not Found" (T.pack (L8.unpack json))
                then do
                        print ("Unable to fetch" :: String)
        else if json == "Invalid URL"
                then do
                        print("Invalid URL found" :: String)
        else do
                case (parseRecords json) of
                        Left err -> do
                                print ("Ooops!! No data found on iTunes for the search criteria!! :( " :: String)
                                print ("Error: " ++ show(err))
                                print ("Please try a different search value " :: String)
                        Right recs -> do
                                if resultCount recs == 0
                                        then do
                                                print ("No data found on iTunes for the search criteria!! Please try a different search value " :: String)
                                else do
                                        print (show (resultCount recs) ++ " values found!!!" :: String)
                                        print ("Downloading and Parsing..." :: String)
                                        let name = results recs
                                        print ("Saving on DB..." :: String)
                                        saveRecords conn (results recs)
                                        printTrackArtistCollectionName name 1
                                        print ("All above entries were downloaded and saved on DB!! " :: String)

{- | Prints all the Genre values that are present in the genre Table, using the GenreTableValues datatype -}
printAllGenreNames :: [GenreTableValues] -> Int -> IO()
printAllGenreNames [] _ = return ()
printAllGenreNames (x:xs) i = do
        putStrLn ("(" ++ show i ++ ") " ++ show (primaryGenreName_ x))
        printAllGenreNames xs (i + 1)

{- | Function to print the Track Name, Artist Name and Collection Name from Record datatype -}
printTrackArtistCollectionName :: [Record] -> Int -> IO()
printTrackArtistCollectionName [] _ = return ()
printTrackArtistCollectionName (x:xs) i = do
        putStrLn ("(" ++ show i ++ ") " ++ show (trackName x) ++ " by: " ++ show (artistName x) ++ " from: " ++ show (collectionName x))
        printTrackArtistCollectionName xs (i + 1)

{- | Prints all the Artist Name values that are retirived from the entries Table, using the SongCollectionArtistTrack datatype -}
printAllArtistNames :: [SongCollectionArtistTrack] -> Int -> IO()
printAllArtistNames [] _ = return ()
printAllArtistNames (x:xs) i = do
        putStrLn ("(" ++ show i ++ ") " ++ show (artistNameSong x))
        printAllArtistNames xs (i + 1)

{- | Function to print the Track Name, Artist Name and Collection Name from SongCollectionArtistTrack datatype -}
printSongsByGenreNames :: [SongCollectionArtistTrack] -> Int -> IO()
printSongsByGenreNames [] _ = return ()
printSongsByGenreNames (x:xs) i = do
        putStrLn ("(" ++ show i ++ ") " ++ show (trackNameSong x) ++ " by: " ++ show (artistNameSong x) ++ " from Collection " ++ show (collectionNameSong x))
        printSongsByGenreNames xs (i + 1)

{- | Prints the Track Name, Artist Name and Collection Name from UserPlaylistValues datatype using the printSongsByGenreNames function -}
printSongsInPlaylist :: Connection -> [UserPlaylistValues] -> IO()
printSongsInPlaylist _ [] = return ()
printSongsInPlaylist conn (x:xs) = do
        resultForPlaylist <- querySongsForPlaylist conn (fk_FROMENTRIES x)
        printSongsByGenreNames resultForPlaylist (id_USERPLAYLISTID x)
        printSongsInPlaylist conn xs --(id_USERPLAYLISTID x)

{- | Prints the Track Name, Artist Name and Collection Name from ForPlaylistAdd datatype -}
printSongsByArtistForPlaylist :: [ForPlaylistAdd] -> Int -> IO()
printSongsByArtistForPlaylist [] _ = return ()
printSongsByArtistForPlaylist (x:xs) i = do
        putStrLn ("(" ++ show (idForList x) ++ ") " ++ show (trackNameForList x) ++ " by: " ++ show (artistNameForList x) ++ " from Collection " ++ show (collectionNameForList x))
        printSongsByArtistForPlaylist xs (i + 1)