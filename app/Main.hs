{- |
   Module     : Main

   Maintainer : Shashank Singh <ec22267@qmul.ac.uk>

   The purpose of this application is to Download new data from iTunes URL and sotre it in a SQLite databse. 
   Main Functionality is as Follows: 
        - The user has an option to download using Artist Name and/or Genre Type
        - Additional features are to list all the Genres and Artist 
        - As well as search and list all the songs by a particular Genre
        - Also, the user can perform operations like Add, Delete and View a Playlist

   Written by Shashank Singh, Naman Shrivastava, Vishal and Ziyad Sheikh
-}

module Main (main) where

import System.IO
import Database
import CustomMethodsCreated
import System.IO.Error

{- 
    Options below are for adding the details to the database.
    A lots of exceptions are being handled here!  
-}
-- | Options below are for adding the details to the database.
-- | A lots of exceptions are being handled here!  

main :: IO ()
main = do
    conn <- initialiseDB

    putStrLn "--------------------------------------"
    putStrLn "  Welcome to the Music app            "
    putStrLn "  (1) Get New data by Genre or Artist "
    putStrLn "  (2) List all the available Genres   "
    putStrLn "  (3) List all the available Artists  "
    putStrLn "  (4) Search Songs by available Genre "
    putStrLn "  (5) Playlist Operation              "
    putStrLn "  (6) Quit Application                "
    putStrLn "--------------------------------------"
    
    hSetBuffering stdout NoBuffering
    putStrLn "Choose an option : "
    option <- tryIOError (readLn :: IO Int)
    case option of
        Left e -> do
            putStrLn ("Warning!!!! Error: " ++ show e)
            putStrLn "Invalid option: Please select a valid option from below menu"
            main
        Right x -> case x of
            1 -> do
                putStrLn "-----------------------------"
                putStrLn "  Get Data by?               "
                putStrLn "  (1) Genre                  "
                putStrLn "  (2) Artist                 "
                putStrLn "  (3) Return to Main Menu    "
                putStrLn "  (4) Quit                   "
                putStrLn "-----------------------------"
                putStrLn "Choose an option : "
                option2 <- tryIOError (readLn :: IO Int)
                case option2 of
                    Left e2 -> do
                        putStrLn ("Warning!!!! Error: " ++ show e2)
                        putStrLn "Invalid option: Please select a valid option from below menu"
                        main
                    Right x2 -> case x2 of
                        1 -> do
                            putStrLn "Enter Genre Name you want to search by:   "
                            genreNameValue <- getLine :: IO String
                            putStrLn ("Fetching Data for " ++ genreNameValue ++ " ...")
                            let url = "https://itunes.apple.com/search?term=" ++ genreNameValue ++ "&primaryGenreName=" ++ genreNameValue
                            --let url = "https://www.google.com/"
                            --let url = "https://itunes.apple.com/search?term=askjxskjbckjadshbfkadshf&primaryGenreName=askjxskjbckjadshbfkadshf"
                            if null url
                                then do
                                    putStrLn ("Invalid URL. URL cannot be empty!!" :: String)
                            else do
                                fetchAndParseData conn url
                            main
                        2 -> do
                            putStrLn "Enter the Artist's Name you want to search by:   "
                            artistNameValue <- getLine :: IO String
                            let url = "https://itunes.apple.com/search?term=" ++ artistNameValue ++ "&entity=song"
                            --to check for invalid URL
                            --let url = "xxxxxxxxxxxxxxxxxxxxxx"
                            --let url = "https://dribbble.com/hfghjdsavhdfijlhdfskl"
                            if null url
                                then do
                                    putStrLn ("Invalid URL. URL cannot be empty!!" :: String)
                            else do
                                fetchAndParseData conn url
                            main
                        3 -> do
                            main
                        4 -> putStrLn "Hope you've enjoyed using the Music App!"
                        _ -> do
                            putStrLn "Warning!!!!"
                            putStrLn "Invalid option: Please select a valid option from below menu"
                            main
            2 -> do
                entries <- queryGetAllGenres conn
                let lengthValue = length entries
                if lengthValue == 0
                    then do
                        putStrLn "No entries currently Available, try downloading using option #1"
                else do
                    putStrLn ("Total entries found: " ++ show(lengthValue :: Int))
                    putStrLn "All the available Genres are: "
                    printAllGenreNames entries 1
                main
            3 -> do
                entries <- queryGetAllArtists conn
                let lengthValue = length entries
                if lengthValue == 0
                    then do
                        putStrLn "No enrties currently Available, try downloading using option #1"
                else do
                    putStrLn ("Total entries found: " ++ show(lengthValue :: Int))
                    putStrLn "All the available Genres are: "
                    printAllArtistNames entries 1
                main
            4 -> do
                putStrLn "Enter Genre Name you want to search by:   "
                genreNameValue <- getLine :: IO String
                entries <- querySearchByAvailGenre conn genreNameValue
                let lengthValue = length entries
                if lengthValue == 0
                    then do
                        putStrLn ("No enrties found for " ++ show(genreNameValue :: String) ++ ", try downloading using option #1")
                else do
                    putStrLn ("Total entries found: " ++ show(lengthValue :: Int))
                    putStrLn ("Values found for " ++ show(genreNameValue :: String) ++ " are: ")
                    printSongsByGenreNames entries 1
                main
            5 -> do
                putStrLn "----------------------------------"
                putStrLn " Playlist Operations Available    "
                putStrLn " (1) View Playlist                "
                putStrLn " (2) Add Songs to Playlist        "
                putStrLn " (3) Delete Songs from Playlist   "
                putStrLn " (4) Return to Main Menu          "
                putStrLn " (5) Quit                         "
                putStrLn "----------------------------------"
                putStrLn "Choose an option : "
                option2 <- tryIOError (readLn :: IO Int)
                case option2 of
                    Left e2 -> do
                        putStrLn ("Warning!!!! Error: " ++ show e2)
                        putStrLn "Invalid option: Please select a valid option from below menu"
                        main
                    Right x2 -> case x2 of
                        1 -> do
                            entries <- queryPlaylistGetValues conn
                            let lengthValue = length entries
                            if lengthValue == 0
                                then do
                                    putStrLn ("The Playlist is currently Empty. Moving to main menu..." :: String)
                            else do
                                putStrLn ("Total entries found: " ++ show(lengthValue :: Int))
                                putStrLn ("Songs in the Playlist are: " :: String)
                                printSongsInPlaylist conn entries
                            main
                        2 -> do
                            putStrLn "Enter Artist Name you want to search by:   "
                            artistNameValue <- getLine :: IO String
                            entries <- querySearchByArtistName conn artistNameValue
                            let lengthValue = length entries
                            if lengthValue == 0
                                then do
                                    putStrLn ("No enrties found for " ++ show(artistNameValue :: String) ++ ", try downloading using option #1")
                            else do
                                putStrLn ("Total entries found: " ++ show(lengthValue :: Int))
                                putStrLn ("Values found for " ++ show(artistNameValue :: String) ++ " are: ")
                                printSongsByArtistForPlaylist entries 1
                                putStrLn ("Select the serial number of the song that you want to Add to Playlist : " :: String)
                                option3 <- tryIOError (readLn :: IO Int)
                                case option3 of
                                    Left e2 -> do
                                        putStrLn ("Warning!!!! Error: " ++ show e2)
                                        putStrLn "Invalid option: Please select a valid option from below menu"
                                        main
                                    Right x3 -> do
                                        queryAddSongToPlaylist conn x3 artistNameValue
                            main
                        3 -> do
                            entries <- queryPlaylistGetValues conn
                            let lengthValue = length entries
                            if lengthValue == 0
                                then do
                                    putStrLn ("The Playlist is currently Empty. Moving to main menu..." :: String)
                            else do
                                putStrLn ("Songs in the Playlist are: " :: String)
                                printSongsInPlaylist conn entries
                                putStrLn ("Select the serial number of the song that you want to Delete from Playlist : " :: String)
                                option3 <- tryIOError (readLn :: IO Int)
                                case option3 of
                                    Left e2 -> do
                                        putStrLn ("Warning!!!! Error: " ++ show e2)
                                        putStrLn "Invalid option: Please select a valid option from below menu"
                                        main
                                    Right x3 -> do
                                        queryDeleteSongFromPlaylist conn x3
                            main
                        4 -> do
                            main
                        5 -> putStrLn "Hope you've enjoyed using the Music App!"
                        _ -> do
                            putStrLn "Warning!!!!"
                            putStrLn "Invalid option: Please select a valid option from below menu"
                            main
            6 -> putStrLn "Hope you've enjoyed using the Music App!"
            _ -> do
                putStrLn "Warning!!!!"
                putStrLn "Invalid option: Please select a valid option from below menu"
                main
