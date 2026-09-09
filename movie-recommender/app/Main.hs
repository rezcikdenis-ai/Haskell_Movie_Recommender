{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Vector as V
import CSVLoader (loadMoviesFromFile)
import Preprocessing (preprocessMovie)
import Types (ProcessedMovie(..))

main :: IO ()
main = do
    putStrLn "Movie Recommender"
    
    result <- loadMoviesFromFile "movies.csv"
    
    case result of
        Left err -> putStrLn $ "Ошибка загрузки: " ++ err
        Right movies -> do
            putStrLn $ "Загружено фильмов: " ++ show (V.length movies)
            
            let processed = V.map preprocessMovie movies
            putStrLn $ "После предобработки: " ++ show (V.length processed)
            
            -- вывод первых 2 фильмов для проверки
            V.forM_ (V.take 2 processed) $ \m -> do
                putStrLn $ "ID: " ++ show (pMovieId m)
                putStrLn $ "Title: " ++ show (pOriginalTitle m)
                putStrLn $ "Tokens (первые 10): " ++ show (take 10 $ pTokens m)