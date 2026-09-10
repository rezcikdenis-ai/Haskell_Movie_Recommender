{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Vector as V
import CSVLoader (loadMoviesFromFile)
import Preprocessing (preprocessMovie)
import TFIDFVectorizer (buildTFIDFModel)
import ModelEvaluation (evaluateModel, EvaluationMetrics(..))
import Types

main :: IO ()
main = do
    putStrLn "Movie Recommender (проверка 4 этапа)"
    result <- loadMoviesFromFile "movies.csv"
    case result of
          Left err -> putStrLn $ "Ошибка: " ++ err
          Right moviesV -> do
            let movies = V.toList moviesV
                processed = map preprocessMovie movies

            putStrLn $ "Загружено фильмов: " ++ show (length processed)

            let model = buildTFIDFModel processed
                metrics = evaluateModel processed model

            putStrLn "\nМетрики модели"
            putStrLn $ "Размер словаря:           " ++ show (vocabularySize metrics)
            putStrLn $ "Покрытие датасета:        " ++ show (coverage metrics * 100) ++ "%"
            putStrLn $ "Средняя разреженность:    " ++ show (avgSparsity metrics)
            putStrLn $ "Среднее сходство в жанре: " ++ show (avgIntraGenreSimilarity metrics)
            putStrLn "\nОтладка genres"
            mapM_ printGenreDebug (take 5 processed)
            where
                printGenreDebug m = do
                    putStrLn $ "Title: " ++ show (pOriginalTitle m)
                    putStrLn $ "Genres: " ++ show (pGenres m)
                    putStrLn $ "Keywords (первые 5): " ++ show (take 5 $ pKeywords m)