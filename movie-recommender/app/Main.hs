{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Vector as V
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)

import CSVLoader (loadMoviesFromFile) 
import Preprocessing (preprocessMovie)
import TFIDFVectorizer (buildTFIDFModel)
import ModelEvaluation (evaluateModel, EvaluationMetrics(..))
import ModelPersistence (saveModel, loadModel)
import UI (runUI)
import Types

main :: IO ()
main = do
    putStrLn "Movie Recommender"

    let modelDir = "model_cache"
        csvPath  = "movies.csv"

    moviesResult <- loadMoviesFromFile csvPath

    case moviesResult of
        Left err -> putStrLn $ "Ошибка загрузки CSV: " ++ err
        Right moviesV -> do
          let movies    = V.toList moviesV
              processed = map preprocessMovie movies

          cacheExists <- doesDirectoryExist modelDir
          modelResult <- if cacheExists
                        then loadModel modelDir
                        else pure $ Left "no cache"

          model <- case modelResult of
              Right m -> do
                putStrLn "Модель загружена из кеша"
                pure m
              Left _ -> do
                  putStrLn "Строим модель..."
                  let newModel = buildTFIDFModel processed
                  createDirectoryIfMissing True modelDir
                  saveModel modelDir newModel
                  let metrics = evaluateModel processed newModel
                  putStrLn $ "Словарь: " ++ show (vocabularySize metrics)
                        ++ " | Сходство в жанре: " ++ show (avgIntraGenreSimilarity metrics)
                  pure newModel 
          runUI model processed