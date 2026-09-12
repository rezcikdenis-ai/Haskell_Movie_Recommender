{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Vector as V
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)

import CSVLoader (loadMoviesFromFile)
import Preprocessing (preprocessMovie)
import TFIDFVectorizer( buildTFIDFModelFromSplit, splitDataset, DatasetSplit(..))
import ModelEvaluation (evaluateModel, EvaluationMetrics(..))
import ModelPersistence (saveModel, loadModel)
import UI (runUI)
import Logger (logInfo, logError, logWarning, withLogging)
import Types

main :: IO ()
main = do
    logInfo "Запуск Movie Recommender"

    let modelDir = "model_cache"
        csvPath  = "movies.csv"

    moviesResult <- withLogging "Загрузка CSV" $
        loadMoviesFromFile csvPath

    case moviesResult of
        Left err -> logError $ "Ошибка загрузки CSV: " ++ err
        Right moviesV -> do
            let movies    = V.toList moviesV
                processed = map preprocessMovie movies

            logInfo $ "Загружено фильмов: " ++ show (length processed)

            let split = splitDataset processed
            logInfo $ "Train: " ++ show (length $ trainSet split)
            logInfo $ "Val:   " ++ show (length $ valSet split)
            logInfo $ "Test:  " ++ show (length $ testSet split)

            cacheExists <- doesDirectoryExist modelDir
            modelResult <- if cacheExists
                               then loadModel modelDir
                               else pure $ Left "Кеш отсутствует"

            model <- case modelResult of
                Right m -> do
                    logInfo "Модель загружена из кеша"
                    pure m
                Left _ -> do
                    logInfo "Кеш не найден, строим модель..."
                    let newModel = buildTFIDFModelFromSplit split

                    createDirectoryIfMissing True modelDir
                    saveModel modelDir newModel
                    logInfo "Модель сохранена в model_cache"

                    let metrics = evaluateModel (valSet split) newModel
                    logInfo $ "Размер словаря: " ++ show (vocabularySize metrics)
                    logInfo $ "Покрытие (val): " ++ show (coverage metrics * 100) ++ "%"
                    logInfo $ "Разреженность (val): " ++ show (avgSparsity metrics)
                    logInfo $ "Сходство в жанре (val): " ++ show (avgIntraGenreSimilarity metrics)
                    pure newModel

            logInfo "Запуск пользовательского интерфейса"
            runUI model processed
            logInfo "Завершение работы"