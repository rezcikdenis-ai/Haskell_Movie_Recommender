{-# LANGUAGE OverloadedStrings #-}

module ModelEvaluation
  ( evaluateModel
  , EvaluationMetrics(..)
  ) where

import qualified Data.Map.Strict as Map
import Data.List (foldl')
import Types (ProcessedMovie(..), SparseVector, TFIDFModel(..))
import TFIDFVectorizer (cosineSimilarity)

data EvaluationMetrics = EvaluationMetrics
    {avgIntraGenreSimilarity :: Double,
     avgSparsity :: Double,
     coverage :: Double,
     vocabularySize :: Int
    } deriving (Show)

evaluateModel :: [ProcessedMovie] -> TFIDFModel -> EvaluationMetrics
evaluateModel movies model =
    let
        vecs = vectors model
        nMovies = length movies
        nWithVectors = Map.size vecs

        cov = if nMovies == 0 then 0 else fromIntegral nWithVectors / fromIntegral nMovies

        vocabSize = Map.size (vocabulary model)
        sparsities = [fromIntegral (Map.size v) / fromIntegral vocabSize | v <- Map.elems vecs , vocabSize > 0]
        avgSpar = if null sparsities then 0 else sum sparsities / fromIntegral (length sparsities)

        genreSims = computeIntraGenreSimilarities movies vecs
        avgGenreSim = if null genreSims then 0 else sum genreSims / fromIntegral (length genreSims)

    in EvaluationMetrics
        {avgIntraGenreSimilarity = avgGenreSim,
         avgSparsity = avgSpar,
         coverage = cov,
         vocabularySize = vocabSize
        }

-- среднее сходство фильмов жанра с центроидом жанра
computeIntraGenreSimilarities :: [ProcessedMovie] -> Map.Map Int SparseVector -> [Double]
computeIntraGenreSimilarities movies vecs =
    let
        -- Группируем movieId по жанрам
        genreToIds = foldl' addGenres Map.empty movies
            where
                addGenres acc m = foldl' (\a g -> Map.insertWith (++) g [pMovieId m] a) acc (pGenres m)

        -- Для каждого жанра считаем среднее сходство с центроидом
        sims = map (avgSimilarityToCentroid vecs) (Map.elems genreToIds)
    in filter (> 0) sims

-- считаем центроид и среднее косинусное сходство фильмов к нему
avgSimilarityToCentroid :: Map.Map Int SparseVector -> [Int] -> Double
avgSimilarityToCentroid vecs ids
    | length ids < 2 = 0.0
    | otherwise =
        let
            -- собираем векторы жанра
            genreVecs = [ v | mid <- ids, Just v <- [Map.lookup mid vecs] ]

            -- центроид = средний вектор
            centroid = averageVectors genreVecs

            -- сходство каждого фильма с центроидом
            scores = map (`cosineSimilarity` centroid) genreVecs
        in if null scores then 0 else sum scores / fromIntegral (length scores)

averageVectors :: [SparseVector] -> SparseVector
averageVectors [] = Map.empty
averageVectors vs =
    let
        n = fromIntegral (length vs) :: Double
        summed = foldl' (Map.unionWith (+)) Map.empty vs
    in Map.map (/ n) summed