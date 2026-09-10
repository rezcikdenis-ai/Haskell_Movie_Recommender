{-# LANGUAGE OverloadedStrings #-}

module TFIDFVectorizer
  ( buildTFIDFModel
  , cosineSimilarity
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.List (foldl')
import Types (ProcessedMovie(..), SparseVector, TFIDFModel(..))

-- построение TF-IDF модели из списка предобработанных фильмов
buildTFIDFModel :: [ProcessedMovie] -> TFIDFModel
buildTFIDFModel movies =
  let
    -- собираем все уникальные токены в словарь
    allTokens = concatMap pTokens movies
    uniqueTokens = Set.toList $ Set.fromList allTokens
    vocab = Map.fromList $ zip uniqueTokens [0..]

    -- считаем DF
    docFreq = foldl' updateDocFreq Map.empty movies
      where
        updateDocFreq acc m =
          let uniqueInDoc = Set.fromList (pTokens m)
          in foldl' (\a t -> Map.insertWith (+) t 1 a) acc uniqueInDoc

    nDocs = fromIntegral (length movies) :: Double

    -- считаем IDF
    idfMap = Map.fromList
        [ (idx, log (nDocs / (1 + fromIntegral df)))  -- сглаживание +1
        | (term, idx) <- Map.toList vocab
        , let df = Map.findWithDefault 0 term docFreq
        ]

    -- строим TF-IDF векторы
    vectorsMap = Map.fromList
        [ (pMovieId m, computeTFIDF (pTokens m) vocab idfMap)
        | m <- movies
        ]

  in TFIDFModel
       {vocabulary = vocab,
        idf        = idfMap,
        vectors    = vectorsMap
       }

-- вычисление TF-IDF вектора для одного документа
computeTFIDF :: [T.Text] -> Map.Map T.Text Int -> Map.Map Int Double -> SparseVector
computeTFIDF tokens vocab idfMap =
    let
        tf = foldl' (\acc t -> Map.insertWith (+) t 1 acc) Map.empty tokens
        totalTerms = fromIntegral (length tokens) :: Double

        tfidf = Map.fromList
            [ (idx, (fromIntegral count / totalTerms) * idfVal)
            | (term, count) <- Map.toList tf
            , Just idx <- [Map.lookup term vocab]
            , let idfVal = Map.findWithDefault 0.0 idx idfMap
            , idfVal > 0
            ]
    in tfidf

-- косинусная мера сходства
cosineSimilarity :: SparseVector -> SparseVector -> Double
cosineSimilarity v1 v2
    | norm1 == 0 || norm2 == 0 = 0.0
    | otherwise = dot / (norm1 * norm2)
    where
        --скалярное произведение (только по общим ключам)
        commonKeys = Map.keys $ Map.intersection v1 v2
        dot = sum [ Map.findWithDefault 0 k v1 * Map.findWithDefault 0 k v2 | k <- commonKeys ]
        norm1 = sqrt $ sum [ x * x | x <- Map.elems v1 ]
        norm2 = sqrt $ sum [ x * x | x <- Map.elems v2 ]