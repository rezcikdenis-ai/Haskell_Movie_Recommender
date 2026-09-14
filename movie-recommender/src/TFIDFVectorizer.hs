{-# LANGUAGE OverloadedStrings #-}

module TFIDFVectorizer
    (buildTFIDFModelFromSplit, splitDataset, cosineSimilarity, DatasetSplit(..), vectorizeText) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.List (foldl', sortOn)
import Types (ProcessedMovie(..), SparseVector, TFIDFModel(..))
import System.Random (mkStdGen, randomRs)

-- бьём датасет
data DatasetSplit = DatasetSplit
    {trainSet :: [ProcessedMovie],
    valSet :: [ProcessedMovie],
    testSet :: [ProcessedMovie]
    } deriving (Show)


splitDataset :: [ProcessedMovie] -> DatasetSplit
splitDataset movies =
    let
        n      = length movies
        nTrain = round (0.70 * fromIntegral n :: Double)
        nVal   = round (0.15 * fromIntegral n :: Double)

        -- Простое детерминированное «перемешивание» по остатку от деления id
        sorted = sortOn (\m -> pMovieId m `mod` 97) movies

        train = take nTrain sorted
        rest  = drop nTrain sorted
        val   = take nVal rest
        test  = drop nVal rest
    in DatasetSplit
        { trainSet = train
        , valSet   = val
        , testSet  = test
        }

-- построение TF-IDF модели из списка предобработанных фильмов
buildTFIDFModelFromSplit :: DatasetSplit -> TFIDFModel
buildTFIDFModelFromSplit split =
    let
        train = trainSet split
        --словарь из train
        allTokens = concatMap pTokens train
        uniqueTokens = Set.toList $ Set.fromList allTokens
        vocab = Map.fromList $ zip uniqueTokens [0..]

        -- считаем DF
        docFreq = foldl' updateDocFreq Map.empty train
            where
                updateDocFreq acc m =
                    let uniqueInDoc = Set.fromList (pTokens m)
                    in foldl' (\a t -> Map.insertWith (+) t 1 a) acc uniqueInDoc

        nDocs = fromIntegral (length train) :: Double

        -- считаем IDF
        idfMap = Map.fromList
            [ (idx, log (nDocs / (1 + fromIntegral df)))  -- сглаживание +1
            | (term, idx) <- Map.toList vocab
            , let df = Map.findWithDefault 0 term docFreq
            ]

        -- строим TF-IDF векторы
        allMovies = train ++ valSet split ++ testSet split
        vectorsMap = Map.fromList
            [(pMovieId m, computeTFIDF (pTokens m) vocab idfMap) | m <- allMovies]

    in TFIDFModel
        {vocabulary = vocab, 
            idf = idfMap,
            vectors = vectorsMap
        }
--оставил часть старого интерфейса просто для совместимости
buildTFIDFModel :: [ProcessedMovie] -> TFIDFModel
buildTFIDFModel movies =
    let split = DatasetSplit movies [] []
    in buildTFIDFModelFromSplit split

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

--добавление возможности векторизации введённого вручную текста
vectorizeText :: TFIDFModel -> [T.Text] -> SparseVector
vectorizeText model tokens =
    computeTFIDF tokens (vocabulary model) (idf model)