{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.QuickCheck
import Test.QuickCheck.Instances.Text ()
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.List (nub, sort)
import qualified Data.Set as Set
import Preprocessing (preprocessText, englishStopWords)
import TFIDFVectorizer (cosineSimilarity, buildTFIDFModel)
import Types (ProcessedMovie(..), SparseVector, TFIDFModel(..))
import Recommender (recommend, Recommendation(..))

-- Генераторы

-- генератор «разумных» токенов
genToken :: Gen T.Text
genToken = T.pack <$> listOf1 (elements ['a'..'z'])

genTokens :: Gen [T.Text]
genTokens = listOf genToken

-- генератор sparse-вектора
genSparseVector :: Gen SparseVector
genSparseVector = Map.fromList <$> listOf ((,) <$> choose (0, 100) <*> choose (0.01, 5.0))

-- простой ProcessedMovie
genProcessedMovie :: Gen ProcessedMovie
genProcessedMovie = do
    mid   <- choose (1, 10000)
    title <- T.pack <$> listOf1 (elements (['a'..'z'] ++ [' ']))
    tokens <- genTokens
    genres <- listOf genToken
    pure $ ProcessedMovie mid title tokens genres [] 1.0

-- Properties: Preprocessing

-- после предобработки не должно оставаться стоп-слов
prop_noStopWords :: String -> Bool
prop_noStopWords s =
    all (`Set.notMember` englishStopWords) (preprocessText (T.pack s))

-- все токены в нижнем регистре
prop_allLowercase :: String -> Bool
prop_allLowercase s =
    all (\t -> t == T.toLower t) (preprocessText (T.pack s))

-- нет пустых токенов
prop_noEmptyTokens :: String -> Bool
prop_noEmptyTokens s =
    all (not . T.null) (preprocessText (T.pack s))

-- короткие токены (длина < 3) отфильтрованы
prop_minTokenLength :: String -> Bool
prop_minTokenLength s =
    all (\t -> T.length t >= 3) (preprocessText (T.pack s))

-- Properties: Cosine Similarity

-- сходство вектора с самим собой = 1 (если вектор ненулевой)
prop_cosineSelf :: SparseVector -> Property
prop_cosineSelf v =
    let norm = sqrt $ sum [x * x | x <- Map.elems v]
    in norm > 1e-9 ==> abs (cosineSimilarity v v - 1.0) < 1e-9

-- сходство симметрично
prop_cosineSymmetric :: SparseVector -> SparseVector -> Bool
prop_cosineSymmetric v1 v2 =
    abs (cosineSimilarity v1 v2 - cosineSimilarity v2 v1) < 1e-9

--сходство всегда в диапазоне [0, 1] (для неотрицательных TF-IDF)
prop_cosineRange :: SparseVector -> SparseVector -> Property
prop_cosineRange v1 v2 =
    all (>= 0) (Map.elems v1) && all (>= 0) (Map.elems v2) ==>
        let s = cosineSimilarity v1 v2
        in s >= -1e-9 && s <= 1.0 + 1e-9

-- нулевой вектор -> сходство 0
prop_cosineZero :: SparseVector -> Bool
prop_cosineZero v =
    cosineSimilarity Map.empty v == 0.0
    && cosineSimilarity v Map.empty == 0.0

-- Properties: Recommend

-- рекомендации не содержат выбранные фильмы
prop_recommendExcludesSelected :: [ProcessedMovie] -> [Int] -> Property
prop_recommendExcludesSelected movies selected =
    not (null movies) ==>
        let model = buildTFIDFModel movies
            recs  = recommend model movies selected 10
            recIds = map recMovieId recs
        in all (`notElem` selected) recIds

--количество рекомендаций ≤ topN
prop_recommendLimit :: [ProcessedMovie] -> Property
prop_recommendLimit movies =
    not (null movies) ==>
        let model = buildTFIDFModel movies
            selected = [pMovieId (head movies)]
            recs = recommend model movies selected 5
        in length recs <= 5

-- запуск
main :: IO ()
main = do
    putStrLn "QuickCheck: Preprocessing"
    quickCheck prop_allLowercase
    quickCheck prop_noEmptyTokens
    quickCheck prop_minTokenLength

    putStrLn "\nQuickCheck: Cosine Similarity"
    quickCheck (forAll genSparseVector prop_cosineSelf)
    quickCheck (forAll genSparseVector $ \v1 ->
                forAll genSparseVector $ \v2 ->
                    prop_cosineSymmetric v1 v2)
    quickCheck (forAll genSparseVector $ \v1 ->
                forAll genSparseVector $ \v2 ->
                    prop_cosineRange v1 v2)
    quickCheck (forAll genSparseVector prop_cosineZero)

    putStrLn "\nQuickCheck: Recommend"
    quickCheck (forAll (listOf1 genProcessedMovie) prop_recommendLimit)

    putStrLn "\nВсе тесты завершены."