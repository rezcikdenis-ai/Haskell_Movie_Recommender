{-# LANGUAGE OverloadedStrings #-}
module Recommender where
import qualified Data.Map.Strict as Map
import Data.List (sortOn)
import Data.Ord (Down(..))
import Data.Text (Text)
import Types (ProcessedMovie(..), SparseVector, TFIDFModel(..))
import TFIDFVectorizer (cosineSimilarity)

-- | Одна рекомендация
data Recommendation = Recommendation
  {recMovieId    :: Int,
   recTitle      :: Text,
   recScore      :: Double
  } deriving (Show)

-- | получение топа рекомендаций по списку фильмов
recommend :: TFIDFModel -> [ProcessedMovie] -> [Int] -> Int -> [Recommendation]
recommend model allMovies selectedIds topN
    | null selectedIds = []
    | otherwise =
        let
            vecs = vectors model
            selectedVecs = [ v | mid <- selectedIds, Just v <- [Map.lookup mid vecs] ]
            userVector = averageVectors selectedVecs
            -- остальные фильмы
            candidates = Map.toList $ Map.filterWithKey (\k _ -> k `notElem` selectedIds) vecs
            -- подсчёт сходства
            scored = [ (mid, cosineSimilarity userVector v) | (mid, v) <- candidates ]
            -- сортировка по убывнию
            sorted = sortOn (Down . snd) scored
            -- берём топ-10 с названиями
            titleMap = Map.fromList [ (pMovieId m, pOriginalTitle m) | m <- allMovies ]
            recommendations =
                [ Recommendation mid (Map.findWithDefault "Unknown" mid titleMap) score
                | (mid, score) <- take topN sorted
                ]
        in recommendations

-- | Усреднение sparse-векторов
averageVectors :: [SparseVector] -> SparseVector
averageVectors [] = Map.empty
averageVectors vs =
    let n = fromIntegral (length vs) :: Double
        summed = foldl (Map.unionWith (+)) Map.empty vs
    in Map.map (/ n) summed