{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Csv (FromNamedRecord(..), (.:))
import qualified Data.Map as Map
import Control.Applicative ((<|>))

-- основная запись о фильме
data Movie = Movie
    {movieId :: Int,
     originalTitle :: Text,
     overview :: Text,
     keywords :: Text,
     genres :: Text,
     popularity :: Double
    } deriving (Show, Eq, Generic)

instance FromNamedRecord Movie where
    parseNamedRecord r =
        Movie
            <$> r .: "id"
            <*> r .: "original_title"
            <*> (r .: "overview" <|> pure "")          -- на случай пустого overview
            <*> (r .: "keywords" <|> pure "")
            <*> (r .: "genres"   <|> pure "")
            <*> r .: "popularity"

-- фильм после предобработки
data ProcessedMovie = ProcessedMovie
    {pMovieId :: Int,
     pOriginalTitle :: Text,
     pTokens :: [Text],
     pGenres :: [Text],
     pKeywords :: [Text],
     pPopularity :: Double
    } deriving (Show, Eq)

type SparseVector = Map.Map Int Double

-- обученная TF-IDF модель
data TFIDFModel = TFIDFModel
    {vocabulary :: Map.Map Text Int,      -- слово в индекс
     idf :: Map.Map Int Double,    -- индекс в IDF
     vectors    :: Map.Map Int SparseVector  -- movieId в вектор
    } deriving (Show)