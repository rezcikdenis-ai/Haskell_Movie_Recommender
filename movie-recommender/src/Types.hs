{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Csv (FromNamedRecord(..), (.:))
import Control.Applicative ((<|>))

-- | Основная запись о фильме (только нужные нам поля)
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

-- | Фильм после предобработки
data ProcessedMovie = ProcessedMovie
    {pMovieId       :: Int,
     pOriginalTitle :: Text,
     pTokens        :: [Text],
     pGenres        :: [Text],
     pKeywords      :: [Text],
     pPopularity    :: Double
    } deriving (Show, Eq)