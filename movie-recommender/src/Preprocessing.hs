{-# LANGUAGE OverloadedStrings #-}

module Preprocessing( preprocessText, preprocessMovie, englishStopWords) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isAlpha, isSpace)
import Data.Set (Set)
import qualified Data.Set as Set
import Types( Movie(..), ProcessedMovie(..))

--список английских стоп-слов
englishStopWords :: Set Text
englishStopWords = Set.fromList
    [ "a", "an", "the", "and", "or", "but", "in", "on", "at", "to", "for"
    , "of", "with", "by", "from", "up", "about", "into", "over", "after"
    , "is", "are", "was", "were", "be", "been", "being", "have", "has", "had"
    , "do", "does", "did", "will", "would", "could", "should", "may", "might"
    , "must", "shall", "can", "need", "dare", "ought", "used"
    , "i", "you", "he", "she", "it", "we", "they", "me", "him", "her", "us", "them"
    , "my", "your", "his", "her", "its", "our", "their"
    , "this", "that", "these", "those", "what", "which", "who", "whom", "whose"
    , "where", "when", "why", "how", "all", "any", "both", "each", "few"
    , "more", "most", "other", "some", "such", "no", "nor", "not", "only"
    , "own", "same", "so", "than", "too", "very", "just", "also"
    ]

-- | Основная функция предобработки одного текста
preprocessText :: Text -> [Text]
preprocessText = filter (not . T.null)
               . filter (`Set.notMember` englishStopWords)
               . map T.toLower
               . T.words
               . T.map replacePunctuation
    where
        replacePunctuation c
            | isAlpha c || isSpace c = c
            | otherwise              = ' '

-- | Предобработка всего фильма
preprocessMovie :: Movie -> ProcessedMovie
preprocessMovie m = ProcessedMovie
    {pMovieId       = movieId m,
     pOriginalTitle = originalTitle m,
     pTokens        = preprocessText (overview m),
     pGenres        = splitCommaSeparated (genres m),
     pKeywords      = splitCommaSeparated (keywords m),
     pPopularity    = popularity m
    }
    where
        splitCommaSeparated :: Text -> [Text]
        splitCommaSeparated = filter (not . T.null) . map T.strip . T.splitOn ","