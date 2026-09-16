{-# LANGUAGE OverloadedStrings #-}

module Preprocessing(preprocessText, preprocessMovie, englishStopWords) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum)
import Data.Set (Set)
import qualified Data.Set as Set
import Types (Movie(..), ProcessedMovie(..))

englishStopWords :: Set Text
englishStopWords = Set.fromList
    ["a", "an", "the", "and", "or", "but", "in", "on", "at", "to", "for",
    "of", "with", "by", "from", "up", "about", "into", "over", "after",
    "is", "are", "was", "were", "be", "been", "being", "have", "has", "had",
    "do", "does", "did", "will", "would", "could", "should", "may", "might",
    "must", "shall", "can", "need", "dare", "ought", "used",
    "i", "you", "he", "she", "it", "we", "they", "me", "him", "her", "us", "them",
    "my", "your", "his", "her", "its", "our", "their",
    "this", "that", "these", "those", "what", "which", "who", "whom", "whose",
    "where", "when", "why", "how", "all", "any", "both", "each", "few",
    "more", "most", "other", "some", "such", "no", "nor", "not", "only",
    "own", "same", "so", "than", "too", "very", "just", "also"
    ]

-- предобработка обычного текста (овервьюшка)
preprocessText :: Text -> [Text]
preprocessText = filter meaningful
               . filter (`Set.notMember` englishStopWords)
               . map T.toLower
               . T.words
               . T.map replacePunctuation
    where
      replacePunctuation c
          | isAlphaNum c || isSpace c = c
          | otherwise                 = ' '
      meaningful t = T.length t >= 3

-- простое разбиение строки по пробелам + очистка
splitAndClean :: Text -> [Text]
splitAndClean = filter meaningful
              . filter (`Set.notMember` englishStopWords)
              . map T.toLower
              . T.words
              . T.map replacePunctuation
    where
        replacePunctuation c
            | isAlphaNum c || isSpace c = c
            | otherwise                 = ' '
        meaningful t = T.length t >= 3

-- предобработка фильма
preprocessMovie :: Movie -> ProcessedMovie
preprocessMovie m =
    let
        overviewTokens = preprocessText (overview m)
        genreTokens    = splitAndClean (genres m)
        keywordTokens  = splitAndClean (keywords m)

    -- объединяем все токены.
    -- жанры повторяем 2 раза, чтобы они имели больший вес.
        allTokens = overviewTokens ++ genreTokens ++ genreTokens ++ keywordTokens
    in ProcessedMovie
        {pMovieId = movieId m,
          pOriginalTitle = originalTitle m,
          pTokens = allTokens,
          pGenres = genreTokens,
          pKeywords = keywordTokens,
          pPopularity = popularity m
          }