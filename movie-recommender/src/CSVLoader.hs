{-# LANGUAGE OverloadedStrings #-}

module CSVLoader( loadMovies, loadMoviesFromFile) where

import qualified Data.ByteString.Lazy as BL
import Data.Csv (decodeByName)
import qualified Data.Vector as V
import Types (Movie)
import System.IO.Error (tryIOError)

loadMovies :: BL.ByteString -> Either String (V.Vector Movie)
loadMovies csvData =
    case decodeByName csvData of
        Left err -> Left err
        Right (_, v) -> Right v
                
loadMoviesFromFile :: FilePath -> IO (Either String (V.Vector Movie))
loadMoviesFromFile path = do
    result <- tryIOError (BL.readFile path)
    case result of
        Left e  -> pure $ Left $ "Ошибка чтения файла: " ++ show e
        Right bs -> pure $ loadMovies bs