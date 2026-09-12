{-# LANGUAGE OverloadedStrings #-}

module ModelPersistence where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Csv
import qualified Data.Vector as V
import System.Directory (doesFileExist)
import Types (TFIDFModel(..), SparseVector)
import Data.List (intercalate)

-- вспомогательные типы для сериализации

data VocabRow = VocabRow
  {vTerm  :: T.Text,
   vIndex :: Int
  } deriving (Show)

instance FromNamedRecord VocabRow where
    parseNamedRecord r = VocabRow <$> r .: "term" <*> r .: "index"

instance ToNamedRecord VocabRow where
    toNamedRecord (VocabRow t i) = namedRecord ["term" .= t, "index" .= i]

instance DefaultOrdered VocabRow where
    headerOrder _ = header ["term", "index"]

data IdfRow = IdfRow
  {iIndex :: Int,
   iValue :: Double
  } deriving (Show)

instance FromNamedRecord IdfRow where
    parseNamedRecord r = IdfRow <$> r .: "index" <*> r .: "idf"

instance ToNamedRecord IdfRow where
    toNamedRecord (IdfRow i v) = namedRecord ["index" .= i, "idf" .= v]

instance DefaultOrdered IdfRow where
    headerOrder _ = header ["index", "idf"]

data VectorRow = VectorRow
  {vecMovieId :: Int,
   vecData    :: T.Text 
  } deriving (Show)

instance FromNamedRecord VectorRow where
    parseNamedRecord r = VectorRow <$> r .: "movie_id" <*> r .: "vector"

instance ToNamedRecord VectorRow where
    toNamedRecord (VectorRow mid v) = namedRecord ["movie_id" .= mid, "vector" .= v]

instance DefaultOrdered VectorRow where
    headerOrder _ = header ["movie_id", "vector"]

-- сохранение модели
saveModel :: FilePath -> TFIDFModel -> IO ()
saveModel dir model = do
    let vocabRows = [ VocabRow term idx | (term, idx) <- Map.toList (vocabulary model) ]
    BL.writeFile (dir ++ "/vocabulary.csv") $ encodeByName (headerOrder (undefined :: VocabRow)) vocabRows

    let idfRows = [ IdfRow idx val | (idx, val) <- Map.toList (idf model) ]
    BL.writeFile (dir ++ "/idf.csv") $ encodeByName (headerOrder (undefined :: IdfRow)) idfRows

    let vectorRows = [VectorRow mid (encodeSparse v) | (mid, v) <- Map.toList (vectors model)]
    BL.writeFile (dir ++ "/vectors.csv") $ encodeByName (headerOrder (undefined :: VectorRow)) vectorRows

    putStrLn $ "Модель сохранена в " ++ dir

-- кодирование sparse-вектора в строку
encodeSparse :: SparseVector -> T.Text
encodeSparse v =
    T.pack $ intercalate " " [ show idx ++ ":" ++ show val | (idx, val) <- Map.toList v ]

-- декодирование строки обратно в sparse-вектор
decodeSparse :: T.Text -> SparseVector
decodeSparse txt =
    Map.fromList
        [(read (T.unpack idx), read (T.unpack val))
        | part <- T.words txt,
        let (idx, rest) = T.break (== ':') part,
        not (T.null rest),
        let val = T.drop 1 rest
        ]

-- загрузка модели
loadModel :: FilePath -> IO (Either String TFIDFModel)
loadModel dir = do
    vocabExists <- doesFileExist (dir ++ "/vocabulary.csv")
    idfExists   <- doesFileExist (dir ++ "/idf.csv")
    vecExists   <- doesFileExist (dir ++ "/vectors.csv")

    if not (vocabExists && idfExists && vecExists)
        then pure $ Left "Файлы модели не найдены"
        else do
        vocabBs <- BL.readFile (dir ++ "/vocabulary.csv")
        idfBs   <- BL.readFile (dir ++ "/idf.csv")
        vecBs   <- BL.readFile (dir ++ "/vectors.csv")

        pure $ do
            (_, vocabV) <- decodeByName vocabBs
            (_, idfV)   <- decodeByName idfBs
            (_, vecV)   <- decodeByName vecBs

            let vocabMap = Map.fromList [ (vTerm r, vIndex r) | r <- V.toList vocabV ]
                idfMap   = Map.fromList [ (iIndex r, iValue r) | r <- V.toList idfV ]
                vecMap   = Map.fromList [ (vecMovieId r, decodeSparse (vecData r)) | r <- V.toList vecV ]

            pure $ TFIDFModel vocabMap idfMap vecMap