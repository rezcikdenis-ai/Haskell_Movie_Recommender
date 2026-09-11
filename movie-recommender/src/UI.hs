{-# LANGUAGE OverloadedStrings #-}

module UI where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (find, isPrefixOf)
import Data.Char (toLower, isSpace)
import System.IO (hFlush, stdout)
import Types
import Recommender

-- | Главный цикл интерфейса
runUI :: TFIDFModel -> [ProcessedMovie] -> IO ()
runUI model movies = do
    putStrLn "†††††Movie Recommender System†††††"
    menuLoop model movies

menuLoop :: TFIDFModel -> [ProcessedMovie] -> IO ()
menuLoop model movies = do
    putStrLn "\nВыберите действие:"
    putStrLn "  1. Ввести названия фильмов вручную"
    putStrLn "  2. Загрузить список фильмов из файла"
    putStrLn "  3. Найти фильм по названию (поиск)"
    putStrLn "  4. Выход"
    putStr "\nВаш выбор: "
    hFlush stdout
    choice <- getLine

    case choice of
        "1" -> do
            selected <- inputMoviesManually movies
            showRecommendations model movies selected
            menuLoop model movies

        "2" -> do
            selected <- loadMoviesFromtxt movies
            showRecommendations model movies selected
            menuLoop model movies

        "3" -> do
            searchMovie movies
            menuLoop model movies

        "4" -> putStrLn "\nДо свидания!"

        _ -> do
            putStrLn "Неверный выбор. Попробуйте снова."
            menuLoop model movies

-- | Ручной ввод названий фильмов
inputMoviesManually :: [ProcessedMovie] -> IO [Int]
inputMoviesManually movies = do
    putStrLn "\nВведите названия фильмов через запятую"
    putStrLn "(например: Avatar, The Dark Knight, Inception)"
    putStr "> "
    hFlush stdout
    line <- getLine
    let titles = map (T.pack . trim) $ splitOn ',' line
        found  = map (findMovieByTitle movies) titles

    -- Показываем, что нашлось
    mapM_ reportFound (zip titles found)

    pure [ mid | Just mid <- found ]

    where
        reportFound (title, Nothing) =
            putStrLn $ "  [!] Фильм не найден: " ++ T.unpack title
        reportFound (title, Just mid) =
            putStrLn $ "  [+] Найден: " ++ T.unpack title ++ " (id=" ++ show mid ++ ")"

-- | Загрузка списка фильмов из текстового файла
loadMoviesFromtxt :: [ProcessedMovie] -> IO [Int]
loadMoviesFromtxt movies = do
    putStr "\nПуть к файлу со списком фильмов: "
    hFlush stdout
    path <- getLine
    content <- readFile path
    let titles = map (T.pack . trim) $ lines content
        found  = map (findMovieByTitle movies) titles

    mapM_ reportFound (zip titles found)
    pure [ mid | Just mid <- found ]

    where
        reportFound (title, Nothing) =
            putStrLn $ "  [!] Фильм не найден: " ++ T.unpack title
        reportFound (title, Just mid) =
            putStrLn $ "  [+] Найден: " ++ T.unpack title ++ " (id=" ++ show mid ++ ")"

-- поиск фильма по частичному названию
searchMovie :: [ProcessedMovie] -> IO ()
searchMovie movies = do
    putStr "\nВведите часть названия: "
    hFlush stdout
    query <- getLine
    let q = map toLower query
        results = take 15
            [ m | m <- movies
                , q `isPrefixOf` map toLower (T.unpack $ pOriginalTitle m)
                || map toLower (T.unpack $ pOriginalTitle m) `contains` q
            ]

    if null results
        then putStrLn "Ничего не найдено."
        else do
        putStrLn "\nНайденные фильмы:"
        mapM_ (\m -> putStrLn $ "  " ++ show (pMovieId m) ++ " | " ++ T.unpack (pOriginalTitle m)) results

    where
        contains s sub = sub `elem` words s || any (isPrefixOf sub) (tails s)
        tails [] = []
        tails xs@(_:xs') = xs : tails xs'

-- поиск movieId по названию (без учёта регистра, точное совпадение)
findMovieByTitle :: [ProcessedMovie] -> T.Text -> Maybe Int
findMovieByTitle movies title =
    let t = T.toLower (T.strip title)
    in pMovieId <$> find (\m -> T.toLower (pOriginalTitle m) == t) movies

-- | Вывод рекомендаций
showRecommendations :: TFIDFModel -> [ProcessedMovie] -> [Int] -> IO ()
showRecommendations model movies selectedIds =
    if null selectedIds
        then putStrLn "\nНе выбрано ни одного фильма."
        else do
        let recs = recommend model movies selectedIds 10
        putStrLn "\n========== Рекомендации =========="
        if null recs
            then putStrLn "Рекомендации не найдены."
            else mapM_ printRec (zip [1..] recs)
        putStrLn "=================================="

    where
        printRec (i, r) =
            putStrLn $ show i ++ ". "
                ++ T.unpack (recTitle r)
                ++ "  (" ++ show (round $ recScore r * 100) ++ "%)"
                ++ "  [id=" ++ show (recMovieId r) ++ "]"

-- Вспомогательные функции
trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn delim str =
    let (first, rest) = break (== delim) str
    in first : case rest of
                []     -> []
                (_:xs) -> splitOn delim xs