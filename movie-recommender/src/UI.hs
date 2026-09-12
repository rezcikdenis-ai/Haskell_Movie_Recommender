{-# LANGUAGE OverloadedStrings #-}

module UI(runUI) where

import qualified Data.Text as T
import Data.List (find, isPrefixOf)
import Data.Char (toLower, isSpace)
import System.IO (hFlush, stdout)
import Types (ProcessedMovie(..), TFIDFModel)
import Recommender (recommend, Recommendation(..))
import Logger (logInfo, logWarning, logError)

runUI :: TFIDFModel -> [ProcessedMovie] -> IO ()
runUI model movies = do
    logInfo "Открыт пользовательский интерфейс"
    putStrLn "Movie Recommender System"
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

    logInfo $ "Пользователь выбрал пункт меню: " ++ choice

    case choice of
        "1" -> do
            logInfo "Режим: ручной ввод названий фильмов"
            selected <- inputMoviesManually movies
            showRecommendations model movies selected
            menuLoop model movies

        "2" -> do
            logInfo "Режим: загрузка списка фильмов из файла"
            selected <- loadMoviesFromTextFile movies
            showRecommendations model movies selected
            menuLoop model movies

        "3" -> do
            logInfo "Режим: поиск фильма по названию"
            searchMovie movies
            menuLoop model movies

        "4" -> do
            logInfo "Пользователь завершил работу"
            putStrLn "\nДо свидания!"

        _ -> do
            logWarning $ "Неверный выбор меню: " ++ choice
            putStrLn "Неверный выбор. Попробуйте снова."
            menuLoop model movies

inputMoviesManually :: [ProcessedMovie] -> IO [Int]
inputMoviesManually movies = do
    putStrLn "\nВведите названия фильмов через запятую"
    putStrLn "(например: Avatar, The Dark Knight, Inception)"
    putStr "> "
    hFlush stdout
    line <- getLine

    logInfo $ "Введённые названия: " ++ line

    let titles = map (T.pack . trim) $ splitOn ',' line
        found  = map (findMovieByTitle movies) titles

    mapM_ reportFound (zip titles found)
    let ids = [ mid | Just mid <- found ]
    logInfo $ "Успешно найдено фильмов: " ++ show (length ids)
    pure ids
  where
    reportFound (title, Nothing) = do
        logWarning $ "Фильм не найден: " ++ T.unpack title
        putStrLn $ "  [!] Фильм не найден: " ++ T.unpack title
    reportFound (title, Just mid) = do
        logInfo $ "Найден фильм: " ++ T.unpack title ++ " (id=" ++ show mid ++ ")"
        putStrLn $ "  [+] Найден: " ++ T.unpack title ++ " (id=" ++ show mid ++ ")"

loadMoviesFromTextFile :: [ProcessedMovie] -> IO [Int]
loadMoviesFromTextFile movies = do
    putStr "\nПуть к файлу со списком фильмов: "
    hFlush stdout
    path <- getLine
    logInfo $ "Попытка загрузки списка из файла: " ++ path

    content <- readFile path
    let titles = map (T.pack . trim) $ lines content
        found  = map (findMovieByTitle movies) titles

    mapM_ reportFound (zip titles found)
    let ids = [ mid | Just mid <- found ]
    logInfo $ "Из файла успешно распознано фильмов: " ++ show (length ids)
    pure ids
  where
    reportFound (title, Nothing) = do
        logWarning $ "Фильм не найден: " ++ T.unpack title
        putStrLn $ "  [!] Фильм не найден: " ++ T.unpack title
    reportFound (title, Just mid) = do
        logInfo $ "Найден фильм: " ++ T.unpack title ++ " (id=" ++ show mid ++ ")"
        putStrLn $ "  [+] Найден: " ++ T.unpack title ++ " (id=" ++ show mid ++ ")"

searchMovie :: [ProcessedMovie] -> IO ()
searchMovie movies = do
    putStr "\nВведите часть названия: "
    hFlush stdout
    query <- getLine
    logInfo $ "Поисковый запрос: " ++ query

    let q = map toLower query
        results = take 15
            [ m | m <- movies
                , q `isPrefixOf` map toLower (T.unpack $ pOriginalTitle m)
                  || map toLower (T.unpack $ pOriginalTitle m) `contains` q
            ]

    if null results
        then do
            logWarning $ "По запросу ничего не найдено: " ++ query
            putStrLn "Ничего не найдено."
        else do
            logInfo $ "Найдено результатов: " ++ show (length results)
            putStrLn "\nНайденные фильмы:"
            mapM_ (\m -> putStrLn $ "  " ++ show (pMovieId m) ++ " | " ++ T.unpack (pOriginalTitle m)) results
  where
    contains s sub = any (isPrefixOf sub) (tails s)
    tails [] = []
    tails xs@(_:xs') = xs : tails xs'

showRecommendations :: TFIDFModel -> [ProcessedMovie] -> [Int] -> IO ()
showRecommendations model movies selectedIds =
    if null selectedIds
        then do
            logWarning "Рекомендации не запрошены — список фильмов пуст"
            putStrLn "\nНе выбрано ни одного фильма."
        else do
            logInfo $ "Запрос рекомендаций для movieId: " ++ show selectedIds
            let recs = recommend model movies selectedIds 10
            putStrLn "\n†††Рекомендации†††"
            if null recs
                then do
                    logWarning "Рекомендации не найдены"
                    putStrLn "Рекомендации не найдены."
                else do
                    logInfo $ "Выдано рекомендаций: " ++ show (length recs)
                    mapM_ printRec (zip [1..] recs)
  where
    printRec (i, r) =
        putStrLn $ show i ++ ". "
                ++ T.unpack (recTitle r)
                ++ "  (" ++ show (round $ recScore r * 100) ++ "%)"
                ++ "  [id=" ++ show (recMovieId r) ++ "]"

findMovieByTitle :: [ProcessedMovie] -> T.Text -> Maybe Int
findMovieByTitle movies title =
    let t = T.toLower (T.strip title)
    in pMovieId <$> find (\m -> T.toLower (pOriginalTitle m) == t) movies

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