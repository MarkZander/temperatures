{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC #-}
module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
-- from cassava
import Data.Csv
import qualified Data.Foldable as F
import Data.Time
import System.Environment
import System.Directory
import Data.Maybe
import Data.IntMap.Strict (IntMap)

import Turtle
import qualified Control.Foldl as Fold
import Control.Monad
import qualified Data.Text as Text

import Lib
import TimeMap

-- import Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as Map

type TimeString = String

-- Data as read from each line of the csv temperature file.
type TempData = (TimeString, Maybe Number)
    -- BL.ByteString, BL.ByteString)

-- dateTime :: (t0, t1, t2, t3) -> t0
-- dateTime (x0, _, _, _) = x0

-- temperature :: (t0, t1, t2, t3) -> t1
-- temperature  (_, x1, _, _) = x1

dateTime = fst

temperature = snd

-- foldr :: (a -> b -> b) -> b -> ta -> b
-- Need function a -> b -> b where a is TempTime(?) and b is a TimeMap
-- insertTimeMap mask td m =
--     case timeFromString $ dateTime td of
--         Nothing -> m
--         Just ltim -> let tim = packYMDHm $ fromLocalTime ltim in
--             case temperature td of
--                 Nothing -> m
--                 Just temp -> insert mask tim (makeStats tim temp) m

insertTimeMap :: TimeMask -> TempData -> TimeMap -> TimeMap
insertTimeMap mask td m = fromMaybe m $ do
    ltim <- timeFromString $ dateTime td
    let tim = packYMDHm $ fromLocalTime ltim
    temp <- temperature td
    return $ insert mask tim (makeStats tim temp) m

printVec :: Show a => V.Vector a -> IO ()
printVec v = V.mapM_ print v


makeMap :: TimeMask -> TimeMap -> V.Vector TempData -> TimeMap
makeMap mask m v = V.foldr (insertTimeMap mask) m v

-- fromFiles :: Foldable t => TimeMask ->
-- (V.Vector TempData -> TimeMap) ->
-- t Prelude.FilePath -> IO (V.Vector TempData -> TimeMap)
-- fromFiles :: TimeMask -> TimeMap -> Shell Turtle.FilePath -> IO(TimeMap)
-- fromFiles mask = Fold.foldM (freader mask)
-- fromFiles mask m l = F.foldlM (reader mask) m $ F.toList l

-- foldM :: (Foldable f, Monad m) => FoldM m a b -> f a -> m b
-- FoldM (x -> a -> m x) (m x) (x -> m b)

-- F.foldM .toList . (find name)

mkList :: Shell a -> IO [a]
mkList fp = fold fp Fold.list

freader :: TimeMask -> TimeMap -> Pattern Text ->
    Turtle.FilePath -> IO TimeMap
freader mask m sensor dr = do
    f <- mkList $ files sensor dr
    F.foldlM (reader mask) m f

reader :: TimeMask -> TimeMap -> Turtle.FilePath -> IO TimeMap
reader mask m = reader' mask m . filePathToString

reader' :: TimeMask -> TimeMap -> Prelude.FilePath -> IO TimeMap
reader' mask m fname = do
    csvData <- BL.readFile fname
    let v = decode HasHeader csvData :: Either String (V.Vector TempData)
    case v of
        Left s -> do
            putStrLn $ "file: " ++ fname ++ ", error: " ++ s
            return m
        Right vec -> return $ makeMap mask m vec

filePathToString :: Turtle.FilePath -> Prelude.FilePath
filePathToString = Text.unpack . format fp

dir :: Turtle.FilePath
dir = "/Users/zander/7120CR178/HouseData/Temperatures"

indoor :: Turtle.FilePath
indoor =
    "/Users/zander/7120CR178/HouseData/Temperatures/IndoorTemp120601.csv"

files :: Pattern Text -> Turtle.FilePath -> Shell Turtle.FilePath
files sensor = find $ has sensor <> star alphaNum <> ".csv"

ff x = print (dateTime x, temperature x)

{-
Temperatures - analyze temperature statistics from a temperature
data logger. To run use:

% stack exec -- temperatures-exe dir files

dir - directory containing data files
files - files containing data
-}
main :: IO ()
main = do
    args <- getArgs
    print args
    setCurrentDirectory $ head args
    csvData <- BL.readFile "testdata.csv"
    let v = decode HasHeader csvData :: Either String (V.Vector TempData)
    case v of
        Left s -> putStrLn s
        Right vec -> do
            V.mapM_ print vec
            V.mapM_ ff vec
            -- let x = foldMap stats $ justFilter vec
            -- putStrLn $ "Stats = " ++ show x


{--
-- freader mask m sensor dr =
--     -- F.foldlM (reader mask) m =<< (fold (files sensor dr) Fold.list)
--     F.foldlM (reader mask) m =<< (mkList $ files sensor dr)
-- Orphan instance not in prelude required by Data.Semigroup.Min
instance Bounded Float where
    maxBound = 1/0 :: Float
    minBound = negate maxBound

-- Orphan instance not in prelude required by Data.Semigroup.Min
instance Bounded Double where
    maxBound = 1/0
    minBound = negate maxBound

-- Orphan instance not in Data.Monoid
instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f) =>
                Monoid (a,b,c,d,e,f) where
        mempty = (mempty, mempty, mempty, mempty, mempty,mempty)
        (a1,b1,c1,d1,e1,f1) `mappend` (a2,b2,c2,d2,e2,f2) =
                (a1 `mappend` a2, b1 `mappend` b2, c1 `mappend` c2,
                 d1 `mappend` d2, e1 `mappend` e2, f1 `mappend` f2)

-- Define the type of numerical data
type Number = Double

-- parseString = "%D %r:%M:%S %p"

-- timeFromString :: String -> LocalTime
-- timeFromString ts = readTime defaultTimeLocale parseString ts
insertTimeMap mask td m =
    let time = makeTime $ dateTime td in
        case temperature td of
            Nothing -> m
            Just t -> insert mask time (makeStats time t) m
            --Just t -> insertWith accumStats time (makeStats time t) m

insertTimeMap mask td m = do
    ltime <- timeFromString $ dateTime td
    temp <- temperature td
    let tim = packYMDHm $ fromLocalTime ltime
    return insert mask tim (makeStats tim temp) m


-- I have to properly parse the time here
justFilter :: Foldable f => f TempData -> [TempTime]
justFilter = foldMap f where
    f td = case temperature td of
        Nothing -> []
        Just t -> [Arg t (timeFromString (dateTime td))]

oJust :: a -> Option a
oJust = Option . Just

oNothing :: Option a
oNothing = Option $ Nothing

stats :: TempTime -> (Option (First LocalTime), Sum Int, Sum Number,
    Sum Number, Option (Min TempTime), Option (Max TempTime))
stats tt@(Arg temp time) = (
    Option $ Just $ First time, -- Start of time interval
    const (Sum 1) tt,           -- Count number of values
    Sum temp,                   -- Sum of temperature
    Sum $ temp * temp,          -- Sum of temperature squared
    Option $ Just $ Min tt,     -- Minimum temperature and time it occured
    Option $ Just $ Max tt      -- Maximum temperature and time it occured
    )

--}
