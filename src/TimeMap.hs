{-# OPTIONS_GHC -funbox-strict-fields #-}
module TimeMap where

import Lib
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

type Number = Double

data TempTime = TempTime !Number !PkYMDHm deriving Show

data Stats = Stats
    { nValues :: !Int
    , mean :: !Number
    , stddev :: !Number
    , miniTemp :: !TempTime
    , maxiTemp :: !TempTime
    } deriving Show

sumTemp = mean
sumTempSq = stddev

type TimeMap = IntMap Stats

-- insertWith :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
-- Need function a -> a -> a where a == Stats

minTemp tt1@(TempTime temp1 _) tt2@(TempTime temp2 _) =
    if temp1 <= temp2 then tt1 else tt2

maxTemp tt1@(TempTime temp1 _) tt2@(TempTime temp2 _) =
    if temp1 >= temp2 then tt1 else tt2

makeStats tim temp = Stats
    { nValues = 1
    , mean = temp
    , stddev = temp * temp
    , miniTemp = TempTime temp tim
    , maxiTemp = TempTime temp tim
    }

accumStats st1 st2 = Stats
    { nValues = nValues st1 + nValues st2
    , mean = sumTemp st1 + sumTemp st2
    , stddev = sumTempSq st1 + sumTempSq st2
    , miniTemp = minTemp (miniTemp st1) (miniTemp st2)
    , maxiTemp = maxTemp (maxiTemp st1) (maxiTemp st2)
    }

analyzeStats st = Stats
    { nValues = nValues st
    , mean = avg
    , stddev = sqrt $ sumTempSq st / n - avg * avg
    , miniTemp = miniTemp st
    , maxiTemp = maxiTemp st
    } where
        n = fromIntegral $ nValues st
        avg = sumTemp st / n

insert mask time stats m = IntMap.insertWith accumStats k stats m
    where k = fromPkYMDHm $ timeMask mask time

foldStats mask m = IntMap.foldlWithKey' f IntMap.empty m
    where f tmap i stats = insert mask (PkYMDHm i) stats tmap

printTimeMap :: TimeMap -> IO [()]
printTimeMap m = mapM prt $ IntMap.assocs m
    where
        prt (tim, stat) = print (PkYMDHm tim) >> print (analyzeStats stat)
