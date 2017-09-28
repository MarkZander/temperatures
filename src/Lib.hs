module Lib where
    {-
    ( someFunc
    , Years
    , toYears
    ) where
    -}

import Data.Int
import Data.Bits
import Data.Time
import Data.List

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap


someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype Years   = Years Int deriving (Eq, Ord, Show)
newtype Months  = Months Int deriving (Eq, Ord, Show)
newtype Days    = Days Int deriving (Eq, Ord, Show)
newtype Hours   = Hours Int deriving (Eq, Ord, Show)
newtype Minutes = Minutes Int deriving (Eq, Ord, Show)

data YMDHm = YMDHm Years Months Days Hours Minutes
                deriving (Eq, Ord, Show)
newtype PkYMDHm = PkYMDHm { fromPkYMDHm :: Int }
                deriving (Eq, Ord)

newtype TimeMask = TimeMask { fromTimeMask :: Int }
                deriving (Eq, Ord, Show)

instance Show PkYMDHm where
    show pk = show $ unpackYMDHm pk

toLocalTime :: YMDHm -> LocalTime
toLocalTime
    (YMDHm (Years y) (Months m) (Days d) (Hours h) (Minutes mn)) =
    LocalTime (fromGregorian (fromIntegral y) m d) (TimeOfDay h mn 0)

fromLocalTime :: LocalTime -> YMDHm
fromLocalTime (LocalTime day (TimeOfDay h mn s)) =
    YMDHm (Years y) (Months m) (Days d) (Hours h) (Minutes mn)
    where
        (yy, m, d) = toGregorian day
        y = fromIntegral yy

--
-- formatString :: String
-- formatString = "%m/%d/%y %l:%M:%S %p"
-- testString = "11/03/14 04:10:00 PM"

formatString :: String
formatString = "%m/%d/%y %H:%M:%S"

testString = "11/03/14 04:10:00"

timeFromString :: String -> Maybe LocalTime
timeFromString = parseTimeM True defaultTimeLocale formatString

timeToString :: LocalTime -> String
timeToString = formatTime defaultTimeLocale formatString

pack6 :: Int -> Int -> Int
pack6 i v = shiftL v 6 + i .&. 63

unpack6 :: Int -> (Int, Int)
unpack6 v = (shiftR v 6, v .&. 63)

packYMDHm :: YMDHm -> PkYMDHm
packYMDHm
    (YMDHm (Years y) (Months m) (Days d) (Hours h) (Minutes mn))
    = PkYMDHm $ pack6 mn $ pack6 h $ pack6 (d - 1)
        $ pack6 (m - 1) (y - 2000)

unpackYMDHm :: PkYMDHm -> YMDHm
unpackYMDHm (PkYMDHm i) = YMDHm (Years (y + 2000)) (Months (m + 1))
    (Days (d + 1)) (Hours h) (Minutes mn)
    where
        (i1, mn) = unpack6 i
        (i2, h) = unpack6 i1
        (i3, d) = unpack6 i2
        (y, m) = unpack6 i3

mask0 = TimeMask 0
mnMask = TimeMask 63                -- keep minutes
hMask = TimeMask (shiftL 63 6)      -- keep hours
dMask = TimeMask (shiftL 63 12)     -- keep days
mMask = TimeMask (shiftL 63 18)     -- keep months
yMask = TimeMask (shiftL 255 24)    -- keep Years

-- determine which time unit to keep with the mask. mask0 should
-- result in stats summarized over all time units.

timeMask (TimeMask mask) (PkYMDHm time) = PkYMDHm (time .&. mask)

orMask (TimeMask m1) (TimeMask m2) = TimeMask (m1 .|. m2)

orMasks = foldr orMask mask0

maskYMDH = orMasks [yMask, mMask, dMask, hMask]

{--
timeFromString :: String -> LocalTime
timeFromString =
    parseTimeOrError True defaultTimeLocale formatString

data MaskUnit = Summarize | NoSummerzie

data TimeMask = TimeMask
    { yearMask :: MaskUnit
    , monthMask :: MaskUnit
    , dayMask :: MaskUnit
    , hourMask :: MaskUnit
    , minuteMask :: MaskUnit
}

unitMask Summarize _ st = st
unitMask NoSummerize ut _ = ut

timeMask (YMDHm ym mm dm hm minum) (TimeMask yt mt dt ht minut) = YMDHm
    unitMask ym yt Years 0
    unitMask mm mt Months 1
    unitMask dm dt Days 1
    unitMask hm ht Hours 0
    unitMask minum minut Minutes 0

sumNone = TimeMask
    { yearMask = NoSummerize
    , monthMask = NoSummerize
    , dayMask = NoSummerize
    , hourMask = NoSummerize
    , minuteMask = NoSummerize
    }

ymdh = sumNone { minuteMask = Summerize }

ymd = ymdh { hourMask = Summerize }

ym = ymd { dayMask = Summerize }

y = ym { monthMask = Summerize }

sumAll = y { yearMask = Summerize }

m = sumAll { monthMask = NoSummerize }

md = sumAll { monthMask = NoSummerize, dayMask = NoSummerize }

year0 = Years 0
month1 = Months 1
day1 = Days 1
hour0 = Hours 0
minute0 = Minutes 0

toYMDH :: YMDHm -> YMDH
toYMDH (YMDHm y m d h _) = YMDH y m d h

fromYMDH :: YMDH -> YMDHm
fromYMDH (YMDH y m d h) = YMDHm y m d h minute0

toYMD :: YMDHm -> YMD
toYMD (YMDHm y m d _ _) = YMD y m d

fromYMD :: YMD -> YMDHm
fromYMD (YMD y m d) = YMDHm y m d hour0 minute0

toYM :: YMDHm -> YM
toYM (YMDHm y m _ _ _) = YM y m

fromYM :: YM -> YMDHm
fromYM (YM y m) = YMDHm y m day1 hour0 minute0

toMD :: YMDHm -> MD
toMD (YMDHm _ m d _ _) = MD m d

fromMD :: MD -> YMDHm
fromMD (MD m d) = YMDHm year0 m d hour0 minute0

toMDH :: YMDHm -> MDH
toMDH (YMDHm _ m d h _) = MDH m d h

fromMDH :: MDH -> YMDHm
fromMDH (MDH m d h) = YMDHm year0 m d h minute0

toMH :: YMDHm -> MH
toMH (YMDHm _ m _ h _) = MH m h

fromMH :: MH -> YMDHm
fromMH (MH m h) = YMDHm year0 m day1 h minute0

toYMH :: YMDHm -> YMH
toYMH (YMDHm y m _ h _) = YMH y m h

fromYMH :: YMH -> YMDHm
fromYMH (YMH y m h) = YMDHm y m day1 h minute0

data YMDH  = YMDH  Years Months Days Hours deriving (Eq, Ord, Show)
data YMD   = YMD   Years Months Days deriving (Eq, Ord, Show)
data YM    = YM    Years Months deriving (Eq, Ord, Show)

data MD = MD Months Days deriving (Eq, Ord, Show)
data MDH = MDH Months Days Hours deriving (Eq, Ord, Show)
data MH = MH Months Hours deriving (Eq, Ord, Show)
data YMH = YMH Years Months Hours deriving (Eq, Ord, Show)

pack8 :: Int -> Int -> Int
pack8 i v = shiftL v 8 + i .&. 255

unpack8 :: Int -> (Int, Int)
unpack8 v = (shiftR v 8, v .&. 255)

timeIn :: String -> String -> LocalTime
timeIn format ts =
    parseTimeOrError True defaultTimeLocale formatString ts

timeOut :: String -> LocalTime -> String
timeOut format lt = formatTime defaultTimeLocale formatString lt

-- packing and unpacking of the above data structures is a good idea
-- but would be different than below.

newtype YMDHm = YMDHm Int deriving Eq
newtype YMDH = YMDH Int deriving Eq
newtype YMD = YMD Int deriving Eq
newtype YM = YM Int deriving Eq

newtype Yearly = Yearly Int deriving Eq
-- Not yet done
newtype Monthly = Monthly Int
newtype Hourly = Hourly Int
newtype MD = MD Int
newtype MDH = MDH Int
newtype MH = MH Int
newtype YMH = YMH Int

toYMDH :: YMDHm -> YMDH
toYMDH (YMDHm i) = YMDH $ shiftR8 i

fromYMDH :: YMDH -> YMDHm
fromYMDH (YMDH i) = YMDHm $ shiftL8 i + 0

toYMD :: YMDHm -> YMD
toYMD (YMDHm i) = YMD $ shiftR8 $ shiftR8 i

fromYMD :: YMD -> YMDHm
fromYMD (YMD i) = fromYMDH $ YMDH $ shiftL8 i + 0

toYM :: YMDHm -> YM
toYM (YMDHm i) = YM $ shiftR8 $ shiftR8 $ shiftR8 i

fromYM :: YM -> YMDHm
fromYM (YM i) = fromYMD $ YMD $ shiftL8 i + 1

toYearly :: YMDHm -> Yearly
toYearly (YMDHm i) = Yearly $ shiftR8 $ shiftR8 $ shiftR8 $ shiftR8 i

fromYearly :: Yearly -> YMDHm
fromYearly (Yearly i) = fromYM $ YM $ shiftL8 i + 1

toLocalTime :: Int -> Int -> Int -> Int -> Int -> Int -> LocalTime
toLocalTime y m d h mn s = LocalTime (fromGregorian (fromIntegral y) m d)
    (TimeOfDay h mn (fromIntegral s))

fromLocalTime :: LocalTime -> [Int]
fromLocalTime (LocalTime day (TimeOfDay h mn s)) =
    [fromIntegral y, m, d, h, mn]
    where (y, m, d) = toGregorian day

packYMDHm :: LocalTime -> YMDHm
packYMDHm lt = YMDHm (foldl' pack x xs)
    where (x:xs) = fromLocalTime lt

unpackYMDHm :: YMDHm -> LocalTime
unpackYMDHm (YMDHm i) = toLocalTime y m d h mn 0
    where
        (i1, mn) = unpack i
        (i2, h) = unpack i1
        (i3, d) = unpack i2
        (y, m) = unpack i3

fromYears :: Years -> YMDHm
fromYears y = YMDHm y month1 day1 hour0 minute0

fromMonths :: Months -> YMDHm
fromMonths m = YMDHm year0 m day1 hour0 minute0

fromDays :: Days -> YMDHm
fromDays d = YMDHm year0 month1 d hour0 minute0

fromHours :: Hours -> YMDHm
fromHours h = YMDHm year0 month1 day1 h minute0

fromMinutes :: Minutes -> YMDHm
fromMinutes mn = YMDHm year0 month1 day1 hour0 mn

--}
---------------------------------------------------------------
