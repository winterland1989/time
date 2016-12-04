{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Time.Format.Part where

import Data.Time.LocalTime
import Data.Time.Format.Locale
import Data.Int
import Data.Char (toLower, toUpper)
import Data.Fixed
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Calendar.OrdinalDate (toOrdinalDate, mondayStartWeek, sundayStartWeek)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Clock

-- | The @no padding@, @space padded@ and @zero padded@ modifier.
--
data PaddingModifier = NP | SP | ZP deriving (Show, Eq, Ord)

-- | The @lowercase@, @no case conversion@ and @uppercase@ modifier.
--
data CaseModifier = Lower | NoMod | Upper deriving (Show, Eq, Ord)

-- | All the various formatter that can be part of a time format string.
-- <http://www.opengroup.org/onlinepubs/007908799/xsh/strftime.html>
--
-- 'Part' is parametrized by a @builder@ type, for easily implementing other backends.
--
data Part
    = Century PaddingModifier      -- ^ century, padded to 2 chars.
    | WDCentury PaddingModifier    -- ^ century for Week Date format, padded to 2 chars.
    | Year2 PaddingModifier        -- ^ year of century (70 is 1970, 69 is 2069), padded to 2 chars.
    | WDYear2 PaddingModifier      -- ^ year of century for Week Date format, padded to 4 chars
    | Year4 PaddingModifier        -- ^ year, padded to 4 chars
    | WDYear4 PaddingModifier      -- ^ year for Week Date format, padded to 4 chars
    | WeekOfYear PaddingModifier   -- ^ 'mondayStartWeek' of year (0 - 53) , padded to 2 chars
    | WeekOfYear' PaddingModifier  -- ^ 'sundayStartWeek' of year (0 - 53), padded to 2 chars
    | WDWeekOfYear PaddingModifier -- ^ week of year for Week Date format, padded to 2 chars.
    | Month PaddingModifier        -- ^ month of year (1 - 12).
    | MonthAbbr CaseModifier       -- ^ name of the month short ('snd' from 'months' locale, Jan - Dec).
    | MonthFull CaseModifier       -- ^ name of the month long ('snd' from 'months' locale, January - December).
    | DayOfMonth PaddingModifier   -- ^ day of month, (1 - 32).
    | DayOfYear PaddingModifier    -- ^ day of year, (1 - 366).
    | DayOfWeek                    -- ^ day of week, 0 (= Sunday) - 6 (= Saturday).
    | WDDayOfWeek                  -- ^ day of week for Week Date format, (1 - 7).
    | WeekDayAbbr CaseModifier     -- ^ weekday name abbreviated ('snd' from 'wDays' locale, Sun - Sat).
    | WeekDayFull CaseModifier     -- ^ weekday name full ('fst' from 'wDays' locale, Sunday - Saturday).
    | DayHalf CaseModifier         -- ^ day-half from ('amPm' locale), (AM, PM).
    | Hour PaddingModifier         -- ^ hour of day (0 to 23).
    | HourHalf PaddingModifier     -- ^ hour of day-half (0 to 12).
    | Minute PaddingModifier       -- ^ minute (0 to 59).
    | Second PaddingModifier       -- ^ second (0 to 59).
    | MilliSecond                  -- ^ millisecond (000 to 999).
    | MicroSecond                  -- ^ microsecond (000000 to 999999).
    | NanoSecond                   -- ^ nanosecond (000000000 to 999999999).
    | PicoSecond                   -- ^ picosecond (000000000000 to 999999999999).
    | SecondFrac                   -- ^ decimal point and fraction of second up to 12 decimals without trailing zeros.
    | PosixSeconds                 -- ^ number of seconds since 1 jan 1970. unix epoch.
    | TZ                           -- ^ timeoffset offset (+0200).
    | TZColon                      -- ^ timeoffset offset with colon (+02:00)
    | TZName CaseModifier          -- ^ timezone name (e.g. GMT, PST).
    | Char Char                    -- ^ a verbatim 'Char'.
    | String String                -- ^ a verbatim 'String'.
  deriving (Show, Eq)

mkParts :: TimeLocale -> String -> [Part]
mkParts l ('%':p:xs) = case p of
    'z' -> TZ : next
    'Z' -> TZName Upper : next
    'c' -> mkParts l (dateTimeFmt l) ++ next
    'R' -> mkParts l "%H:%M" ++ next
    'T' -> mkParts l "%H:%M:%S" ++ next
    'X' -> mkParts l (timeFmt l) ++ next
    'r' -> mkParts l (time12Fmt l) ++ next
    'P' -> DayHalf Lower : next
    'p' -> DayHalf Upper : next
    'H' -> Hour ZP : next
    'k' -> Hour SP : next
    'I' -> HourHalf ZP : next
    'l' -> HourHalf SP : next
    'M' -> Minute ZP : next
    'S' -> Second ZP : next
    'q' -> PicoSecond : next
    'Q' -> Char '.' : SecondFrac : next
    's' -> PosixSeconds : next
    'D' -> mkParts l "%m/%d/%y" ++ next
    'F' -> mkParts l "%Y-%m-%d" ++ next
    'x' -> mkParts l (dateFmt l) ++ next
    'Y' -> Year4 NP : next
    'y' -> Year2 ZP : next
    'C' -> Century NP : next
    'B' -> MonthFull NoMod: next
    'b' -> MonthAbbr NoMod: next
    'h' -> MonthAbbr NoMod: next
    'm' -> Month ZP : next
    'd' -> DayOfMonth ZP : next
    'e' -> DayOfMonth SP : next
    'j' -> DayOfYear ZP : next
    'G' -> WDYear4 NP : next
    'g' -> WDYear2 ZP : next
    'f' -> WDCentury ZP : next
    'V' -> WDWeekOfYear ZP : next
    'u' -> WDDayOfWeek : next
    'a' -> WeekDayAbbr NoMod : next
    'A' -> WeekDayFull NoMod : next
    'U' -> WeekOfYear' ZP : next
    'W' -> WeekOfYear ZP : next
    'w' -> DayOfWeek : next
    '%' -> Char '%'  : next
    't' -> Char '\t' : next
    'n' -> Char '\n' : next
    '-' -> case xs of
        (y:ys)
            | y == 'H' || y == 'k' -> Hour NP : next'
            | y == 'I' || y == 'l' -> HourHalf NP : next'
            | y == 'M' -> Minute NP : next'
            | y == 'S' -> Second NP : next'
            | y == 'Y' -> Year4 NP : next'
            | y == 'y' -> Year2 NP : next'
            | y == 'm' -> Month NP : next'
            | y == 'd' || y == 'e' -> DayOfMonth NP : next'
            | y == 'j' -> DayOfYear NP : next'
            | y == 'G' -> WDYear4 NP : next'
            | y == 'g' -> WDYear2 NP : next'
            | y == 'f' -> WDCentury NP : next'
            | y == 'V' -> WDWeekOfYear NP : next'
            | y == 'U' -> WeekOfYear' NP : next'
            | y == 'W' -> WeekOfYear NP : next'
            | otherwise -> next
          where next' = mkParts l ys
        _ -> next
    '_' -> case xs of
        (y:ys)
            | y == 'H' || y == 'k' -> Hour SP : next'
            | y == 'I' || y == 'l' -> HourHalf SP : next'
            | y == 'M' -> Minute SP : next'
            | y == 'S' -> Second SP : next'
            | y == 'Y' -> Year4 SP : next'
            | y == 'y' -> Year2 SP : next'
            | y == 'm' -> Month SP : next'
            | y == 'd' || y == 'e' -> DayOfMonth SP : next'
            | y == 'j' -> DayOfYear SP : next'
            | y == 'G' -> WDYear4 SP : next'
            | y == 'g' -> WDYear2 SP : next'
            | y == 'f' -> WDCentury SP : next'
            | y == 'V' -> WDWeekOfYear SP : next'
            | y == 'U' -> WeekOfYear' SP : next'
            | y == 'W' -> WeekOfYear SP : next'
            | otherwise -> next
          where next' = mkParts l ys
        _ -> next
    '0' -> case xs of
        (y:ys)
            | y == 'H' || y == 'k' -> Hour ZP : next'
            | y == 'I' || y == 'l' -> HourHalf ZP : next'
            | y == 'M' -> Minute ZP : next'
            | y == 'S' -> Second ZP : next'
            | y == 'Y' -> Year4 ZP : next'
            | y == 'y' -> Year2 ZP : next'
            | y == 'm' -> Month ZP : next'
            | y == 'd' || y == 'e' -> DayOfMonth ZP : next'
            | y == 'j' -> DayOfYear ZP : next'
            | y == 'G' -> WDYear4 ZP : next'
            | y == 'g' -> WDYear2 ZP : next'
            | y == 'f' -> WDCentury ZP : next'
            | y == 'V' -> WDWeekOfYear ZP : next'
            | y == 'U' -> WeekOfYear' ZP : next'
            | y == 'W' -> WeekOfYear ZP : next'
            | otherwise -> next
          where next' = mkParts l ys
        _ -> next
    '^' -> case xs of
        (y:ys)
            | y == 'P' -> DayHalf Upper : next'
            | y == 'b' || y == 'h' -> MonthAbbr Upper : next'
            | y == 'B' -> MonthFull Upper : next'
            | y == 'a' -> WeekDayAbbr Upper : next'
            | y == 'A' -> WeekDayFull Upper : next'
            | otherwise -> next
          where next' = mkParts l ys
        _ -> next
    '#' -> case xs of
        (y:ys)
            | y == 'P' -> DayHalf Lower : next'
            | y == 'b' || y == 'h' -> MonthAbbr Lower : next'
            | y == 'B' -> MonthFull Lower : next'
            | y == 'a' -> WeekDayAbbr Lower : next'
            | y == 'A' -> WeekDayFull Lower : next'
            | otherwise -> next
          where next' = mkParts l ys
        _ -> next
    _   -> next
  where
    next = mkParts l xs

mkParts l (x:xs) = Char x : mkParts l xs
mkParts _ [] = []

--------------------------------------------------------------------------------

class FormatTime t builder where
    -- | Each @t@ should replace the 'Part' s it knows with a 'Builder' 'builder',
    -- 'Data.Time.Format.Part.Char' and 'Data.Time.Format.Part.String' can be either
    -- left as it is or be processed depend on different implementation.
    --
    buildTimeParts ::  TimeLocale -> t -> (Part -> builder) -> Part -> builder

formatTimeParts :: (FormatTime t String) => TimeLocale -> [Part] -> t -> String
formatTimeParts l parts t = concatMap (buildTimeParts l t (const "")) parts

instance FormatTime UniversalTime String where
    buildTimeParts l ut = buildTimeParts l (ut1ToLocalTime 0 ut)

instance FormatTime UTCTime String where
    buildTimeParts l ut = buildTimeParts l (utcToZonedTime utc ut)

instance FormatTime ZonedTime String where
    buildTimeParts l zt@(ZonedTime lt tz) next part =
        case part of
            PosixSeconds -> show posixS
            String s     -> s
            Char c       -> [c]
            p            -> next' p
      where
        next' = buildTimeParts l lt (buildTimeParts l tz next)
        posixS = floor (utcTimeToPOSIXSeconds (zonedTimeToUTC zt)) :: Integer

instance FormatTime TimeZone String where
    buildTimeParts _ (TimeZone t _ name) next part =
        case part of
            TZ        -> (sign : showZP2 h) ++ showZP2 m
            TZColon   -> (sign : showZP2 h) ++ (':' : showZP2 m)
            TZName c  -> modifyCase c name
            String s    -> s
            Char c      -> [c]
            p         -> next p
      where
        sign = if t < 0 then '-' else '+'
        (h, m) =  abs t `quotRem` 60

instance FormatTime LocalTime String where
    buildTimeParts l (LocalTime day tod) next =
        buildTimeParts l day (buildTimeParts l tod next)

instance FormatTime Day String where
    buildTimeParts l day next part =
        case part of
            Century p      -> show2 p century
            WDCentury p    -> show2 p century_wd
            Year2 p        -> show2 p yy
            WDYear2 p      -> show2 p yy_wd
            Year4 p        -> show4 p yyyy
            WDYear4 p      -> show4 p yyyy_wd
            WeekOfYear p   -> show2 p ww
            WeekOfYear' p  -> show2 p ww'
            WDWeekOfYear p -> show2 p ww_wd
            Month p        -> show2 p mm
            MonthAbbr c    -> (modifyCase c monthAbbr)
            MonthFull c    -> (modifyCase c monthFull)
            DayOfMonth p   -> show2 p md
            DayOfYear p    -> show3 p yd
            DayOfWeek      -> show wd'
            WDDayOfWeek    -> show wd_wd
            WeekDayAbbr c  -> (modifyCase c wDayAbbr)
            WeekDayFull c  -> (modifyCase c wDayFull)
            String s    -> s
            Char c      -> [c]
            p              -> next p
      where
        (yyyy, mm, md) = toGregorian day
        (century, yy) = yyyy `quotRem` 100
        (_, yd) = toOrdinalDate day
        monthAbbr = snd (months l !! (mm - 1))
        monthFull = fst (months l !! (mm - 1))
        (yyyy_wd, ww_wd, wd_wd) = toWeekDate day
        (century_wd, yy_wd) = yyyy_wd `quotRem` 100
        (ww, _) = mondayStartWeek day   -- 1 - 7
        (ww', wd') = sundayStartWeek day -- 0 - 6
        wDayAbbr = snd (wDays l !! wd')
        wDayFull = fst (wDays l !! wd')

instance FormatTime TimeOfDay String where
    buildTimeParts l (TimeOfDay hour minute (MkFixed ps)) next part =
        case part of
            Hour p      -> show2 p hour
            HourHalf p  -> show2 p hourHalf
            Minute p    -> show2 p minute
            Second p    -> show2 p isec
            MilliSecond -> showZP3 (fsec `div` 1000000000)
            MicroSecond -> showZP6 (fsec `div` 1000000)
            NanoSecond  -> showZP9 (fsec `div` 1000)
            PicoSecond  -> showZP12 fsec
            SecondFrac  -> '.' : takeWhile (/='0') (showZP12 fsec)
            DayHalf c   -> modifyCase c dayHalf
            String s    -> s
            Char c      -> [c]
            p           -> next p
      where
        hourHalf = mod (hour - 1) 12 + 1
        (isec, fsec) = fromIntegral ps `quotRem` (1000000000000 :: Int64)
        dayHalf = (if hour < 12 then fst else snd) (amPm l)

--------------------------------------------------------------------------------

modifyCase :: CaseModifier -> String -> String
modifyCase NoMod = id
modifyCase Upper = map toUpper
modifyCase Lower = map toLower
{-# INLINE modifyCase #-}

show2 :: (Show a, Integral a) => PaddingModifier -> a -> String
show2 NP = show
show2 ZP = showZP2
show2 SP = showSP2
{-# INILINE show2 #-}
{-# SPECIALIZE show2 :: PaddingModifier -> Int -> String #-}
{-# SPECIALIZE show2 :: PaddingModifier -> Integer -> String #-}

show3 :: (Show a, Integral a) => PaddingModifier -> a -> String
show3 NP = show
show3 ZP = showZP3
show3 SP = showSP3
{-# INILINE show3 #-}
{-# SPECIALIZE show3 :: PaddingModifier -> Int -> String #-}
{-# SPECIALIZE show3 :: PaddingModifier -> Integer -> String #-}

show4 :: (Show a, Integral a) => PaddingModifier -> a -> String
show4 NP = show
show4 ZP = showZP4
show4 SP = showSP4
{-# INILINE show4 #-}
{-# SPECIALIZE show4 :: PaddingModifier -> Int -> String #-}
{-# SPECIALIZE show4 :: PaddingModifier -> Integer -> String #-}

showZP2 :: (Show a, Integral a) => a -> String
showZP2 n = if n < 10 then '0' : show n
                      else show n
{-# INLINE showZP2 #-}

showSP2 :: (Show a, Integral a) => a -> String
showSP2 n = if n < 10 then ' ' : show n
                      else show n
{-# INLINE showSP2 #-}

showZP3 :: (Show a, Integral a) => a -> String
showZP3 n
    | n < 10 = "00" ++ show n
    | n < 100 = "0" ++ show n
    | otherwise = show n
{-# INLINE showZP3 #-}

showSP3 :: (Show a, Integral a) => a -> String
showSP3 n
    | n < 10 = "  " ++ show n
    | n < 100 = " " ++ show n
    | otherwise = show n
{-# INLINE showSP3 #-}

showZP4 :: (Show a, Integral a) => a -> String
showZP4 n
    | n < 10 = "000" ++ show n
    | n < 100 = "00" ++ show n
    | n < 1000 = "0" ++ show n
    | otherwise = show n
{-# INLINE showZP4 #-}

showSP4 :: (Show a, Integral a) => a -> String
showSP4 n
    | n < 10 = "   " ++ show n
    | n < 100 = "  " ++ show n
    | n < 1000 = " " ++ show n
    | otherwise = show n
{-# INLINE showSP4 #-}

showZP6 :: (Show a, Integral a) => a -> String
showZP6 n
    | n < 10 = "00000" ++ show n
    | n < 100 = "0000" ++ show n
    | n < 1000 = "000" ++ show n
    | n < 10000 = "00" ++ show n
    | n < 100000 = "0" ++ show n
    | otherwise = show n
{-# INLINE showZP6 #-}

showZP9 :: (Show a, Integral a) => a -> String
showZP9 n
    | n < 10 = "00000000" ++ show n
    | n < 100 = "0000000" ++ show n
    | n < 1000 = "000000" ++ show n
    | n < 10000 = "00000" ++ show n
    | n < 100000 = "0000" ++ show n
    | n < 1000000 = "000" ++ show n
    | n < 10000000 = "00" ++ show n
    | n < 100000000 = "0" ++ show n
    | otherwise = show n
{-# INLINE showZP9 #-}

showZP12 :: (Show a, Integral a) => a -> String
showZP12 n
    | n < 10 = "00000000000" ++ show n
    | n < 100 = "0000000000" ++ show n
    | n < 1000 = "000000000" ++ show n
    | n < 10000 = "00000000" ++ show n
    | n < 100000 = "0000000" ++ show n
    | n < 1000000 = "000000" ++ show n
    | n < 10000000 = "00000" ++ show n
    | n < 100000000 = "0000" ++ show n
    | n < 1000000000 = "000" ++ show n
    | n < 10000000000 = "00" ++ show n
    | n < 100000000000 = "0" ++ show n
    | otherwise = show n
{-# INLINE showZP12 #-}
