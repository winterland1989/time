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
import Debug.Trace (trace)

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
data Part a
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
    | MonthAbbr CaseModifier      -- ^ name of the month short ('snd' from 'months' locale, Jan - Dec).
    | MonthFull CaseModifier       -- ^ name of the month long ('snd' from 'months' locale, January - December).
    | DayOfMonth PaddingModifier
    | DayOfYear PaddingModifier
    | DayOfWeek
    | WDDayOfWeek
    | WeekDayAbbr CaseModifier
    | WeekDayFull CaseModifier
    | DayHalf CaseModifier
    | Hour PaddingModifier
    | HourHalf PaddingModifier
    | Minute PaddingModifier
    | Second PaddingModifier
    | MilliSecond
    | MicroSecond
    | NanoSecond
    | PicoSecond
    | SecondFrac                   -- ^ decimal point and fraction of second, up to 12 decimals without trailing zeros
    | PosixSeconds
    | TZ
    | TZColon
    | TZName CaseModifier
    | Char Char
    | Part a
  deriving (Show, Eq)

mkParts :: TimeLocale -> String -> [Part a]
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
    -- | Each @t@ should replace the 'Part' s it knows with a 'Part' 'builder',
    -- 'Char' can be either left as it is or be processed depend
    -- on different implementation.
    --
    formatPart ::  TimeLocale -> t -> [Part builder] -> [Part builder]

formatParts :: (FormatTime t String) => TimeLocale -> [Part String] -> t -> String
formatParts l parts t = foldr go "" (formatPart l t parts)
  where
    go (Part f) acc = f ++ acc
    go (Char c) acc = c:acc
    go _        acc = acc

instance FormatTime LocalTime String where
    formatPart l (LocalTime day tod) =
        formatPart l day . formatPart l tod

instance FormatTime Day String where
    formatPart _ _ [] = []
    formatPart l day (part:parts) = case part of
        Century p      -> show2 p century                 next
        WDCentury p    -> show2 p century_wd              next
        Year2 p        -> show2 p yy                      next
        WDYear2 p      -> show2 p yy_wd                   next
        Year4 p        -> show4 p yyyy                    next
        WDYear4 p      -> show4 p yyyy_wd                 next
        WeekOfYear p   -> show2 p ww                      next
        WeekOfYear' p  -> show2 p ww'                     next
        WDWeekOfYear p -> show2 p ww_wd                   next
        Month p        -> show2 p mm                      next
        MonthAbbr c    -> Part (modifyCase c monthAbbr) : next
        MonthFull c    -> Part (modifyCase c monthFull) : next
        DayOfMonth p   -> show2 p md                      next
        DayOfYear p    -> show3 p yd                      next
        DayOfWeek      -> showNP wd                       next
        WDDayOfWeek    -> showNP wd_wd                    next
        WeekDayAbbr c  -> Part (modifyCase c wDayAbbr) :  next
        WeekDayFull c  -> Part (modifyCase c wDayFull) :  next
        p              -> p :                             next
      where
        next = formatPart l day parts
        (yyyy, mm, md) = toGregorian day
        (century, yy) = yyyy `divMod` 100
        (_, yd) = toOrdinalDate day
        monthAbbr = snd (months l !! (mm - 1))
        monthFull = fst (months l !! (mm - 1))
        (yyyy_wd, ww_wd, wd_wd) = toWeekDate day
        (century_wd, yy_wd) = yyyy_wd `divMod` 100
        (ww, wd) = mondayStartWeek day
        (ww', wd') = sundayStartWeek day
        wDayAbbr = snd (wDays l !! (wd - 1))
        wDayFull = fst (wDays l !! (wd - 1))

instance FormatTime TimeOfDay String where
    formatPart _ _ [] = []
    formatPart l t@(TimeOfDay hour minute (MkFixed ps)) (part:parts) = case part of
        Hour p      -> show2 p hour                    next
        HourHalf p  -> show2 p hourHalf                next
        Minute p    -> show2 p minute                  next
        Second p    -> show2 p isec                    next
        MilliSecond -> showZP3 (fsec `div` 1000000000) next
        MicroSecond -> showZP6 (fsec `div` 1000000)    next
        NanoSecond  -> showZP9 (fsec`div` 1000)        next
        PicoSecond  -> showZP12 fsec                   next
        SecondFrac  -> showZP12NT fsec                 next
        DayHalf c   -> Part (modifyCase c dayHalf) :   next
        p           -> p :                             next
      where
        next = formatPart l t parts
        hourHalf = mod (hour - 1) 12 + 1
        (isec, fsec) = fromIntegral ps `divMod` (1000000000000 :: Int64)
        dayHalf = (if hour < 12 then fst else snd) (amPm l)

--------------------------------------------------------------------------------

modifyCase :: CaseModifier -> String -> String
modifyCase NoMod = id
modifyCase Upper = map toUpper
modifyCase Lower = map toLower
{-# INLINE modifyCase #-}

type PartS = [Part String] -> [Part String]

show2 :: (Show a, Integral a) => PaddingModifier -> a -> PartS
show2 NP = showNP
show2 ZP = showZP2
show2 SP = showSP2
{-# INLINE show2 #-}

show3 :: (Show a, Integral a) => PaddingModifier -> a -> PartS
show3 NP = showNP
show3 ZP = showZP3
show3 SP = showSP3
{-# INLINE show3 #-}

show4 :: (Show a, Integral a) => PaddingModifier -> a -> PartS
show4 NP = showNP
show4 ZP = showZP4
show4 SP = showSP4
{-# INLINE show4 #-}

showNP :: (Show a, Integral a) => a -> PartS
showNP n next = Part (show n) : next
{-# INLINE showNP #-}

showZP2 :: (Show a, Integral a) => a -> PartS
showZP2 n next  = if n < 10 then Part "0" : Part (show n) : next
                            else Part (show n) : next
{-# INLINE showZP2 #-}

showSP2 :: (Show a, Integral a) => a -> PartS
showSP2 n next = if n < 10 then Part " " : Part (show n) : next
                           else Part (show n) : next
{-# INLINE showSP2 #-}

showZP3 :: (Show a, Integral a) => a -> PartS
showZP3 n next
    | n < 10 = Part "00" : Part (show n) : next
    | n < 100 = Part "0" : Part (show n) : next
    | otherwise = Part (show n) : next
{-# INLINE showZP3 #-}

showSP3 :: (Show a, Integral a) => a -> PartS
showSP3 n next
    | n < 10 = Part "  " : Part (show n) : next
    | n < 100 = Part " " : Part (show n) : next
    | otherwise = Part (show n) : next
{-# INLINE showSP3 #-}

showZP4 :: (Show a, Integral a) => a -> PartS
showZP4 n next
    | n < 10 = Part "000" : Part (show n) : next
    | n < 100 = Part "00" : Part (show n) : next
    | n < 1000 = Part "0" : Part (show n) : next
    | otherwise = Part (show n) : next
{-# INLINE showZP4 #-}

showSP4 :: (Show a, Integral a) => a -> PartS
showSP4 n next
    | n < 10 = Part "   " : Part (show n) : next
    | n < 100 = Part "  " : Part (show n) : next
    | n < 1000 = Part " " : Part (show n) : next
    | otherwise = Part (show n) : next
{-# INLINE showSP4 #-}

showZP6 :: (Show a, Integral a) => a -> PartS
showZP6 n next
    | n < 10 = Part "00000" : Part (show n) : next
    | n < 100 = Part "0000" : Part (show n) : next
    | n < 1000 = Part "000" : Part (show n) : next
    | n < 10000 = Part "00" : Part (show n) : next
    | n < 100000 = Part "0" : Part (show n) : next
    | otherwise = Part (show n) : next
{-# INLINE showZP6 #-}

showZP9 :: (Show a, Integral a) => a -> PartS
showZP9 n next
    | n < 10 = Part "00000000" : Part (show n) : next
    | n < 100 = Part "0000000" : Part (show n) : next
    | n < 1000 = Part "000000" : Part (show n) : next
    | n < 10000 = Part "00000" : Part (show n) : next
    | n < 100000 = Part "0000" : Part (show n) : next
    | n < 1000000 = Part "000" : Part (show n) : next
    | n < 10000000 = Part "00" : Part (show n) : next
    | n < 100000000 = Part "0" : Part (show n) : next
    | otherwise = Part (show n) : next
{-# INLINE showZP9 #-}

showZP12 :: (Show a, Integral a) => a -> PartS
showZP12 n next
    | n < 10 = Part "00000000000" : Part (show n) : next
    | n < 100 = Part "0000000000" : Part (show n) : next
    | n < 1000 = Part "000000000" : Part (show n) : next
    | n < 10000 = Part "00000000" : Part (show n) : next
    | n < 100000 = Part "0000000" : Part (show n) : next
    | n < 1000000 = Part "000000" : Part (show n) : next
    | n < 10000000 = Part "00000" : Part (show n) : next
    | n < 100000000 = Part "0000" : Part (show n) : next
    | n < 1000000000 = Part "000" : Part (show n) : next
    | n < 10000000000 = Part "00" : Part (show n) : next
    | n < 100000000000 = Part "0" : Part (show n) : next
    | otherwise = Part (show n) : next
{-# INLINE showZP12 #-}

showZP12NT :: (Show a, Integral a) => a -> PartS
showZP12NT n next
    | n < 10 = Part "00000000000" : Part (cut (show n)) : next
    | n < 100 = Part "0000000000" : Part (cut (show n)) : next
    | n < 1000 = Part "000000000" : Part (cut (show n)) : next
    | n < 10000 = Part "00000000" : Part (cut (show n)) : next
    | n < 100000 = Part "0000000" : Part (cut (show n)) : next
    | n < 1000000 = Part "000000" : Part (cut (show n)) : next
    | n < 10000000 = Part "00000" : Part (cut (show n)) : next
    | n < 100000000 = Part "0000" : Part (cut (show n)) : next
    | n < 1000000000 = Part "000" : Part (cut (show n)) : next
    | n < 10000000000 = Part "00" : Part (cut (show n)) : next
    | n < 100000000000 = Part "0" : Part (cut (show n)) : next
    | otherwise = Part (cut (show n)) : next
  where
    cut = takeWhile (/= '0')
{-# INLINE showZP12NT #-}
