module Data.Time.Format.Part where

import Data.Time.LocalTime
import Data.Time.Format.Locale
import Data.Int
import Data.Fixed

-- | The @no padding@, @space padded@ and @zero padded@ modifier.
--
data PaddingModifier = NP | SP | ZP deriving (Show, Eq, Ord)

-- | The @uppercase@, @no case conversion@ and @uppercase@ modifier.
--
data CaseModifier = Lower | NoModify | Upper deriving (Show, Eq, Ord)

-- | All the various formatter that can be part of a time format string.
-- <http://www.opengroup.org/onlinepubs/007908799/xsh/strftime.html>
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
    | MonthShort CaseModifier      -- ^ name of the month short ('snd' from 'months' locale, Jan - Dec).
    | MonthLong CaseModifier       -- ^ name of the month long ('snd' from 'months' locale, January - December).
    | DayOfMonth PaddingModifier
    | DayOfYear PaddingModifier
    | DayOfWeek
    | WDDayOfWeek
    | WeekDayShort CaseModifier
    | WeekDayLong CaseModifier
    | DayHalf CaseModifier
    | Hour PaddingModifier
    | HourHalf PaddingModifier
    | Minute PaddingModifier
    | Second PaddingModifier
    | MilliSecond
    | MicroSecond
    | NanoSecond
    | PicoSecond
    | NTSecondFrac                 -- ^ decimal point and fraction of second, up to 12 decimals without trailing zeros
    | PosixSeconds
    | TZ
    | TZColon
    | TZName CaseModifier
    | Char Char
    | String String
  deriving (Show, Eq)

mkParts :: TimeLocale -> String -> [Part]
mkParts l ('%':p:xs) = case p of
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
            | y == 'b' || y == 'h' -> MonthShort Upper : next'
            | y == 'B' -> MonthLong Upper : next'
            | y == 'a' -> WeekDayShort Upper : next'
            | y == 'A' -> WeekDayLong Upper : next'
            | otherwise -> next
          where next' = mkParts l ys
        _ -> next

    '#' -> case xs of
        (y:ys)
            | y == 'P' -> DayHalf Lower : next'
            | y == 'b' || y == 'h' -> MonthShort Lower : next'
            | y == 'B' -> MonthLong Lower : next'
            | y == 'a' -> WeekDayShort Lower : next'
            | y == 'A' -> WeekDayLong Lower : next'
            | otherwise -> next
          where next' = mkParts l ys
        _ -> next

    '%' -> Char '%'  : next
    't' -> Char '\t' : next
    'n' -> Char '\n' : next
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
    'Q' -> Char '.' : NTSecondFrac : next
    's' -> PosixSeconds : next
    'D' -> mkParts l "%m/%d/%y" ++ next
    'F' -> mkParts l "%Y-%m-%d" ++ next
    'x' -> mkParts l (dateFmt l) ++ next
    'Y' -> Year4 NP : next
    'y' -> Year2 ZP : next
    'C' -> Century NP : next
    'B' -> MonthLong NoModify: next
    'b' -> MonthShort NoModify: next
    'h' -> MonthShort NoModify: next
    'm' -> Month ZP : next
    'd' -> DayOfMonth ZP : next
    'e' -> DayOfMonth SP : next
    'j' -> DayOfYear ZP : next
    'G' -> WDYear4 NP : next
    'g' -> WDYear2 ZP : next
    'f' -> WDCentury ZP : next
    'V' -> WDWeekOfYear ZP : next
    'u' -> WDDayOfWeek : next
    'a' -> WeekDayShort NoModify : next
    'A' -> WeekDayLong NoModify : next
    'U' -> WeekOfYear' ZP : next
    'W' -> WeekOfYear ZP : next
    'w' -> DayOfWeek : next
    _   -> next
  where
    next = mkParts l xs

mkParts l (x:xs) = Char x : mkParts l xs
mkParts _ [] = []

--------------------------------------------------------------------------------

class FormatTime t where
    -- | Each @t@ should replace the 'Part' it knows with a 'String' or 'Char' 'Part'.
    --
    formatPart ::  TimeLocale -> t -> [Part] -> [Part]

formatParts :: (FormatTime t) => TimeLocale -> [Part] -> t -> String
formatParts l parts t = foldr go "" (formatPart l t parts)
  where
    go (String f) acc = f ++ acc
    go (Char c) acc = c:acc
    go _         acc = acc

{-

instance FormatTime LocalTime where
    formatPart l part (LocalTime day tod) =
        formatPart l part (localTimeOfDay day) . formatPart part l (localTimeOfDay tod)
-}
instance FormatTime TimeOfDay where
    -- Aggregate
    formatPart _ _ [] = []
    formatPart l t@(TimeOfDay hour minute (MkFixed ps)) (part:parts) = case part of
        Hour NP -> showNP hour next
        Hour ZP -> showZP2 hour next
        Hour SP -> showSP2 hour next
        HourHalf NP -> showNP hourHalf next
        HourHalf ZP -> showZP2 hourHalf next
        HourHalf SP -> showSP2 hourHalf next
        Minute NP -> showNP minute next
        Minute ZP -> showZP2 minute next
        Minute SP -> showSP2 minute next
        Second NP -> showNP (fromIntegral isec) next
        Second ZP -> showZP2 (fromIntegral isec) next
        Second SP -> showSP2 (fromIntegral isec) next
        MilliSecond  -> showZP3 (fromIntegral (fsec `div` 1000000000)) next
        MicroSecond  -> showZP6 (fsec `div` 1000000) next
        NanoSecond   -> showZP9 (fsec`div` 1000) next
        PicoSecond   -> showZP12 fsec next
        NTSecondFrac -> showZP12NT fsec next
        Char x       -> Char x : next
        _            -> next
      where
        next = formatPart l t parts
        {-# INLINE next #-}
        hourHalf = mod (hour - 1) 12 + 1
        (isec, fsec) = fromIntegral ps `divMod` (1000000000000 :: Int64)

--------------------------------------------------------------------------------

type PartS =  [Part] -> [Part]

showNP :: Int -> PartS
showNP n next = String (show n) : next
{-# INLINE showNP #-}

showZP2 :: Int -> PartS
showZP2 n next  = if n < 10 then String "0" : String (show n) : next
                            else String (show n) : next
{-# INLINE showZP2 #-}

showSP2 :: Int -> PartS
showSP2 n next = if n < 10 then String " " : String (show n) : next
                           else String (show n) : next
{-# INLINE showSP2 #-}

showZP3 :: Int -> PartS
showZP3 n next
    | n < 10 = String "00" : String (show n) : next
    | n < 100 = String "0" : String (show n) : next
    | otherwise = String (show n) : next
{-# INLINE showZP3 #-}

showSP3 :: Int -> PartS
showSP3 n next
    | n < 10 = String "  " : String (show n) : next
    | n < 100 = String " " : String (show n) : next
    | otherwise = String (show n) : next
{-# INLINE showSP3 #-}

showZP4 :: Int -> PartS
showZP4 n next
    | n < 10 = String "000" : String (show n) : next
    | n < 100 = String "00" : String (show n) : next
    | n < 1000 = String "0" : String (show n) : next
    | otherwise = String (show n) : next
{-# INLINE showZP4 #-}

showSP4 :: Int -> PartS
showSP4 n next
    | n < 10 = String "   " : String (show n) : next
    | n < 100 = String "  " : String (show n) : next
    | n < 1000 = String " " : String (show n) : next
    | otherwise = String (show n) : next
{-# INLINE showSP4 #-}

showZP6 :: Int64 -> PartS
showZP6 n next
    | n < 10 = String "00000" : String (show n) : next
    | n < 100 = String "0000" : String (show n) : next
    | n < 1000 = String "000" : String (show n) : next
    | n < 10000 = String "00" : String (show n) : next
    | n < 100000 = String "0" : String (show n) : next
    | otherwise = String (show n) : next
{-# INLINE showZP6 #-}

showZP9 :: Int64 -> PartS
showZP9 n next
    | n < 10 = String "00000000" : String (show n) : next
    | n < 100 = String "0000000" : String (show n) : next
    | n < 1000 = String "000000" : String (show n) : next
    | n < 10000 = String "00000" : String (show n) : next
    | n < 100000 = String "0000" : String (show n) : next
    | n < 1000000 = String "000" : String (show n) : next
    | n < 10000000 = String "00" : String (show n) : next
    | n < 100000000 = String "0" : String (show n) : next
    | otherwise = String (show n) : next
{-# INLINE showZP9 #-}

showZP12 :: Int64 -> PartS
showZP12 n next
    | n < 10 = String "00000000000" : String (show n) : next
    | n < 100 = String "0000000000" : String (show n) : next
    | n < 1000 = String "000000000" : String (show n) : next
    | n < 10000 = String "00000000" : String (show n) : next
    | n < 100000 = String "0000000" : String (show n) : next
    | n < 1000000 = String "000000" : String (show n) : next
    | n < 10000000 = String "00000" : String (show n) : next
    | n < 100000000 = String "0000" : String (show n) : next
    | n < 1000000000 = String "000" : String (show n) : next
    | n < 10000000000 = String "00" : String (show n) : next
    | n < 100000000000 = String "0" : String (show n) : next
    | otherwise = String (show n) : next
{-# INLINE showZP12 #-}

showZP12NT :: Int64 -> PartS
showZP12NT n next
    | n < 10 = String "00000000000" : String (cut (show n)) : next
    | n < 100 = String "0000000000" : String (cut (show n)) : next
    | n < 1000 = String "000000000" : String (cut (show n)) : next
    | n < 10000 = String "00000000" : String (cut (show n)) : next
    | n < 100000 = String "0000000" : String (cut (show n)) : next
    | n < 1000000 = String "000000" : String (cut (show n)) : next
    | n < 10000000 = String "00000" : String (cut (show n)) : next
    | n < 100000000 = String "0000" : String (cut (show n)) : next
    | n < 1000000000 = String "000" : String (cut (show n)) : next
    | n < 10000000000 = String "00" : String (cut (show n)) : next
    | n < 100000000000 = String "0" : String (cut (show n)) : next
    | otherwise = String (cut (show n)) : next
  where
    cut = takeWhile (/= '0')
{-# INLINE showZP12NT #-}
