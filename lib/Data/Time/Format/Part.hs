{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Time.Format.Part where

import Data.Time.LocalTime
import Data.Time.Format.Locale
import Data.Int
import Data.Fixed
import Control.Arrow ((&&&))

-- | All the various formatter that can be part of a time format string.
-- <http://www.opengroup.org/onlinepubs/007908799/xsh/strftime.html>
--
newtype Part = Part Int  -- There's no sign bit in Unicode, so 'Int' can present all 'Char'.

instance Show Part where
    show (Part (-1) ) =  "NPCentury"
    show (Part (-2) ) =  "SPCentury"
    show (Part (-3) ) =  "ZPCentury"
    show (Part (-4) ) =  "NPWDCentury"
    show (Part (-5) ) =  "SPWDCentury"
    show (Part (-6) ) =  "ZPWDCentury"
    show (Part (-7) ) =  "NPYear2"
    show (Part (-8) ) =  "SPYear2"
    show (Part (-9) ) =  "ZPYear2"
    show (Part (-10)) =  "NPYear4"
    show (Part (-11)) =  "SPYear4"
    show (Part (-12)) =  "ZPYear4"
    show (Part (-13)) =  "NPWDYear2"
    show (Part (-14)) =  "SPWDYear2"
    show (Part (-15)) =  "ZPWDYear2"
    show (Part (-16)) =  "NPWDYear4"
    show (Part (-17)) =  "SPWDYear4"
    show (Part (-18)) =  "ZPWDYear4"
    show (Part (-19)) =  "NPWDWeek"
    show (Part (-20)) =  "SPWDWeek"
    show (Part (-21)) =  "ZPWDWeek"
    show (Part (-22)) =  "WDDayOfWeek"
    show (Part (-23)) =  "NPMonth"
    show (Part (-24)) =  "SPMonth"
    show (Part (-25)) =  "ZPMonth"
    show (Part (-26)) =  "MonthShortLower"
    show (Part (-27)) =  "MonthShort"
    show (Part (-28)) =  "MonthShortUpper"
    show (Part (-29)) =  "MonthLongLower"
    show (Part (-30)) =  "MonthLong"
    show (Part (-31)) =  "MonthLongUpper"
    show (Part (-32)) =  "NPMonthOfYear"
    show (Part (-33)) =  "SPMonthOfYear"
    show (Part (-34)) =  "ZPMonthOfYear"
    show (Part (-35)) =  "NPDayOfMonth"
    show (Part (-36)) =  "SPDayOfMonth"
    show (Part (-37)) =  "ZPDayOfMonth"
    show (Part (-38)) =  "NPDayOfYear"
    show (Part (-39)) =  "SPDayOfYear"
    show (Part (-40)) =  "ZPDayOfYear"
    show (Part (-41)) =  "DayOfWeek"
    show (Part (-42)) =  "WeekDayShortLower"
    show (Part (-43)) =  "WeekDayShort"
    show (Part (-44)) =  "WeekDayShortUpper"
    show (Part (-45)) =  "WeekDayLongLower"
    show (Part (-46)) =  "WeekDayLong"
    show (Part (-47)) =  "WeekDayLongUpper"
    show (Part (-48)) =  "NPWeekOfYear'"
    show (Part (-49)) =  "SPWeekOfYear'"
    show (Part (-50)) =  "ZPWeekOfYear'"
    show (Part (-51)) =  "NPWeekOfYear"
    show (Part (-52)) =  "SPWeekOfYear"
    show (Part (-53)) =  "ZPWeekOfYear"
    show (Part (-54)) =  "NPWDWeekOfYear"
    show (Part (-55)) =  "SPWDWeekOfYear"
    show (Part (-56)) =  "ZPWDWeekOfYear"
    show (Part (-57)) =  "DayHalfLower"
    show (Part (-58)) =  "DayHalfUpper"
    show (Part (-59)) =  "NPHour"
    show (Part (-60)) =  "SPHour"
    show (Part (-61)) =  "ZPHour"
    show (Part (-62)) =  "NPHourHalf"
    show (Part (-63)) =  "SPHourHalf"
    show (Part (-64)) =  "ZPHourHalf"
    show (Part (-65)) =  "NPMinute"
    show (Part (-66)) =  "SPMinute"
    show (Part (-67)) =  "ZPMinute"
    show (Part (-68)) =  "NPSecond"
    show (Part (-69)) =  "SPSecond"
    show (Part (-70)) =  "ZPSecond"
    show (Part (-71)) =  "PosixSeconds"
    show (Part (-72)) =  "MilliSecond"
    show (Part (-73)) =  "MicroSecond"
    show (Part (-74)) =  "NanoSecond"
    show (Part (-75)) =  "PicoSecond"
    show (Part (-76)) =  "TZ"
    show (Part (-77)) =  "TZColon"
    show (Part (-78)) =  "TZNameLower"
    show (Part (-79)) =  "TZNameUpper"
    show (Part c    ) =  "Char " ++ show c

-- | century, no padding.
pattern NPCentury = Part (-1)
-- | century, space padded to 2 chars
pattern SPCentury = Part (-2)
-- | century, zero padded to 2 chars
pattern ZPCentury = Part (-3)
-- | century for Week Date format, no padding
pattern NPWDCentury = Part (-4)
-- | century for Week Date format, space padded to 2 chars
pattern SPWDCentury = Part (-5)
-- | century for Week Date format, zero padded to 2 chars
pattern ZPWDCentury = Part (-6)
-- | year of century (70 is 1970, 69 is 2069), no padding
pattern NPYear2 = Part (-7)
-- | year of century (70 is 1970, 69 is 2069), space padded to 2 chars
pattern SPYear2 = Part (-8)
-- | year of century (70 is 1970, 69 is 2069), zero padded to 2 chars
pattern ZPYear2 = Part (-9)
-- | year, no padding
pattern NPYear4 = Part (-10)
-- | year, space padded to 4 chars
pattern SPYear4 = Part (-11)
-- | year, zero padded to 4 chars
pattern ZPYear4 = Part (-12)
-- | year of century for Week Date format, no padding
pattern NPWDYear2 = Part (-13)
-- | year of century for Week Date format, space padded to 4 chars
pattern SPWDYear2 = Part (-14)
-- | year of century for Week Date format, zero padded to 4 chars
pattern ZPWDYear2 = Part (-15)
-- | year for Week Date format, no padding.
pattern NPWDYear4 = Part (-16)
-- | year for Week Date format, space padded to 4 chars.
pattern SPWDYear4 = Part (-17)
-- | year for Week Date format, zero padded to 4 chars.
pattern ZPWDYear4 = Part (-18)
-- | week of year for Week Date format (1 - 53), no padding.
pattern NPWDWeek = Part (-19)
-- | week of year for Week Date format (1 - 53), space padded to two chars.
pattern SPWDWeek = Part (-20)
-- | week of year for Week Date format (1 - 53), zero padded to two chars.
pattern ZPWDWeek = Part (-21)
-- | day of week for Week Date format, (1 - 7).
pattern WDDayOfWeek = Part (-22)
-- | months (1 to 12), no padding.
pattern NPMonth = Part (-23)
-- | months (1 to 12), space padded.
pattern SPMonth = Part (-24)
-- | months (1 to 12), zero padded.
pattern ZPMonth = Part (-25)
-- | name of the month short ('snd' from 'months' locale, Jan - Dec), converted to lowercase.
pattern MonthShortLower = Part (-26)
-- | name of the month short ('snd' from 'months' locale, Jan - Dec).
pattern MonthShort = Part (-27)
-- | name of the month short ('snd' from 'months' locale, Jan - Dec), converted to uppercase.
pattern MonthShortUpper = Part (-28)
-- | name of the month long ('snd' from 'months' locale, January - December), converted to lowercase.
pattern MonthLongLower = Part (-29)
-- | name of the month long ('snd' from 'months' locale, January - December).
pattern MonthLong = Part (-30)
-- | name of the month long ('snd' from 'months' locale, January - December), converted to uppercase.
pattern MonthLongUpper = Part (-31)
-- | month of year, (1 - 12), no padding.
pattern NPMonthOfYear = Part (-32)
-- | month of year, (1 - 12), space padded.
pattern SPMonthOfYear = Part (-33)
-- | month of year, (1 - 12), zero padded.
pattern ZPMonthOfYear = Part (-34)
-- | day of month, (1 - 32), no padding
pattern NPDayOfMonth = Part (-35)
-- | day of month, (1 - 32), space padded.
pattern SPDayOfMonth = Part (-36)
-- | day of month, (1 - 32), zero padded.
pattern ZPDayOfMonth = Part (-37)
-- | day of year, (1 - 366), no padding.
pattern NPDayOfYear = Part (-38)
-- | day of year, (1 - 366), space padded.
pattern SPDayOfYear = Part (-39)
-- | day of year, (1 - 366), zero padded.
pattern ZPDayOfYear = Part (-40)
-- | day of week, (1 - 7).
pattern DayOfWeek = Part (-41)
-- | short weekday name ('snd' from 'wDays' locale, Sun - Sat), converted to lowercase.
pattern WeekDayShortLower = Part (-42)
-- | short weekday name ('snd' from 'wDays' locale, Sun - Sat)
pattern WeekDayShort = Part (-43)
-- | short weekday name ('snd' from 'wDays' locale, Sun - Sat), converted to uppercase.
pattern WeekDayShortUpper = Part (-44)
-- | long weekday name ('snd' from 'wDays' locale, Sunday - Saturday), converted to lowercase.
pattern WeekDayLongLower = Part (-45)
-- | long weekday name ('snd' from 'wDays' locale, Sunday - Saturday)
pattern WeekDayLong = Part (-46)
-- | long weekday name ('snd' from 'wDays' locale, Sunday - Saturday), converted to uppercase.
pattern WeekDayLongUpper = Part (-47)
-- | week of year where weeks start on Sunday (as 'sundayStartWeek', 0 - 53), no padding.
pattern NPWeekOfYear' = Part (-48)
-- | week of year where weeks start on Sunday (as 'sundayStartWeek', 0 - 53), space padded to two chars.
pattern SPWeekOfYear' = Part (-49)
-- | week of year where weeks start on Sunday (as 'sundayStartWeek', 0 - 53), zero padded to two chars.
pattern ZPWeekOfYear' = Part (-50)
-- | week of year where weeks start on Monday (as 'mondayStartWeek', 0 - 53), no padding.
pattern NPWeekOfYear = Part (-51)
-- | week of year where weeks start on Monday (as 'mondayStartWeek', 0 - 53), space padded.
pattern SPWeekOfYear = Part (-52)
-- | week of year where weeks start on Monday (as 'mondayStartWeek', 0 - 53), space padded.
pattern ZPWeekOfYear = Part (-53)
-- | week of year for Week Date format, no padding.
pattern NPWDWeekOfYear = Part (-54)
-- | week of year for Week Date format, no padding.
pattern SPWDWeekOfYear = Part (-55)
-- | week of year for Week Date format, no padding.
pattern ZPWDWeekOfYear = Part (-56)
-- | day-half of day from ('amPm' locale, AM, PM), converted to lowercase.
pattern DayHalfLower = Part (-57)
-- | day-half of day from ('amPm' locale, AM, PM).
pattern DayHalfUpper = Part (-58)
-- | hour of day (0 to 23), no padding.
pattern NPHour = Part (-59)
-- | hour of day (0 to 23), space padded.
pattern SPHour = Part (-60)
-- | hour of day (0 to 23), zero padded.
pattern ZPHour = Part (-61)
-- | hour of day-half (0 to 12), no padding.
pattern NPHourHalf = Part (-62)
-- | hour of day-half (0 to 12), space padded.
pattern SPHourHalf = Part (-63)
-- | hour of day-half (0 to 12), zero padded.
pattern ZPHourHalf = Part (-64)
-- | minutes (0 to 59), no padding.
pattern NPMinute = Part (-65)
-- | minutes (0 to 59), space padded.
pattern SPMinute = Part (-66)
-- | minutes (0 to 59), zero padded.
pattern ZPMinute = Part (-67)
-- | seconds (0 to 59, 60 for leap seconds), no padding.
pattern NPSecond = Part (-68)
-- | seconds (0 to 59, 60 for leap seconds), space padding.
pattern SPSecond = Part (-69)
-- | seconds (0 to 59, 60 for leap seconds), zero padding.
pattern ZPSecond = Part (-70)
-- | number of seconds since 1 jan 1970. unix epoch.
pattern PosixSeconds = Part (-71)
-- | Milliseconds (000 to 999), without trailing zeros
pattern MilliSecond = Part (-72)
-- | MicroSeconds (000000 to 999999), without trailing zeros
pattern MicroSecond = Part (-73)
-- | NanoSeconds (000000000 to 999999999), without trailing zeros
pattern NanoSecond = Part (-74)
-- | PicoSeconds (000000000000 to 999999999999), without trailing zeros.
pattern PicoSecond = Part (-75)
-- | timeoffset offset (+0200)
pattern TZ = Part (-76)
-- | timeoffset offset with colon (+02:00)
pattern TZColon = Part (-77)
-- | timezone name (e.g. GMT, PST), converted to lowercase.
pattern TZNameLower = Part (-78)
-- | timezone name (e.g. GMT, PST).
pattern TZNameUpper = Part (-79)
-- | a verbatim char
pattern Char :: Char -> Part
pattern Char c <- Part (signum &&& toEnum -> (1, c)) where Char c = Part (fromEnum c)

mkParts :: TimeLocale -> String -> [Part]
mkParts l ('%':p:xs) = case p of
    '-' -> case xs of
        (y:ys)
            | y == 'H' || y == 'k' -> NPHour : next'
            | y == 'I' || y == 'l' -> NPHourHalf : next'
            | y == 'M' -> NPMinute : next'
            | y == 'S' -> NPSecond : next'
            | y == 'Y' -> NPYear4 : next'
            | y == 'y' -> NPYear2 : next'
            | y == 'm' -> NPMonthOfYear : next'
            | y == 'd' || y == 'e' -> NPDayOfMonth : next'
            | y == 'j' -> NPDayOfYear : next'
            | y == 'G' -> NPWDYear4 : next'
            | y == 'g' -> NPWDYear2 : next'
            | y == 'f' -> NPWDCentury : next'
            | y == 'V' -> NPWDWeekOfYear : next'
            | y == 'U' -> NPWeekOfYear' : next'
            | y == 'W' -> NPWeekOfYear : next'
            | otherwise -> next
          where next' = mkParts l ys
        _ -> next

    '_' -> case xs of
        (y:ys)
            | y == 'H' || y == 'k' -> SPHour : next'
            | y == 'I' || y == 'l' -> SPHourHalf : next'
            | y == 'M' -> SPMinute : next'
            | y == 'S' -> SPSecond : next'
            | y == 'Y' -> SPYear4 : next'
            | y == 'y' -> SPYear2 : next'
            | y == 'm' -> SPMonthOfYear : next'
            | y == 'd' || y == 'e' -> SPDayOfMonth : next'
            | y == 'j' -> SPDayOfYear : next'
            | y == 'G' -> SPWDYear4 : next'
            | y == 'g' -> SPWDYear2 : next'
            | y == 'f' -> SPWDCentury : next'
            | y == 'V' -> SPWDWeekOfYear : next'
            | y == 'U' -> SPWeekOfYear' : next'
            | y == 'W' -> SPWeekOfYear : next'
            | otherwise -> next
          where next' = mkParts l ys
        _ -> next

    '0' -> case xs of
        (y:ys)
            | y == 'H' || y == 'k' -> ZPHour : next'
            | y == 'I' || y == 'l' -> ZPHourHalf : next'
            | y == 'M' -> ZPMinute : next'
            | y == 'S' -> ZPSecond : next'
            | y == 'Y' -> ZPYear4 : next'
            | y == 'y' -> ZPYear2 : next'
            | y == 'm' -> ZPMonthOfYear : next'
            | y == 'd' || y == 'e' -> ZPDayOfMonth : next'
            | y == 'j' -> ZPDayOfYear : next'
            | y == 'G' -> ZPWDYear4 : next'
            | y == 'g' -> ZPWDYear2 : next'
            | y == 'f' -> ZPWDCentury : next'
            | y == 'V' -> ZPWDWeekOfYear : next'
            | y == 'U' -> ZPWeekOfYear' : next'
            | y == 'W' -> ZPWeekOfYear : next'
            | otherwise -> next
          where next' = mkParts l ys
        _ -> next

    '^' -> case xs of
        (y:ys)
            | y == 'P' -> DayHalfUpper : next'
            | y == 'b' || y == 'h' -> MonthShortUpper : next'
            | y == 'B' -> MonthLongUpper : next'
            | y == 'a' -> WeekDayShortUpper : next'
            | y == 'A' -> WeekDayLongUpper : next'
            | otherwise -> next
          where next' = mkParts l ys
        _ -> next

    '#' -> case xs of
        (y:ys)
            | y == 'P' -> DayHalfLower : next'
            | y == 'b' || y == 'h' -> MonthShortLower : next'
            | y == 'B' -> MonthLongLower : next'
            | y == 'a' -> WeekDayShortLower : next'
            | y == 'A' -> WeekDayLongLower : next'
            | otherwise -> next
          where next' = mkParts l ys
        _ -> next

    '%' -> Char '%'  : next
    't' -> Char '\t' : next
    'n' -> Char '\n' : next

    'z' -> TZ : next
    'Z' -> TZNameUpper : next

    'c' -> mkParts l (dateTimeFmt l) ++ next

    'R' -> mkParts l "%H:%M" ++ next
    'T' -> mkParts l "%H:%M:%S" ++ next
    'X' -> mkParts l (timeFmt l) ++ next
    'r' -> mkParts l (time12Fmt l) ++ next

    'P' -> DayHalfLower : next
    'p' -> DayHalfUpper : next

    'H' -> ZPHour : next
    'k' -> SPHour : next

    'I' -> ZPHourHalf : next
    'l' -> SPHourHalf : next

    'M' -> ZPMinute : next

    'S' -> ZPSecond : next

    'q' -> PicoSecond : next
    'Q' -> Char '.' : PicoSecond : next

    's' -> PosixSeconds : next
    'D' -> mkParts l "%m/%d/%y" ++ next
    'F' -> mkParts l "%Y-%m-%d" ++ next
    'x' -> mkParts l (dateFmt l) ++ next

    'Y' -> NPYear4 : next
    'y' -> ZPYear2 : next

    'C' -> NPCentury : next
    'B' -> MonthLong : next
    'b' -> MonthShort : next
    'h' -> MonthShort : next


    'm' -> ZPMonthOfYear : next

    'd' -> ZPDayOfMonth : next
    'e' -> SPDayOfMonth : next

    'j' -> ZPDayOfYear : next

    'G' -> NPWDYear4 : next
    'g' -> ZPWDYear2 : next

    'f' -> ZPWDCentury : next

    'V' -> ZPWDWeekOfYear : next
    'u' -> WDDayOfWeek : next

    'a' -> WeekDayShort : next
    'A' -> WeekDayLong : next

    'U' -> ZPWeekOfYear' : next
    'W' -> ZPWeekOfYear : next

    'w' -> DayOfWeek : next

    _ -> next
  where
    next = mkParts l xs

mkParts l (x:xs) = Char x : mkParts l xs
mkParts _ [] = []

--------------------------------------------------------------------------------

formatParts :: (FormatTime t) => TimeLocale -> [Part] -> t -> String
formatParts l parts t = foldr (\part -> (formatPart l part t .)) id parts ""

class FormatTime t where
    formatPart ::  TimeLocale -> Part -> t -> ShowS
{-
instance FormatTime LocalTime where
    formatPart l part (LocalTime day tod) =
        formatPart l part (localTimeOfDay day) . formatPart part l (localTimeOfDay tod)
-}
instance FormatTime TimeOfDay where
    -- Aggregate
    formatPart _ part (TimeOfDay hour minute (MkFixed ps)) = case part of
        NPHour -> shows hour
        ZPHour -> showsZP2 hour
        SPHour -> showsSP2 hour
        NPHourHalf -> shows hourHalf
        ZPHourHalf -> showsZP2 hourHalf
        SPHourHalf -> showsSP2 hourHalf
        NPMinute -> shows minute
        ZPMinute -> showsZP2 minute
        SPMinute -> showsSP2 minute
        NPSecond -> shows isec
        ZPSecond -> showsZP2 (fromIntegral isec)
        SPSecond -> showsSP2 (fromIntegral isec)
        PosixSeconds -> shows isec
        MilliSecond  -> showsZP3 (fromIntegral (fsec `div` 1000000000))
        MicroSecond  -> showsZP6 (fsec `div` 1000000)
        NanoSecond   -> showsZP9 (fsec`div` 1000)
        PicoSecond   -> showsZP12 fsec
        Char x       -> (x:)
        _            -> id
      where
        hourHalf = mod (hour - 1) 12 + 1
        (isec, fsec) = fromIntegral ps `divMod` (1000000000000 :: Int64)


--------------------------------------------------------------------------------

showsZP2 :: Int -> ShowS
showsZP2 n = if n < 10 then (:) '0' . shows n else shows n
{-# INLINE showsZP2 #-}

showsSP2 :: Int -> ShowS
showsSP2 n = if n < 10 then (:) ' ' . shows n else shows n
{-# INLINE showsSP2 #-}

showsZP3 :: Int -> ShowS
showsZP3 n
    | n < 10 = (++) "00" . shows n
    | n < 100 = (++) "0" . shows n
    | otherwise = shows n
{-# INLINE showsZP3 #-}

showsSP3 :: Int -> ShowS
showsSP3 n
    | n < 10 = (++) "  " . shows n
    | n < 100 = (++) " " . shows n
    | otherwise = shows n
{-# INLINE showsSP3 #-}

showsZP4 :: Int -> ShowS
showsZP4 n
    | n < 10 = (++) "000" . shows n
    | n < 100 = (++) "00" . shows n
    | n < 1000 = (++) "0" . shows n
    | otherwise = shows n
{-# INLINE showsZP4 #-}

showsSP4 :: Int -> ShowS
showsSP4 n
    | n < 10 = (++) "   " . shows n
    | n < 100 = (++) "  " . shows n
    | n < 1000 = (++) " " . shows n
    | otherwise = shows n
{-# INLINE showsSP4 #-}

showsZP6 :: Int64 -> ShowS
showsZP6 n
    | n < 10 = (++) "00000" . shows n
    | n < 100 = (++) "0000" . shows n
    | n < 1000 = (++) "000" . shows n
    | n < 10000 = (++) "00" . shows n
    | n < 100000 = (++) "0" . shows n
    | otherwise = shows n
{-# INLINE showsZP6 #-}

showsZP9 :: Int64 -> ShowS
showsZP9 n
    | n < 10 = (++) "00000000" . shows n
    | n < 100 = (++) "0000000" . shows n
    | n < 1000 = (++) "000000" . shows n
    | n < 10000 = (++) "00000" . shows n
    | n < 100000 = (++) "0000" . shows n
    | n < 1000000 = (++) "000" . shows n
    | n < 10000000 = (++) "00" . shows n
    | n < 100000000 = (++) "0" . shows n
    | otherwise = shows n
{-# INLINE showsZP9 #-}

showsZP12 :: Int64 -> ShowS
showsZP12 n
    | n < 10 = (++) "00000000000" . shows n
    | n < 100 = (++) "0000000000" . shows n
    | n < 1000 = (++) "000000000" . shows n
    | n < 10000 = (++) "00000000" . shows n
    | n < 100000 = (++) "0000000" . shows n
    | n < 1000000 = (++) "000000" . shows n
    | n < 10000000 = (++) "00000" . shows n
    | n < 100000000 = (++) "0000" . shows n
    | n < 1000000000 = (++) "000" . shows n
    | n < 10000000000 = (++) "00" . shows n
    | n < 100000000000 = (++) "0" . shows n
    | otherwise = shows n
{-# INLINE showsZP12 #-}
