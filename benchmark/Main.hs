{-# LANGUAGE PackageImports #-}
module Main where

import  Criterion.Main
import  Data.Time.Clock
import  Data.Time.Calendar
import  Data.Time.Clock.POSIX
import  Data.Time.LocalTime
import  Data.Time.Format.Part
import  Data.Time.Format.Locale
import  Data.Time.Clock

import qualified "time" Data.Time.Clock       as O
import qualified "time" Data.Time.Clock.POSIX as O
import qualified "time" Data.Time.LocalTime   as O
import qualified "time" Data.Time.Format      as O
import qualified "time" Data.Time.Clock       as O

main :: IO ()
main = do
    getCurrentTime >>= print
    O.getCurrentTime >>= print
    getPOSIXTime >>= print . posixToUTCTime
    O.getPOSIXTime >>= print . O.posixSecondsToUTCTime
    getZonedTime >>= print
    O.getZonedTime >>= print

    tz <- getCurrentTimeZone
    ct <- getCurrentTime
    otz <- O.getCurrentTimeZone
    oct <- O.getCurrentTime

    zt <- getZonedTime
    let parts = mkParts defaultTimeLocale rfc822DateFormat
        lt = zonedTimeToLocalTime zt
        tod = localTimeOfDay lt

    ozt <- O.getZonedTime
    let oparts = O.rfc822DateFormat
        olt = O.zonedTimeToLocalTime ozt
        otod = O.localTimeOfDay olt

    print $ (formatTimeParts defaultTimeLocale parts) ct
    print $ (formatTimeParts defaultTimeLocale parts) zt
    print $ (O.formatTime O.defaultTimeLocale oparts) oct
    print $ (O.formatTime O.defaultTimeLocale oparts) ozt

    defaultMain
        [ bgroup "new"
            [ bench "getCurrentTime" $ nfIO getCurrentTime
            , bench "formatParts@UTCTime" $ nf (formatTimeParts defaultTimeLocale parts) ct
            , bench "formatParts@ZonedTime" $ nf (formatTimeParts defaultTimeLocale parts) zt
            , bench "formatParts@LocalTime" $ nf (formatTimeParts defaultTimeLocale parts) lt
            , bench "formatParts@TimeOfDay" $ nf (formatTimeParts defaultTimeLocale parts) tod
            , bench "getPOSIXTime" $ nfIO getPOSIXTime
            , bench "getZonedTime" $ nfIO getZonedTime
            ]
        ,
          bgroup "old"
            [ bench "getCurrentTime" $ nfIO O.getCurrentTime
            , bench "utcToZonedTime" $ nf (O.utcToZonedTime O.utc) oct
            , bench "formatTime@UTCTime" $ nf (O.formatTime O.defaultTimeLocale oparts) oct
            , bench "formatTime@ZonedTime" $ nf (O.formatTime O.defaultTimeLocale oparts) ozt
            , bench "formatTime@LocalTime" $ nf (O.formatTime O.defaultTimeLocale oparts) olt
            , bench "formatTime@TimeOfDay" $ nf (O.formatTime O.defaultTimeLocale oparts) otod
            , bench "getPOSIXTime" $ nfIO O.getPOSIXTime
            , bench "getZonedTime" $ nfIO O.getZonedTime
            ]
        ]

