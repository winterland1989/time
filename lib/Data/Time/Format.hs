{-# LANGUAGE FlexibleContexts #-}

module Data.Time.Format
    (
    -- * UNIX-style formatting
    FormatTime(..),formatTime,
    module Data.Time.Format.Parse
    ) where

import Data.Time.Format.Parse
import Data.Time.Format.Part

formatTime :: (FormatTime t String) => TimeLocale -> String -> t -> String
formatTime l fmt t = formatTimeParts l (mkParts l fmt) t

