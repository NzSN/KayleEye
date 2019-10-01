-- file: Time.hs

module Time where

import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime

import Text.Regex.TDFA

data TimeOfDay' = TimeOfDay' { tod :: TimeOfDay, tz :: Int } deriving Show

-- String's format is : HH:MMTZ8 or HH:MMTZ-1
timeStrRegex =  "([0-9]+):([0-9]+)TZ([0-9]+|\\-[0-9]+)"
strToLocalTime :: String -> TimeOfDay'
strToLocalTime tStr =
  let (_, _, _, tod) = tStr =~ timeStrRegex :: (String, String, String, [String])
      hour = read (head tod) :: Int
      min  = read (head . tail $ tod) :: Int
      tz   = read (last tod) :: Int
  in TimeOfDay' (TimeOfDay hour min 0) tz
