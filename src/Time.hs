-- file: Time.hs

module Time where

import Test.HUnit

import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime

import Text.Regex.TDFA

data TimeOfDay' = TimeOfDay' { tod :: TimeOfDay, tz :: TimeZone } | Empty_Tod deriving Show

instance Eq TimeOfDay' where
  t1 == t2 = (toMins $ toUtc t1) == (toMins $ toUtc t2)
  t1 /= t2 = not $ t1 == t2

instance Ord TimeOfDay' where
  t1 < t2 = (toMins $ toUtc t1) < (toMins $ toUtc t2)

  t1 <= t2 = t1 == t2 || t1 < t2

  t1 > t2 = (toMins $ toUtc t1) > (toMins $ toUtc t2)
  t1 >= t2 = t1 == t2 || t1 > t2

  max t1 t2 = if t1 > t2 then t1 else t2
  min t1 t2 = if t1 < t2 then t1 else t2

class TimeDiff a where
  (-) :: a -> a -> Int

instance TimeDiff TimeOfDay' where
  t1 - t2 = let t1_utc = toUtc t1
                t2_utc = toUtc t2

                t1_hour = todHour $ tod t1_utc
                t1_min  = todMin $ tod t1_utc
                t2_hour = todHour $ tod t2_utc
                t2_min  = todMin $ tod t2_utc

                t1_total_mins = t1_hour * 60 + t1_min
                t2_total_mins = t2_hour * 60 + t2_min

                total_mins = if t1_total_mins < t2_total_mins
                             then t1_total_mins + 24 * 60
                             else t1_total_mins
            in total_mins Prelude.- t2_total_mins

toMins :: TimeOfDay' -> Integer
toMins t =
  let tod' = tod t
  in toInteger $ (todHour tod') * 60 + (todMin tod')

toUtc :: TimeOfDay' -> TimeOfDay'
toUtc t =
  let tzMin = timeZoneMinutes $ tz t
      hourOffset = tzMin `div` 60
      minOffset = tzMin `mod` 60

      hour = todHour $ tod t
      min  = todMin $ tod t
  in TimeOfDay' (TimeOfDay (hour + hourOffset) (min + minOffset) 0) utc

-- String's format is : HH:MMTZ8 or HH:MMTZ-1
timeStrRegex =  "([0-9]+):([0-9]+)TZ([0-9]+|\\-[0-9]+)"
strToLocalTime :: String -> TimeOfDay'
strToLocalTime tStr =
  let (_, _, _, tod) = tStr =~ timeStrRegex :: (String, String, String, [String])
      hour = read (head tod) :: Int
      min  = read (head . tail $ tod) :: Int
      tz   = read (last tod) :: Int
  in TimeOfDay' (TimeOfDay hour min 0) (hoursToTimeZone tz)

getTimeNow :: IO TimeOfDay'
getTimeNow = do
  time <- getCurrentTime

  let tod_utc = timeToTimeOfDay (utctDayTime time)

  return $ TimeOfDay' tod_utc utc

isTimeInInterval :: TimeOfDay' -- The time be checked
                 -> TimeOfDay' -- Begin of time interval
                 -> TimeOfDay' -- End of time interval
                 -> Bool
isTimeInInterval t begin end = t > begin && t < end

-- Test cases
timeTest :: Test
timeTest = TestList [TestLabel "Time Testing" (TestCase timeAssert)]
  where timeAssert :: Assertion
        timeAssert = do
          let time1 = strToLocalTime "10:21TZ8"
              time2 = strToLocalTime "11:21TZ8"
              time3 = strToLocalTime "12:21TZ8"
          assertEqual "Time diff" 60 $ time2 Time.- time1
          assertEqual "Time diff" 60 $ time3 Time.- time2
          assertEqual "Time diff" 1320 $ time1 Time.- time3

          assertEqual "Time Ord" True $ (time2 > time1) && (time3 > time2)
          assertEqual "Time Ord" True $ (time1 < time2) && (time2 < time3)

          assertEqual "Time range" True $ isTimeInInterval time2 time1 time3
