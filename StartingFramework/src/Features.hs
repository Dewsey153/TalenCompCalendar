{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Features where

import DateTime
import Calendar
import Data.List
import Data.Maybe
import GHC.Base (undefined)
import GHC.Float (double2Float)

-- Exercise 9
-- Count the number of events in the calendar
countEvents :: Calendar -> Int
countEvents calendar = length $ events calendar

-- Find all events which take place on a calendar at a given DateTime
findEvents :: DateTime -> Calendar -> [Event]
findEvents dt calendar = filter (dateTimeInEvent dt) allEvents
    where
        allEvents = events calendar

-- True if the given date time is during the event
dateTimeInEvent :: DateTime -> Event -> Bool
dateTimeInEvent dt e =
    let
        eventTimes = startEndDateTime e
    in
        dateTimeBetween dt eventTimes

-- True if the first DateTime falls between the range (start inclusive, end exclusive)
-- given by the tuple. Assume that start is before end.
dateTimeBetween :: DateTime -> (DateTime, DateTime) -> Bool
dateTimeBetween dateTime (start, end) = start <= dateTime && dateTime < end

-- Get start and end DateTime from Event
startEndDateTime :: Event -> (DateTime, DateTime)
startEndDateTime event = (startDateTime event, endDateTime event)

startDateTime :: Event -> DateTime
startDateTime event =
    let
         start = dtStart event
    in
        runDtStart start

endDateTime :: Event -> DateTime
endDateTime event =
    let
        end   = dtEnd event
    in runDtEnd end

-- True if any event overlaps with another
checkOverlapping :: Calendar -> Bool
checkOverlapping calendar = any (eventOverlaps calendar) (events calendar)
    where
        -- True if given event overlaps with another from the calendar.
        -- Only check if start time is between any other event.
        -- If this is the case, events do overlap.
        -- If this is not the case, events do not overlap.
        eventOverlaps :: Calendar -> Event -> Bool
        eventOverlaps c e = any (\ev -> ev /= e && dateTimeInEvent (startDateTime e) ev) (events c)

-- Checks for a given summary how many time is spent on all events with said summary.
timeSpent :: String -> Calendar -> Int
timeSpent summ cal = sum $ map timeSpentEvent matchingEvents
    where matchingEvents = eventsWithSummary summ cal

-- Returns the amount of minutes spent on the given event
timeSpentEvent :: Event -> Int
timeSpentEvent e = timeSpanToMinutes d1 d2
    where
        d1 = runDtStart(dtStart e)
        d2 = runDtEnd(dtEnd e)

-- Get list of events from calendar with the given summary
eventsWithSummary :: String -> Calendar -> [Event]
eventsWithSummary sum cal = filter
    (\e ->
    let summary = getEventSummary e
    in isJust summary && sum == runSummary (fromJust summary))
    (events cal)

-- Get the summary from the event, if any exists
getEventSummary :: Event -> Maybe Summary
getEventSummary = summary

-- Calculates the amount of minutes between 2 datetimes.
timeSpanToMinutes :: DateTime -> DateTime -> Int
timeSpanToMinutes x y = dateMinutes dateTime2 - dateMinutes dateTime1
    where
        dateTime1 = min x y
        dateTime2 = max x y
        daysInMonth :: Year -> Int -> Int
        daysInMonth year month
            | month == 2 && leapYear year = 29
            | month == 2 = 28
            | month == 4 || month == 6 || month == 9 || month == 11 = 30
            | otherwise = 31
        daysInYear :: Int -> Int
        daysInYear y
            | leapYear (Year y) = 366
            | otherwise = 365
        dateMinutes :: DateTime -> Int
        dateMinutes d =
            let totalDays = sum [daysInYear y | y <- [1..(runYear(year thisDate)) - 1]] + sum [daysInMonth (year thisDate) m | m <- [1..(runMonth(month thisDate)-1)]] + (runDay(day thisDate) - 1)
                totalMinutes = totalDays * 1440 + runHour (hour thisTime) * 60 + runMinute(minute thisTime) -- 1440 minutes in a day
            in totalMinutes
                where
                    thisDate = date d
                    thisTime = time d