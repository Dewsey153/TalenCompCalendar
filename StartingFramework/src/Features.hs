module Features where

import DateTime
import Calendar
import Data.List
import Data.Maybe


-- Exercise 9
-- Count how many events are in the calendar
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

-- True if the first DateTime falls between the range given by the tuple.
-- Assume that start is before end.
dateTimeBetween :: DateTime -> (DateTime, DateTime) -> Bool
dateTimeBetween dateTime (start, end) = start <= dateTime && dateTime <= end

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
        eventOverlaps c e = any (dateTimeInEvent (startDateTime e)) (events c)

timeSpent :: String -> Calendar -> Int
timeSpent sum cal = undefined

-- Get list of events from calendar with the given summary
eventsWithSummary :: String -> Calendar -> [Event]
eventsWithSummary sum cal = filter
    (\e ->
    let summary = getEventSummary e
    in isJust summary && sum == runSummary (fromJust summary))
    (events cal)

-- Get the summary from the event, if any exists
getEventSummary :: Event -> Maybe Summary
getEventSummary event = do
    summary event

timeSpanToMinutes :: DateTime -> DateTime -> Int
timeSpanToMinutes x y = undefined
    where
        dateTime1 = min x y
        dateTime2 = max x y
        date1 = date dateTime1
        date2 = date dateTime2
        time1 = time dateTime1
        time2 = time dateTime2
        dateDifference :: Date -> Date -> Int
        dateDifference d1 d2 = dateMinutes d2 - dateMinutes d1
        dateMinutes :: Date -> Int
        dateMinutes d = undefined -- yearMinutes (year d) + monthMinutes (month d) + runDay (day d) * 1440