{-# LANGUAGE TupleSections #-}
module DateTime where

import ParseLib
import Data.Char
import Data.List
import Text.Printf
import Control.Applicative

-- | "Target" datatype for the DateTime parser, i.e, the parser should produce elements of this type.
data DateTime = DateTime { date :: Date
                         , time :: Time
                         , utc  :: Bool }
    deriving (Eq, Ord, Show)

data Date = Date { year  :: Year
                 , month :: Month
                 , day   :: Day }
    deriving (Eq, Ord, Show)

newtype Year  = Year  { runYear  :: Int } deriving (Eq, Ord, Show)
newtype Month = Month { runMonth :: Int } deriving (Eq, Ord, Show)
newtype Day   = Day   { runDay   :: Int } deriving (Eq, Ord, Show)

data Time = Time { hour   :: Hour
                 , minute :: Minute
                 , second :: Second }
    deriving (Eq, Ord, Show)

newtype Hour   = Hour   { runHour   :: Int } deriving (Eq, Ord, Show)
newtype Minute = Minute { runMinute :: Int } deriving (Eq, Ord, Show)
newtype Second = Second { runSecond :: Int } deriving (Eq, Ord, Show)


-- Exercise 1
-- parse a string describing a DateTime value to DateTime
parseDateTime :: Parser Char DateTime
parseDateTime = (\date _ time utc -> DateTime date time utc) <$> parseDate <*> symbol 'T' <*> parseTime <*> parseUtc
  where
    parseDate :: Parser Char Date
    parseDate = Date <$> parseYear <*> parseMonth <*> parseDay
    parseTime :: Parser Char Time
    parseTime = Time <$> parseHour <*> parseMinute <*> parseSecond
    parseYear :: Parser Char Year
    parseYear = (\x1 x2 x3 x4 -> Year (x1 * 1000 + x2 * 100 + x3 * 10 + x4)) <$> newdigit <*> newdigit <*> newdigit <*> newdigit
    
    parseMonth :: Parser Char Month
    parseMonth = Month <$> parseTwoDigitInt
    parseDay :: Parser Char Day
    parseDay = Day <$> parseTwoDigitInt
    parseHour :: Parser Char Hour
    parseHour = Hour <$> parseTwoDigitInt
    parseMinute :: Parser Char Minute
    parseMinute = Minute <$> parseTwoDigitInt
    parseSecond :: Parser Char Second
    parseSecond = Second <$> parseTwoDigitInt

    -- True if a 'Z' symbol is present, else False
    parseUtc :: Parser Char Bool
    parseUtc = True <$ symbol 'Z' <<|> succeed False

-- parse a number which is two digits long
parseTwoDigitInt :: Parser Char Int
parseTwoDigitInt = (\x1 x2 -> x1 * 10 + x2) <$> newdigit <*> newdigit

-- Exercise 2
-- Run a parser. Return Nothing if the input could not be parsed with an empty tail. 
--  Return the first correct parsing with an empty tail if the input could be parsed.
run :: Parser a b -> [a] -> Maybe b
run parser input =
  let
    result = parse parser input
    convertResult | null result = Nothing
                  | otherwise = do
                    firstFullParse <- find isFullParse result
                    return (fst firstFullParse)
    isFullParse :: (b, [a]) -> Bool
    isFullParse (_, garbage) = null garbage
  in
    convertResult

-- Exercise 3
-- Convert a DateTime into a String
printDateTime :: DateTime -> String
printDateTime (DateTime date time utc) = printDate date ++ "T" ++ printTime time ++ printUtc utc
  where
    printDate :: Date -> String
    printDate date = printYear (year date) ++ printMonth (month date) ++ printDay (day date) 
    printYear :: Year -> String
    printYear year = showFourDigitInt $ runYear year
    printMonth :: Month -> String
    printMonth month = showTwoDigitInt $ runMonth month
    printDay :: Day -> String
    printDay day = showTwoDigitInt $ runDay day

    printTime :: Time -> String
    printTime (Time hour minute second) = printHour hour ++ printMinute minute ++ printSecond second
    printHour :: Hour -> String
    printHour hour = showTwoDigitInt $ runHour hour
    printMinute :: Minute -> String
    printMinute minute = showTwoDigitInt $ runMinute minute
    printSecond :: Second -> String
    printSecond second = showTwoDigitInt $ runSecond second

    printUtc :: Bool -> String
    printUtc value
      | value     = "Z"
      | otherwise = ""

-- Convert an integer with not more than 4 digits into a 4 char long string, 
-- possibly with leading zeros 
showFourDigitInt :: Int -> String
showFourDigitInt = printf "%04d"

-- Does the same as showFourDigitInt, but for for 2 digits in stead of 4 digits.
showTwoDigitInt :: Int -> String
showTwoDigitInt = printf "%02d"

-- Exercise 4
parsePrint :: [Char] -> Maybe String
parsePrint s = fmap printDateTime $ run parseDateTime s

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime datetime = checkDate (date datetime) && checkTime (time datetime)
  where
    checkDate :: Date -> Bool
    checkDate date = checkYear (year date) && checkMonth (month date) && checkDay date

    checkYear :: Year -> Bool
    checkYear year = let ry = runYear year in checkInRange ry (0, 9999)
    checkMonth :: Month -> Bool
    checkMonth month = let m = runMonth month in checkInRange m (1, 12) 
    checkDay :: Date -> Bool
    checkDay d | m == 2 = checkDayFebruary d
               | m == 4 || m == 6 || m == 9 || m == 11 = checkInRange (runDay (day d)) (1, 30)
               | otherwise = checkInRange (runDay (day d)) (1, 31)
      where
        m = runMonth $ month d
    
    checkDayFebruary :: Date -> Bool
    checkDayFebruary date | leapYear (year date) = checkInRange (runDay (day date)) (1, 29)
                          | otherwise = checkInRange (runDay (day date)) (1, 28)

    checkTime :: Time -> Bool
    checkTime time = checkHour (hour time) && checkMinute (minute time) && checkSecond (second time)

    checkHour :: Hour -> Bool
    checkHour hour = let h = runHour hour in checkInRange h (0, 23)
    checkMinute :: Minute -> Bool
    checkMinute minute = let m = runMinute minute in checkInRange m (0, 59)
    checkSecond :: Second -> Bool
    checkSecond second = let s = runSecond second in checkInRange s (0, 59)

-- Check whether x is between l and h. l and h are inclusive.
checkInRange :: Int -> (Int, Int) -> Bool
checkInRange x (l, h) = x >= l && x <= h

leapYear :: Year -> Bool
leapYear y | runYear y `mod` 400 == 0 = True
           | runYear y `mod` 100 == 0 = False
           | runYear y `mod` 4 == 0 = True
           | otherwise = False