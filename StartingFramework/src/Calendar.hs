{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Calendar where

import ParseLib
import DateTime
import Data.List.Split
import Data.List

-- Exercise 6
data Calendar = Calendar { prop1 :: CalProp, prop2 :: CalProp, events :: [Event] }
    deriving (Eq, Ord, Show)

data CalProp = CalPropID { prodID :: ProdID } | CalPropVersion { version :: Version }
    deriving (Eq, Ord, Show)

data ProdID = ProdID { runProdID :: String }
    deriving (Eq, Ord, Show)

data Version = Version
    deriving (Eq, Ord, Show)

data Event = Event { eventProps :: [EventProp] }
    deriving (Eq, Ord, Show)

data EventProp = PropDtStamp { dtStamp :: DtStamp } | PropUid {uid :: Uid} 
    | PropDtStart {dtStart :: DtStart} | PropDtEnd {dtEnd :: DtEnd} 
    | PropDescription {description :: Description} | PropSummary {summary :: Summary} 
    | PropLocation {location :: Location}
  deriving (Eq, Ord, Show)

data DtStamp = DtStamp {runDtStamp :: DateTime}
    deriving (Eq, Ord, Show)

data Uid = Uid {runUid :: String}
    deriving (Eq, Ord, Show)

data DtStart = DtStart{runDtStart :: DateTime}
    deriving (Eq, Ord, Show)

data DtEnd = DtEnd{runDtEnd:: DateTime}
    deriving (Eq, Ord, Show)

data Description = Description{runDescription :: String}
    deriving (Eq, Ord, Show)

data Summary = Summary{runSummary :: String}
    deriving (Eq, Ord, Show)

data Location = Location{runLocation :: String}
    deriving (Eq, Ord, Show)

-- Exercise 7
data Token = Token
    deriving (Eq, Ord, Show)

lexCalendar :: Parser Char [Token]
lexCalendar = undefined

parseCalendar :: Parser Token Calendar
parseCalendar = undefined

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run lexCalendar s >>= run parseCalendar

-- Exercise 8
printCalendar :: Calendar -> String
printCalendar calendar = intercalate "\r\n" $ cutStrings 42 $ splitOn "\r\n" $ printCalendar' calendar
    where 
        cutStrings :: Int -> [String] -> [String]
        cutStrings _ []      = []
        cutStrings x (s : ss) | length s > x = let (s1, s2) = splitAt x s in s1 : cutStrings x (s2 : ss)
                              | otherwise = s : cutStrings x ss

printCalendar' :: Calendar -> String
printCalendar' (Calendar prop1 prop2 events) =
    "BEGIN:CALENDAR\r\n" ++ printCalProp prop1 ++ printCalProp prop2 ++ printEvents events ++ "END:CALENDAR\r\n"
    where
        printCalProp :: CalProp -> String
        printCalProp (CalPropID id) = "PRODID:" ++ runProdID id ++ "\r\n"
        printCalProp (CalPropVersion version) = "VERSION:2.0\r\n"

        printEvents :: [Event] -> String
        printEvents = concatMap printEvent

        printEvent :: Event -> String
        printEvent (Event props) = "BEGIN:EVENT\r\n" ++ concatMap printEventProp props ++ "END:EVENT\r\n"

        printEventProp :: EventProp -> String
        printEventProp eProp = printEventProp' eProp ++ "\r\n"

        printEventProp' :: EventProp -> String
        printEventProp' (PropDtStamp stamp) = "DTSTAMP:" ++ printDateTime (runDtStamp stamp)
        printEventProp' (PropUid uid) = "UID:" ++ runUid uid
        printEventProp' (PropDtStart start) = "DTSTART:" ++ printDateTime (runDtStart start)
        printEventProp' (PropDtEnd end) = "DTEND:" ++ printDateTime (runDtEnd end)
        printEventProp' (PropDescription descr) = "DESCRIPTION:" ++ runDescription descr
        printEventProp' (PropSummary summ) = "SUMMARY:" ++ runSummary summ
        printEventProp' (PropLocation loc) = "LOCATION:" ++ runLocation loc