{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Calendar where

import ParseLib
import DateTime
import Data.List.Split
import Data.List
import Control.Applicative
import Data.Char

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
data Token = Begin | End | Title String | Content String | Section Section
    deriving (Eq, Ord, Show)

data Section = VCalendar | VEvent
    deriving (Eq, Ord, Show)

lexCalendar :: Parser Char [Token]
lexCalendar = greedy parseToken
    where
        -- input contains "\r\n"
        parseToken :: Parser Char Token
        -- If the text is not a title, check whether it is a section. If not, it must be content.
        -- This is to prevent that sections or titles get parsed as content.
        parseToken = parseBegin <|> parseEnd <|> (parseTitle <<|> (parseSection <<|> parseContent))

        parseBegin :: Parser Char Token
        parseBegin =  Begin <$ token "BEGIN:"
        parseEnd :: Parser Char Token
        parseEnd =  End <$ token "END:"
        parseTitle :: Parser Char Token
        parseTitle = Title <$> greedy1 (satisfy isUpper) <* symbol ':'
        parseContent :: Parser Char Token
        parseContent = Content <$> parseText
        parseSection :: Parser Char Token
        parseSection = (succeed (Section VCalendar) <* token "VCALENDAR\r\n")
            <|> (succeed (Section VEvent) <* token "VEVENT\r\n")

-- Parse a text ending on \r\n possibly on multiple lines, with a space after every "\r\n".
parseText :: Parser Char String
parseText = (++) <$> greedy (satisfy (/='\r')) <*> ((token "\r\n " *> parseText) <<|> "" <$ token "\r\n")

parseCalendar :: Parser Token Calendar
parseCalendar = undefined

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run lexCalendar s >>= run parseCalendar

-- Exercise 8
-- Turns a calendar into a valid input string
printCalendar :: Calendar -> String
printCalendar calendar = cutLongLines 42 $ printCalendar' calendar
    where 
        -- If any line (characters between occurences of "\r\n") is beyond x 
        -- chars long, add "\r\n " to avoid long lines.
        cutLongLines :: Int -> String -> String
        cutLongLines x = intercalate "\r\n" . map (cutString x) . splitOn "\r\n"
        
        -- If the string is beyond x chars long, add "\r\n " after x characters.
        -- Do this until each line is max 42 chars long. 
        cutString :: Int -> String -> String
        cutString x s   | length s > x = let (s1, s2) = splitAt x s in s1 ++ "\r\n" ++ cutString x (' ': s2)
                        | otherwise    = s

        -- Actually turns a calendar into a string
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