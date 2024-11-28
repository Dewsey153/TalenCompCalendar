{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use void" #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Calendar where

import ParseLib
import DateTime
import Data.List.Split
import Data.List
import Control.Applicative
import Data.Char
import Control.Monad (replicateM)
import Data.Maybe

-- Exercise 6
data Calendar = Calendar { prodID :: ProdID, version :: Version, events :: [Event] }
    deriving (Eq, Ord, Show)

data CalProp = PropProdID ProdID | PropVersion Version

data ProdID = ProdID { runProdID :: String }
    deriving (Eq, Ord, Show)

data Version = Version
    deriving (Eq, Ord, Show)

data Event = Event { dtStamp :: DtStamp, uid :: Uid,  dtStart :: DtStart, 
    dtEnd :: DtEnd, description :: Maybe Description, summary :: Maybe Summary,
    location :: Maybe Location }
    deriving (Eq, Ord, Show)

data EventProp = PropDtStamp DtStamp | PropUid Uid | PropDtStart DtStart 
    | PropDtEnd DtEnd | PropDescription Description | PropSummary Summary
    | PropLocation Location

data DtStamp = DtStamp {runDtStamp :: DateTime}
    deriving (Eq, Ord, Show)

data Uid = Uid {runUid :: String}
    deriving (Eq, Ord, Show)

data DtStart = DtStart {runDtStart :: DateTime}
    deriving (Eq, Ord, Show)

data DtEnd = DtEnd {runDtEnd:: DateTime}
    deriving (Eq, Ord, Show)

data Description = Description {runDescription :: String}
    deriving (Eq, Ord, Show)

data Summary = Summary {runSummary :: String}
    deriving (Eq, Ord, Show)

data Location = Location {runLocation :: String}
    deriving (Eq, Ord, Show)

-- Exercise 7
data Token = Begin | End | Title String | Content String | Section Section 
    | DateTimeToken DateTime
    deriving (Eq, Ord, Show)

data Section = VCalendar | VEvent
    deriving (Eq, Ord, Show)

lexCalendar :: Parser Char [Token]
lexCalendar = greedy lexToken
    where
        -- input contains "\r\n"
        lexToken :: Parser Char Token
        -- If the text is not a title, check whether it is a section. If not, it must be content.
        -- This is to prevent that sections or titles get parsed as content.
        lexToken = lexBegin <|> lexEnd <|> (lexTitle <<|> (lexSection <<|> (lexDateTime <<|> lexContent)))

        lexBegin :: Parser Char Token
        lexBegin =  Begin <$ token "BEGIN:"
        lexEnd :: Parser Char Token
        lexEnd =  End <$ token "END:"
        lexTitle :: Parser Char Token
        lexTitle = Title <$> greedy1 (satisfy isUpper) <* symbol ':'
        lexContent :: Parser Char Token
        lexContent = Content <$> parseText
        lexSection :: Parser Char Token
        lexSection = (succeed (Section VCalendar) <* token "VCALENDAR\r\n")
            <|> (succeed (Section VEvent) <* token "VEVENT\r\n")
        lexDateTime :: Parser Char Token
        lexDateTime = DateTimeToken <$> (parseDateTime <* token "\r\n")

-- Parse a text ending on \r\n possibly on multiple lines, with a space after every "\r\n".
parseText :: Parser Char String
parseText = (++) <$> greedy (satisfy (/='\r')) <*> ((token "\r\n " *> parseText) <<|> "" <$ token "\r\n")

parseCalendar :: Parser Token Calendar
parseCalendar = pack parseBeginCalendar parseCalendar' parseEndCalendar
    where
        parseBeginCalendar :: Parser Token Token
        parseBeginCalendar = symbol Begin *> symbol (Section VCalendar)
        
        parseEndCalendar :: Parser Token Token
        parseEndCalendar = symbol End *> symbol (Section VCalendar)

        parseCalendar' :: Parser Token Calendar
        parseCalendar'= toCalendar <$> (parseProdID <|> parseVersion) <*> (parseProdID <|> parseVersion) <*> many parseEvent

        -- Provided id or version in any order and a list of event, returns calendar
        toCalendar :: CalProp -> CalProp -> [Event] -> Calendar
        toCalendar (PropProdID id) (PropVersion version) events = Calendar id version events
        toCalendar (PropVersion version) (PropProdID id) events = Calendar id version events
        toCalendar _ _ _ = error "Calendar id or version could not be found." 
        
        parseProdID :: Parser Token CalProp
        parseProdID = (\(Content x) -> PropProdID (ProdID x)) <$> (symbol (Title "PRODID") *> anySymbol)
        parseVersion :: Parser Token CalProp
        parseVersion = PropVersion Version <$ (symbol (Title "VERSION") *> symbol (Content "2.0"))

parseEvent :: Parser Token Event
parseEvent = pack parseBeginEvent parseEvent' parseEndEvent
    where
        parseBeginEvent :: Parser Token Token
        parseBeginEvent = symbol Begin *> symbol (Section VEvent)

        parseEvent' :: Parser Token Event
        parseEvent' = ePropsToEvent <$> many parseEventProp

        ePropsToEvent :: [EventProp] -> Event
        ePropsToEvent eventprops = Event eDtStamp eUid eDtStart eDtEnd eDescription eSummary eLocation
            where
                eDtStamp | length dtStamps == 1 = toDtStamp (head dtStamps)
                         | null dtStamps = error "No dtStamp found." 
                         | otherwise = error "Multiple dtStamps found."
                
                dtStamps = filter (\case
                                    PropDtStamp ep -> True
                                    _ -> False) eventprops

                toDtStamp :: EventProp -> DtStamp
                toDtStamp (PropDtStamp stamp) = stamp
                toDtStamp _ = error "Input is not a DtStamp."




                eUid | length uids == 1 = toUid (head uids)
                     | null uids = error "No uid found." 
                     | otherwise = error "Multiple uids found."
                
                uids = filter (\case
                                    PropUid _ -> True
                                    _ -> False) eventprops

                toUid :: EventProp -> Uid
                toUid (PropUid id) = id
                toUid _ = error "Input is not a uid."


                
                eDtStart | length dtStarts == 1 = toDtStart (head dtStarts)
                     | null dtStarts = error "No dtStart found." 
                     | otherwise = error "Multiple dtStarts found."
                
                dtStarts = filter (\case
                                    PropDtStart _ -> True
                                    _ -> False) eventprops

                toDtStart :: EventProp -> DtStart
                toDtStart (PropDtStart start) = start
                toDtStart _ = error "Input is not a dtStart."


                
                eDtEnd | length dtEnds == 1 = toDtEnd (head dtEnds)
                     | null dtEnds = error "No dtEnd found." 
                     | otherwise = error "Multiple dtEnds found."
                
                dtEnds = filter (\case
                                    PropDtEnd _ -> True
                                    _ -> False) eventprops

                toDtEnd :: EventProp -> DtEnd
                toDtEnd (PropDtEnd end) = end
                toDtEnd _ = error "Input is not a dtEnd."



                eDescription :: Maybe Description
                eDescription | length descriptions == 1 = Just $ toDescription (head descriptions)
                     | null descriptions = Nothing
                     | otherwise = error "Multiple descriptions found."
                
                descriptions = filter (\case
                                    PropDescription _ -> True
                                    _ -> False) eventprops

                toDescription :: EventProp -> Description
                toDescription (PropDescription descr) = descr
                toDescription _ = error "Input is not a description."




                eSummary :: Maybe Summary
                eSummary | length summaries == 1 = Just $ toSummary (head summaries)
                         | null summaries = Nothing
                         | otherwise = error "Multiple summaries found."
                
                summaries = filter (\case
                                    PropSummary _ -> True
                                    _ -> False) eventprops

                toSummary :: EventProp -> Summary
                toSummary (PropSummary sum) = sum
                toSummary _ = error "Input is not a summary."




                eLocation :: Maybe Location
                eLocation | length locations == 1 = Just $ toLocation (head locations)
                         | null locations = Nothing
                         | otherwise = error "Multiple locations found."
                
                locations = filter (\case
                                    PropLocation _ -> True
                                    _ -> False) eventprops

                toLocation :: EventProp -> Location
                toLocation (PropLocation loc) = loc
                toLocation _ = error "Input is not a location."

        parseEventProp :: Parser Token EventProp
        parseEventProp = parsePropDtStamp <|> parsePropUid <|> parsePropDtStart
            <|> parsePropDtEnd <|> parsePropDescription <|> parsePropSummary
            <|> parsePropLocation

        isContent :: Token -> Bool
        isContent (Content _) = True
        isContent _ = False

        isDateTimeToken :: Token -> Bool
        isDateTimeToken (DateTimeToken _) = True
        isDateTimeToken _ = False

        parsePropDtStamp :: Parser Token EventProp
        parsePropDtStamp = stampTokenToProp <$> (symbol (Title "DTSTAMP") *> satisfy isDateTimeToken)

        stampTokenToProp :: Token -> EventProp
        stampTokenToProp (DateTimeToken c) = PropDtStamp (DtStamp c)

        parsePropUid :: Parser Token EventProp
        parsePropUid = uidTokenToProp <$> (symbol (Title "UID") *> satisfy isContent)

        uidTokenToProp :: Token -> EventProp
        uidTokenToProp (Content c) = PropUid (Uid c)

        parsePropDtStart :: Parser Token EventProp
        parsePropDtStart = startTokenToProp <$> (symbol (Title "DTSTART") *> satisfy isDateTimeToken)

        startTokenToProp :: Token -> EventProp
        startTokenToProp (DateTimeToken c) = PropDtStart (DtStart c)

        parsePropDtEnd :: Parser Token EventProp
        parsePropDtEnd = endTokenToProp <$> (symbol (Title "DTEND") *> satisfy isDateTimeToken)

        endTokenToProp :: Token -> EventProp
        endTokenToProp (DateTimeToken c) = PropDtEnd (DtEnd c)

        parsePropDescription :: Parser Token EventProp
        parsePropDescription = descrTokenToProp <$> (symbol (Title "DESCRIPTION") *> satisfy isContent)

        descrTokenToProp :: Token -> EventProp
        descrTokenToProp (Content c) = PropDescription (Description c)

        parsePropSummary :: Parser Token EventProp
        parsePropSummary = summTokenToProp <$> (symbol (Title "SUMMARY") *> satisfy isContent)
        
        summTokenToProp :: Token -> EventProp
        summTokenToProp (Content c) = PropSummary (Summary c)

        parsePropLocation :: Parser Token EventProp
        parsePropLocation = locationTokenToProp <$> (symbol (Title "LOCATION") *> satisfy isContent)

        locationTokenToProp :: Token -> EventProp
        locationTokenToProp (Content c) = PropLocation (Location c)

        parseEndEvent :: Parser Token Token
        parseEndEvent = symbol End *> symbol (Section VEvent)

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
        printCalendar' (Calendar id version events) =
            "BEGIN:CALENDAR\r\n" ++ printProdID id ++ printVersion ++ printEvents events ++ "END:CALENDAR\r\n"
            where
                printProdID :: ProdID -> String
                printProdID (ProdID prodID) = "PRODID:" ++ runProdID id ++ "\r\n"
                printVersion :: String
                printVersion = "VERSION:2.0\r\n"

                printEvents :: [Event] -> String
                printEvents = concatMap printEvent

                printEvent :: Event -> String
                printEvent (Event dtStamp uid dtStart dtEnd description summary location ) = "BEGIN:EVENT\r\n" ++ printDtStamp dtStamp ++ printUID uid ++ printDtStart dtStart ++ printDtEnd dtEnd ++ printDescription description ++ printSummary summary ++ printLocation location ++ "END:EVENT\r\n"
                printDtStamp :: DtStamp -> String
                printDtStamp stamp = "DTSTAMP" ++ printDateTime (runDtStamp stamp) ++ "\r\n"
                printUID :: Uid -> String
                printUID uid = "UID:" ++ runUid uid ++ "\r\n"
                printDtStart :: DtStart -> String
                printDtStart start = "DTSTART:" ++ printDateTime (runDtStart start) ++ "\r\n"
                printDtEnd :: DtEnd -> String
                printDtEnd end = "DTEND:" ++ printDateTime (runDtEnd end) ++ "\r\n"
                printDescription :: Maybe Description -> String
                printDescription descr = case descr of
                                            Just descr -> "DESCRIPTION:" ++ runDescription descr ++ "\r\n"
                                            Nothing -> ""
                printSummary :: Maybe Summary -> String
                printSummary summ = case summ of
                                        Just summ -> "SUMMARY:" ++ runSummary summ ++ "\r\n"
                                        Nothing -> ""
                printLocation :: Maybe Location -> String
                printLocation loc = case loc of
                                        Just loc -> "LOCATION:" ++ runLocation loc ++ "\r\n"
                                        Nothing -> ""