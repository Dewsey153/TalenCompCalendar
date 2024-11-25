module Calendar where

import ParseLib
import DateTime


-- Exercise 6
data Calendar = Calendar [Calprop] [Event]
    deriving (Eq, Ord, Show)

data Calprop = Calprop ProdID Version
    deriving (Eq, Ord, Show)

data ProdID = ProdID String
    deriving (Eq, Ord, Show)

data Version = Version
    deriving (Eq, Ord, Show)

data Event = Event [EventProp]
    deriving (Eq, Ord, Show)

data EventProp = PropDtStamp DtStamp | PropUid Uid | PropDtStart DtStart 
  | PropDtEnd DtEnd | PropDescription Description | PropSummary Summary 
  | PropLocation Location
  deriving (Eq, Ord, Show)

data DtStamp = DtStamp DateTime
    deriving (Eq, Ord, Show)

data Uid = Uid String
    deriving (Eq, Ord, Show)

data DtStart = DtStart DateTime
    deriving (Eq, Ord, Show)

data DtEnd = DtEnd DateTime
    deriving (Eq, Ord, Show)

data Description = Description String
    deriving (Eq, Ord, Show)

data Summary = Summary String
    deriving (Eq, Ord, Show)

data Location = Location String
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
printCalendar = undefined
