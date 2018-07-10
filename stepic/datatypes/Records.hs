module Records where

import Data.Time.Clock (UTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

data Person = Person { firstName :: String, lastName :: String, age :: Int}
  deriving (Show, Eq)

john = Person "John" "Smith" 24
john1 = Person "" "Smith" 24


{-
Определите тип записи, который хранит элементы лога. Имя конструктора должно совпадать с именем типа, и запись должна содержать три поля:
timestamp — время, когда произошло событие (типа UTCTime);
logLevel — уровень события (типа LogLevel);
message — сообщение об ошибке (типа String).
Определите функцию logLevelToString, возвращающую текстуальное представление типа LogLevel, и функцию logEntryToString, возвращающую текстуальное представление записи в виде:

<время>: <уровень>: <сообщение>


Для преобразование типа UTCTime в строку используйте функцию timeToString.
-}


timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info deriving (Show)

data LogEntry = LogEntry {timestamp :: UTCTime, logLevel :: LogLevel, message :: String}

instance Show LogEntry where
  show (LogEntry ts level message) =
    let
      a = timeToString ts
      b = logLevelToString level
    in a ++ ": " ++ b ++ ": " ++ message

logLevelToString :: LogLevel -> String
logLevelToString = show

logEntryToString :: LogEntry -> String
logEntryToString = show

--

xavier = Person {age = 40, firstName = "Phod", lastName = "Xaavier"}
unknownPerson = Person {age = 30}

updateAge :: Int -> Person -> Person
updateAge newAge p = p {age = newAge}

updateLastName :: Person -> Person -> Person
updateLastName p1 p2 = p2 {lastName = lastName p1}

-- Pattern matching with records

name'' :: Person -> String
name'' (Person {lastName = ln, firstName = fn}) = fn ++ " " ++ ln

--
data Shape = Circle Double | Rectangle Double Double

isRectangle :: Shape -> Bool
isRectangle Rectangle{} = True
isRectangle _ = False

--

abbrFirstName :: Person -> Person
abbrFirstName person@(Person [] _ _) = person
abbrFirstName person@(Person (n : []) _ _) = person{firstName = n : []}
abbrFirstName person@(Person (n : d : xs) _ _) = person{firstName = n : '.' : []}

