import System
import Text.Parsec
import Text.Parsec.String

data Month = Jan | Feb | Mar | Apil | May deriving (Show, Read)
data Date  = Date {
    month :: Month,
    day   :: Integer
} deriving (Show, Read)

data Time  = Time {
    hour :: Integer,
    minute :: Integer,
    second :: Integer
} deriving Show

data LogItem = LogItem {
    date :: Date,
    time :: Time,
    hostname :: String,
    process :: String,
    pid :: Integer,
    message :: String 
} deriving Show

dateParser :: Parser Date
dateParser = do {
    m <- many1 letter
        ; char ' '
        ; d <- many1 digit
        ; return $ Date (read m) (read d)
}

timeParser :: Parser Time
timeParser = do {
    h <- many1 digit
    ; char ':'
    ; m <- many1 digit
    ; char ':'
    ; s <- many1 digit
    ; return $ Time (read h) (read m) (read s)
}

bracketedValue :: Parser String
bracketedValue = do {
    char '['
    ; content <- many (noneOf "]")
    ; char ']'
    ; return content }

logParser = do {
    date <- dateParser
    ; space
    ; time <- timeParser
    ; space
    ; hostname <- many1 anyChar
    ; space
    ; process <- many1 letter 
    ; pid <- bracketedValue
    ; char ':'
    ; space
    ; msg <- many1 digit
    ; return $ ((((((LogItem $ date) time) hostname) process) (read pid)) msg)
}
-- May 15 00:30:07 Lin-Xiangdongs-iMac newsyslog[1362]: logfile turned over
mainParser = logParser

play :: String -> IO ()
play inp = case parse mainParser "" inp of 
            { Left err -> print err
            ; Right ans -> print ans
            }


main :: IO()
main = do {
    args <- getArgs
        ; val <- parseFromFile mainParser $ args !! 0
        ; print(val) }
