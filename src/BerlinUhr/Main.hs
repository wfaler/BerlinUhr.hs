module BerlinUhr.Main
(Color(..), Light(..),TimeUnit(..), parseTime, unlitBerlinUhr, hoursLight, lightState, rowState, berlinUhr, berlinUhrAsString, printBerlinUhr)
where

import Data.Time.LocalTime
import Data.Attoparsec.Text
import qualified Data.Text as T
import Control.Monad
import Data.Maybe
import Data.Fixed

data TimeUnit = Hours | Minutes deriving(Eq, Show)
data Color = Red | Yellow deriving(Eq, Show)
data Light = Light { color :: Color, timeRep :: Int, timeUnit :: TimeUnit, on :: Bool} deriving(Eq)

instance Show Light where
  show (Light _ _ _ False) = "O"
  show (Light Red _ _ _) = "R"
  show (Light Yellow _ _ _) = "Y"

hoursLight :: Int -> Light
hoursLight hours = Light Red hours Hours False

minutesLight :: Color -> Int -> Light
minutesLight clr minutes = Light clr minutes Minutes False

unlitBerlinUhr :: [[Light]]
unlitBerlinUhr = [firstRow, secondRow, thirdRow, fourthRow]
  where firstRow = map (\_ -> hoursLight 5) ([1..4] :: [Int])
        secondRow = map (\_ -> hoursLight 1) ([1..4] :: [Int])
        thirdRow = map (\lightNo -> if ((rem lightNo 3) == 0) then minutesLight Red 5 else minutesLight Yellow 5) ([1..11] :: [Int])
        fourthRow = map (\_ -> minutesLight Yellow 1) ([1..4] :: [Int])

parseTime :: String -> Maybe TimeOfDay
parseTime input = (join (maybeResult (parse timeParser (T.pack input))))

timeParser :: Parser (Maybe TimeOfDay)
timeParser = do
  hour <- count 2 digit
  _ <- char ':'
  minutes <- count 2 digit
  _ <- char ':'
  seconds <- count 2 digit
  return $ makeTimeOfDayValid (read hour) (read minutes) (read seconds)  

lightState :: TimeOfDay -> Light -> (TimeOfDay, Light)
lightState (TimeOfDay hh mm ss) (Light c t Hours _) = if (hh >= t)
                                                        then ((TimeOfDay (hh - t) mm ss), (Light c t Hours True))
                                                        else ((TimeOfDay hh mm ss), (Light c t Hours False))
lightState (TimeOfDay hh mm ss) (Light c t Minutes _) = if (mm >= t)
                                                           then ((TimeOfDay hh (mm - t) ss), (Light c t Minutes True))
                                                           else ((TimeOfDay hh mm ss), (Light c t Minutes False))

rowState :: TimeOfDay -> [Light] -> (TimeOfDay, [Light])
rowState tme [] = (tme, [])
rowState tme (x:xs) = let (timeLeft, firstLamp) = lightState tme x
                          (timeAfterAllLights, rest) = rowState timeLeft xs
                      in (timeAfterAllLights, firstLamp : rest)

berlinUhr :: TimeOfDay -> [[Light]]
berlinUhr timeOfDay = calcUhr timeOfDay unlitBerlinUhr
                      where calcUhr _ [] = []
                            calcUhr tme (x:xs) = let (remaining, row) = rowState tme x
                                                 in row : (calcUhr remaining xs)

berlinUhrAsString :: TimeOfDay -> [String]
berlinUhrAsString tme = let topLight = if (((read (showFixed True (todSec tme)) :: Integer) `rem` 2) == 0) then "Y" else "O"
                            restOfTheLights =
                              do
                                row <- berlinUhr tme
                                return $ foldl (++) "" $ map show row
                        in topLight : restOfTheLights

printBerlinUhr :: String -> IO ()
printBerlinUhr input = mapM_ putStrLn $ fromMaybe [input ++ " is not a valid Time!"] $ fmap berlinUhrAsString $ parseTime input
