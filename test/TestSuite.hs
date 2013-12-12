import Test.Hspec
import Test.QuickCheck
import BerlinUhr.Main
import Data.Time.LocalTime

instance Arbitrary TimeOfDay where
  arbitrary = do
    hh <- choose (0,23) :: Gen Int
    mm <- choose (0,59) :: Gen Int
    return $ TimeOfDay hh mm 00

instance Arbitrary Color where
  arbitrary = elements [Red, Yellow]

instance Arbitrary TimeUnit where
  arbitrary = elements [Hours, Minutes]

instance Arbitrary Light where
  arbitrary = do
    c <- arbitrary :: Gen Color
    tUnit <- arbitrary :: Gen TimeUnit
    time <- choose (0,10) :: Gen Int
    isOn <- arbitrary :: Gen Bool
    return $ Light c time tUnit isOn

main :: IO ()
main = hspec $ do
  describe "The timeParser should" $ do
    it "parse a valid time correctly" $ do
      (parseTime "12:12:12") `shouldBe` (Just (TimeOfDay 12 12 12))
    it "not parse invalid times" $ do
      (parseTime "40:65:89") `shouldBe` Nothing
  describe "A fully lit Berlin Uhr should" $ do
    it "match [[R,R,R,R],[R,R,R,R],[Y,Y,R,Y,Y,R,Y,Y,R,Y,Y],[Y,Y,Y,Y]]" $ do
      do{row <- unlitBerlinUhr; [ map (\light -> show $ light {on = True}) row]} `shouldBe` [["R","R","R","R"],
                                                                                             ["R","R","R","R"],
                                                                                             [ "Y","Y","R","Y","Y","R","Y","Y","R","Y","Y"],
                                                                                             ["Y","Y","Y","Y"]]
  describe "lightState should" $ do
    it "turn on a light and decrement the TimeOfDay if the time remaining is higher than the time represented by the light" $ do
      lightState (TimeOfDay 5 0 0) (hoursLight 3) `shouldBe` ((TimeOfDay 2 0 0), (Light Red 3 Hours True))
    it "turn on a light and decrement the TimeOfDay if the time remaining is equal to the time represented by the light" $ do
      lightState (TimeOfDay 5 0 0) (hoursLight 5) `shouldBe` ((TimeOfDay 0 0 0), (Light Red 5 Hours True))
    it "return the inputs if a light represents more time than what is remaining" $ do
      lightState (TimeOfDay 3 0 0) (hoursLight 5) `shouldBe` ((TimeOfDay 3 0 0), (Light Red 5 Hours False))
    it "also work with decrementing for minutes" $ do
      lightState (TimeOfDay 0 15 0) (Light Yellow 5 Minutes False) `shouldBe` ((TimeOfDay 0 10 0), (Light Yellow 5 Minutes True))
    it "not decrement minutes for lights representing more minutes than the time of day has" $ do
      lightState (TimeOfDay 0 10 0) (Light Yellow 15 Minutes False) `shouldBe` ((TimeOfDay 0 10 0), (Light Yellow 15 Minutes False))
    it "be the inverse of addLightToTime" $ do
      property $ \genTime genLight-> let (time, light) = lightState genTime genLight
                                     in (addLightToTime time light) == genTime

  describe "rowState should" $ do
    it "turn the correct number of lights on and decrement the time correctly" $ do
      (rowState (TimeOfDay 10 0 0) [hoursLight 5, hoursLight 5, hoursLight 5]) `shouldBe` ((TimeOfDay 0 0 0),
                                                                                           [(Light Red 5 Hours True),
                                                                                            (Light Red 5 Hours True),
                                                                                            (Light Red 5 Hours False)])
    it "turn no lights on if lights represent more time than what is remaining" $ do
      (rowState (TimeOfDay 4 0 0) [hoursLight 5, hoursLight 5]) `shouldBe` ((TimeOfDay 4 0 0), [hoursLight 5, hoursLight 5])
    it "be the inverse of addRowToTime" $ do
      property $ \genTime genRow -> let (time, lights) = rowState genTime genRow
                                    in addRowToTime time lights == genTime

  describe "berlinUhr should" $ do
    it "be the inverse of addUhrToTime (be able to be turned back to the same time)" $ do
      property $ \genTime -> let lights = berlinUhr genTime
                             in addUhrToTime (TimeOfDay 0 0 0) lights == genTime
--    it "should have no more lights turned on after the first off light has been encountered" $ do
--      property $ \genTime -> let lights = berlinUhr genTime
    
  describe "berlinUhrAsString should" $ do
    it "return [Y,OOOO,OOOO,OOOOOOOOOOO,OOOO] for '00:00:00'" $ do
      (berlinUhrAsString $ TimeOfDay 0 0 0) `shouldBe` ["Y","OOOO","OOOO","OOOOOOOOOOO","OOOO"]
    it "return [O,RROO,RRRO,YYROOOOOOOO,YYOO] for '13:17:01'" $ do
      (berlinUhrAsString $ TimeOfDay 13 17 1) `shouldBe` ["O", "RROO", "RRRO", "YYROOOOOOOO", "YYOO"]
    it "return [O,RRRR,RRRO,YYRYYRYYRYY,YYYY] for '23:59:59'" $ do
      (berlinUhrAsString $ TimeOfDay 23 59 59) `shouldBe` ["O","RRRR","RRRO","YYRYYRYYRYY","YYYY"]

