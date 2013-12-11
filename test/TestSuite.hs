import Test.Hspec
import Test.QuickCheck
import BerlinUhr.Main
import Data.Time.LocalTime

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

  describe "rowState should" $ do
    it "turn the correct number of lights on and decrement the time correctly" $ do
      (rowState (TimeOfDay 10 0 0) [hoursLight 5, hoursLight 5, hoursLight 5]) `shouldBe` ((TimeOfDay 0 0 0),
                                                                                           [(Light Red 5 Hours True), (Light Red 5 Hours True), (Light Red 5 Hours False)])
    it "turn no lights on if lights represent more time than what is remaining" $ do
      (rowState (TimeOfDay 4 0 0) [hoursLight 5, hoursLight 5]) `shouldBe` ((TimeOfDay 4 0 0), [hoursLight 5, hoursLight 5])

  describe "berlinUhrAsString should" $ do
    it "return [Y,OOOO,OOOO,OOOOOOOOOOO,OOOO] for '00:00:00'" $ do
      (berlinUhrAsString $ TimeOfDay 0 0 0) `shouldBe` ["Y","OOOO","OOOO","OOOOOOOOOOO","OOOO"]
    it "return [O,RROO,RRRO,YYROOOOOOOO,YYOO] for '13:17:01'" $ do
      (berlinUhrAsString $ TimeOfDay 13 17 1) `shouldBe` ["O", "RROO", "RRRO", "YYROOOOOOOO", "YYOO"]
    it "return [O,RRRR,RRRO,YYRYYRYYRYY,YYYY] for '23:59:59'" $ do
      (berlinUhrAsString $ TimeOfDay 23 59 59) `shouldBe` ["O","RRRR","RRRO","YYRYYRYYRYY","YYYY"]
