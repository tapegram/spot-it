module Main (main) where

import qualified Data.Set as Set
import qualified Data.Text as Text

-- Modeling the Game
newtype Card = Card (Set.Set Text.Text)

newtype Game = Game (Set.Set Card)

-- Implement necessary Typeclasses for our models so
-- we can do some basic behaviors like print the models to the screen
-- or sort them or determine equality

-- Tell Haskell how to render a Card
instance Show Card where
  show (Card s) = show $ Set.toList s

-- Tell Haskell how to render a Game
instance Show Game where
  show (Game cards) = show $ Set.toList cards

-- Tell Haskell how to order Cards
instance Ord Card where
  (Card as) <= (Card bs) = as <= bs

-- Tell Haskell how to determine if two cards are "equal"
instance Eq Card where
  (Card as) == (Card bs) = as == bs

-- Helper functions to provde some shorthand for instantiating our datatypes

card :: [Text.Text] -> Card
card xs = Card $ Set.fromList xs

game :: [Card] -> Game
game cards = Game $ Set.fromList cards

toSet :: [String] -> Set.Set Text.Text
toSet xs = Set.fromList $ map Text.pack xs

-- Our main algorithm functions

generateSportItCards :: Set.Set Text.Text -> Game
generateSportItCards _ = exampleGame

generateCardsWithSize :: Int -> Set.Set Text.Text -> Set.Set Card
generateCardsWithSize size symbols =
  let go :: State -> Set.Set Card -> Set.Set Card
      go state@(State size _ _) cards =
        if cardIsSize card' size
          then go newState (Set.insert card' cards)
          else cards
        where
          (newState, card') = makeCard state

      initState = State size (Set.drop 1 symbols) (Set.take 1 symbols)
   in go initState Set.empty

data State = State Int (Set.Set Text.Text) (Set.Set Text.Text)

makeCard :: State -> (State, Card)
makeCard (State size unusedSymbols onceUsedSymbols) =
  let unmatched = Set.take (size - 1) unusedSymbols
      match = Set.take 1 onceUsedSymbols
      newState = State size (Set.drop (size - 1) unusedSymbols) (Set.union (Set.drop 1 onceUsedSymbols) unmatched)
      card = Card (Set.union match unmatched)
   in (newState, card)

initialState :: Int -> Set.Set Text.Text -> State
initialState size symbols = State size symbols Set.empty

-- Some analysis functions

cardIsSize :: Card -> Int -> Bool
cardIsSize (Card as) size = Set.size as == size

hasExactlyOneInCommon :: Card -> Card -> Bool
hasExactlyOneInCommon (Card as) (Card bs) = isOne $ Set.size $ Set.intersection as bs

getMatch :: Card -> Card -> Maybe Text.Text
getMatch a b
  | Set.size matches == 1 = Just $ Set.elemAt 0 matches
  | otherwise = Nothing
  where
    matches = getMatches a b

getMatches :: Card -> Card -> Set.Set Text.Text
getMatches (Card as) (Card bs) =
  Set.intersection as bs

isOne :: Int -> Bool
isOne 1 = True
isOne _ = False

-- Example data to use while testing / developing
hand = "🤟"

eye = "👁"

brain = "🧠"

tooth = "🦷"

bone = "🦴"

muscle = "💪"

baby = "👶"

ball = "⚽️"

violin = "🎻"

watch = "⌚️"

phone = "📱"

laptop = "💻"

keyboard = "⌨️"

printer = "🖨"

joystick = "🕹"

cd = "💽"

floppydisk = "💾"

vhs = "📼"

camera = "📷"

symbols :: Set.Set Text.Text
symbols =
  toSet
    [ "hand",
      "eye",
      "brain",
      "tooth",
      "bone",
      "muscle",
      "baby",
      "ball",
      "violin",
      "watch",
      "phone",
      "laptop",
      "keyboard",
      "printer",
      "joystick",
      "cd",
      "floppydisk",
      "vhs",
      "camera"
    ]

letters :: Set.Set Text.Text
letters = toSet ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"]

exampleCard1 :: Card
exampleCard1 = Card (toSet ["bird", "cat", "eyeball"])

exampleCard2 :: Card
exampleCard2 = Card (toSet ["dog", "bird", "cactus"])

exampleGame :: Game
exampleGame = Game (Set.fromList [exampleCard1, exampleCard2])

main :: IO ()
main = do
  putStrLn "hello world"