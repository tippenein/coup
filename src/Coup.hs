module Coup where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Operational
import Control.Monad.Random.Class
import Control.Monad.State
import Data.IORef
import Data.List
import qualified Data.Map.Strict as M
import System.Random
import System.Random.Shuffle

import Coup.Random

-- https://github.com/paf31/Adventure
-- Character Cards

-- Each card has a total of three cards, resulting in a 15 card deck.
-- each player will get two of these cards face-down.

-- There are five different character cards in the game, each with a unique power.
data Card
  = Ambassador -- Exchange this card with another card in the deck. Block another player from stealing from you
  | Assassin --  Pay 3 coins to assassinate another player, revealing one of their character cards.
  | Captain    -- Steal 2 coins from another player. Block another player from stealing from you.
  | Contessa --  Block a player from assassinating you.
  | Duke       -- Take 3 coins from the treasure. Block another player from using foreign aid.
  deriving (Enum, Show)

-- start with 2 coins for each person
-- takes 7 coins to coup
-- coup forces that player to reveal one of their character cards
-- players can gain more currency from stealing from other players

data Player
  = Player
  { heldCards :: MVar [Card]
  , coins     :: IORef Int
  , id        :: Int
  }

-- Each player can use one "action" per turn.
-- An action is defined as using an ability from one of their character cards
-- getting an income of 1 coin from the treasury
-- getting foreign aid of 2 coins from the treasury.
-- player uses a character card ability or counter, another player can choose
-- to call the acting player out on lying. If they are caught in a lie, they
-- must reveal one of their cards; if they are wrong and the acting player was
-- not lying, then they themselves must reveal a card. Once a player reveals
-- both their cards, they are removed from the game.

data Action
  = Reveal Card
  | Challenge Player
  | DrawNewCard
  | Block
  | Steal Player
  | Assassinate Player
  | Coup Player
  | Coin
  | ForeignAid

-- data GameActions a where
--   GetAction :: PlayerType -> GameActions Action
--   ShowState :: GameActions ()

-- type PlayerState a = State Player a
-- data GameActions a where
--   GetAction :: PlayerState -> GameActions Action
--   ShowState :: GameActions ()

-- type GameState = ProgramT GameActions (State Game)

-- | The 'DeckS' is the current state of the 'Deck'.
type DeckState a = State Deck a

data Deck = Deck
  { cards :: [Card] }
  deriving (Show)

mkDeck :: (MonadRandom m) => m [Card]
mkDeck = shuffleM cards
  where
    cards = [ card | card <- [Ambassador ..], _ <- [1..3] :: [Int] ]


data Game
  = Game
  { activePlayers :: [Player]
  , currentTurn   :: Player
  }
-- |'draw' will take one card off the top of the deck.
draw :: DeckState Card
draw = takeCardAt 0

takeCardAt :: Int -> DeckState Card
takeCardAt i = undefined

-- gameLoop :: GameState ()
gameLoop = undefined

-- runAction :: Player -> Action -> GameState
-- runAction player Coin = player { coins=coins+1}

resolveTurn = undefined

-- newPlayer id = do
--   cs <- newIORef 2
--   card1 <- drawCard
--   card2 <- drawCard
--   Player { coins = cs, id=id, cards=[card1, card2] }

-- allocateDeckCards =
--   newIORef (shuffle allCards)

-- allocatePlayers =
--   p1,p2,p3,p4 <- forM [1..4] newPlayer

-- startGame = do
--   players <- allocatePlayers
--   deck <- allocateDeckCards
--   forever $ while (stillCompeting players) doTurn

main = gameLoop
