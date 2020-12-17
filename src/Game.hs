{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings#-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Game where

import Control.Monad (forM, forM_, replicateM, replicateM_, guard, mzero, mplus)
import Control.Monad.Trans(MonadIO, lift,liftIO)
import System.Random
import qualified Control.Monad.State as S
import System.Environment (getEnvironment)
import qualified Data.Vector as V
import Data.Vector.Generic.Lens (vectorIx)
import Control.Lens
import Data.Maybe(fromJust)
import Control.Concurrent(threadDelay)
import qualified Data.ByteString.Lazy as LB(toStrict)
import qualified Data.Text.Encoding as Text(encodeUtf8, decodeUtf8)
--import qualified Math.Combinat.Trees.Binary as Tree

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson
import Data.List (null)

import Control.Exception (finally)
import Control.Monad(forever)
import qualified Data.Text as T(Text,pack, unpack)
import Basic


{-data Card = F | X | ADV | (:+:) | One | Two | N | Trash
  deriving (Enum,Show,Eq,Read)-}

data PlayerState = PlayerState {
  _hands :: [Card]
  , _deck :: [Card]
  , _field :: V.Vector (Maybe (MyTree Card, String))
  } deriving (Show,Eq,Read)
makeLenses ''PlayerState

data Game = Game{
  _opponent :: PlayerState
  , _myself :: PlayerState
  , _turn_count :: Int
  } deriving (Show,Eq, Read)
makeLenses ''Game

-- $(Aeson.deriveJSON Aeson.defaultOptions ''Card)
-- $(Aeson.deriveJSON Aeson.defaultOptions ''Tree.Tree)
$(Aeson.deriveJSON Aeson.defaultOptions ''PlayerState)
$(Aeson.deriveJSON Aeson.defaultOptions ''Game)

selectPlayer :: Bool -> Lens' Game PlayerState
selectPlayer True = myself
selectPlayer False = opponent

initDeck :: IO [Card]
initDeck = do
  replicateM 40 $ do
    r <- randomRIO (0, 6) :: IO Int
    return $ toEnum r :: IO Card
  return $ concat $ replicate 100 [(:+:),One,Two,Trash]


-- カードを配る
-- 入力
-- 手札を数字で指定
-- パース

draw :: (Monad m) => S.StateT PlayerState m Card
draw = do
  d <- (^. deck) <$> S.get
  case d of
    [] -> error "deck empty"
    (x:xs) -> do
      deck .= xs
      return x

drawToHand :: (Monad m) => S.StateT PlayerState m ()
drawToHand = do
  c <- draw
  hands %= (c:)
  return ()

initialDraw :: (Monad m) => S.StateT PlayerState m ()
initialDraw = do
  replicateM 20 drawToHand
  return ()

initialPlayer :: IO PlayerState
initialPlayer = do
  d <- initDeck
  return $ PlayerState {
    _hands=[]
    ,_deck=d
    ,_field=V.replicate 5 Nothing
    }

initialGame :: IO Game
initialGame = do
  m <- initialPlayer
  o <- initialPlayer
  return $ Game {
    _opponent = o & S.execState initialDraw
    , _myself = m & S.execState initialDraw
    , _turn_count=0
    }

type Parser a = S.StateT [Card] [] a


isWinning :: Bool -> Game -> Bool
isWinning turn game = Data.List.null $ game ^. (selectPlayer turn . hands)

evalFormula :: MyTree Card -> Int
evalFormula (Node (Leaf (:+:)) [formula1, formula2]) = (evalFormula formula1) + (evalFormula formula2)
evalFormula (Leaf One) = 1
evalFormula (Leaf Two) = 2
evalFormula x = error "is not formula"

pick :: [Card] -> [Int] -> [Card]
pick hands ix = map (hands!!) ix
