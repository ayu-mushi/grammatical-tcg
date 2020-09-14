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
import qualified Math.Combinat.Trees.Binary as Tree

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson

import Control.Exception (finally)
import Control.Monad(forever)
import qualified Data.Text as T(Text,pack, unpack)


data Card = F | X | ADV
  deriving (Enum,Show,Eq,Read)

data PlayerState = PlayerState {
  _hands :: [Card]
  , _deck :: [Card]
  , _field :: V.Vector (Maybe (Tree.BinTree Card, Tree.Dot))
  } deriving (Show,Eq,Read)
makeLenses ''PlayerState

data Game = Game{
  _opponent :: PlayerState
  , _myself :: PlayerState
  , _turn_count :: Int
  } deriving (Show,Eq, Read)
makeLenses ''Game

$(Aeson.deriveJSON Aeson.defaultOptions ''Card)
$(Aeson.deriveJSON Aeson.defaultOptions ''Tree.BinTree)
$(Aeson.deriveJSON Aeson.defaultOptions ''PlayerState)
$(Aeson.deriveJSON Aeson.defaultOptions ''Game)

selectPlayer :: Bool -> Lens' Game PlayerState
selectPlayer True = myself
selectPlayer False = opponent

initDeck :: IO [Card]
initDeck = do
  replicateM 40 $ do
    r <- randomRIO (0, 10) :: IO Int
    return $ if r==9 then ADV else (if r < 5 then F else X)


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

oneCard :: Card -> S.StateT [Card] [] Card
oneCard one = do
  c <- S.get
  case c of
       [] -> mzero
       (x:xs) -> do
         guard $ one == x
         S.put xs
         return x

parseAdv :: Parser (Tree.BinTree Card)
parseAdv = fmap Tree.Leaf $ oneCard ADV

parsePred :: Parser (Tree.BinTree Card)
parsePred = fmap Tree.Leaf $ oneCard F

parseNoun :: Parser (Tree.BinTree Card)
parseNoun = fmap Tree.Leaf $ oneCard X

parseSentence :: Parser (Tree.BinTree Card)
parseSentence = (do
  f <- parsePred
  x <- parseNoun
  return (Tree.Branch f x)) `mplus` (do
         av <- parseAdv
         s <- parseSentence
         return (Tree.Branch av s))

pick :: [Card] -> [Int] -> [Card]
pick hands ix = map (hands!!) ix
