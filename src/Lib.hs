{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings#-}
module Lib
    ( someFunc
    ) where
import Control.Monad (forM, forM_, replicateM, replicateM_, guard, mzero, mplus)
import System.Random
import qualified Control.Monad.State as S
import System.Environment (getEnvironment)
import qualified Data.Vector as V
import Data.Vector.Generic.Lens (vectorIx)
import Control.Lens

-- websocket message は Show, Read で

data Card = F | X
  deriving (Enum,Show,Eq)

data Tree a = Node (Tree a) (Tree a) | Leaf a
  deriving (Show, Eq)

data PlayerState = PlayerState {
  _hands :: [Card]
  , _deck :: [Card]
  , _field :: V.Vector (Maybe (Tree Card))
  } deriving (Show,Eq)
makeLenses ''PlayerState

data Game = Game{
  _opponent :: PlayerState
  , _myself :: PlayerState
  } deriving (Show,Eq)
makeLenses ''Game

initDeck :: IO [Card]
initDeck = do
  replicateM 40 $ do
    r <- randomRIO (0, 1) :: IO Int
    return $ toEnum r :: IO Card


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

initialDraw :: (Monad m) => S.StateT PlayerState m [Card]
initialDraw = replicateM 5 draw

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
    _opponent = o
    , _myself = m
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

parsePred :: Parser (Tree Card)
parsePred = fmap Leaf $ oneCard F

parseNoun :: Parser (Tree Card)
parseNoun = fmap Leaf $ oneCard X

parseSentence :: Parser (Tree Card)
parseSentence = do
  f <- parsePred
  x <- parseNoun
  return (Node f x)

pick :: [Card] -> [Int] -> [Card]
pick hands ix = map (hands!!) ix

{- gamePlay :: S.StateT Game ActionM ()
gamePlay = do
  deck <- (^. (myself . deck)) <$> S.get
  S.lift $ printAsText deck
  line <- S.lift . S.lift $ map read . words <$> getLine :: S.StateT Game ActionM [Int]
  S.lift $ printAsText $ pick deck line
  let choices = S.runStateT parseSentence (pick deck line)
  S.lift $ printAsText $ choices
  ch <- S.lift . S.lift $ read <$> getLine :: S.StateT Game ActionM Int

  S.lift $ text "where to place?"
  n <- S.lift . S.lift $ read <$> getLine :: S.StateT Game ActionM Int
  (myself . field) %= (V.// ([(n, (Just $ fst $ choices !! ch))]))
  gameState <- S.get
  S.lift $ printAsText gameState -}




someFunc :: IO ()
someFunc = do
  env <- getEnvironment
  let port = maybe 8000 read $ lookup "PORT" env

  return ()


  -- カードの名称と効果を記述するDSL
