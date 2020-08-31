{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings#-}
module Lib
    ( someFunc
    ) where
import Control.Monad (forM, forM_, replicateM, replicateM_, guard, mzero, mplus)
import System.Random
import qualified Control.Monad.State as S
import Web.Scotty as Scotty
import System.Environment (getEnvironment)
import qualified Data.Vector as V

data Card = F | X deriving (Enum,Show,Eq)

data Tree a = Node (Tree a) (Tree a) | Leaf a deriving (Show, Eq)

data PlayerState = PlayerState {
  _hands :: [Card]
  , _deck :: [Card]
  , _field :: V.Vector (Tree Card)
  }

initDeck :: IO [Card]
initDeck = do
  replicateM 40 $ do
    r <- randomRIO (0, 1) :: IO Int
    return $ toEnum r :: IO Card

-- カードを配る
-- 入力
-- 手札を数字で指定
-- パース

draw :: (Monad m) => S.StateT [Card] m Card
draw = do
  c <- S.get
  case c of
    [] -> error "game over"
    (x:xs) -> do
      S.put xs
      return x

initialDraw :: (Monad m) => S.StateT [Card] m [Card]
initialDraw = replicateM 5 draw


type Parser a = S.StateT [Card] [] a

oneCard :: Card -> S.StateT [Card] [] Card
oneCard one = do
  c <- S.get
  case c of
       [] -> mzero
       (x:xs) -> do
         guard (one==x)
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

someFunc :: IO ()
someFunc = do
  {-env <- getEnvironment
  let port = maybe 8000 read $ lookup "PORT" env
  scotty port $ do
    get "/" $ do
      html $ "Hello, Heroku!"-}

  deck <- initDeck
  print deck
  line <- map read . words <$> getLine :: IO [Int]
  print $ pick deck line
  let choices = S.runStateT parseSentence (pick deck line)
  print choices
  ch <- read <$> getLine :: IO Int
  print $ choices !! ch
  return ()


  -- カードの名称と効果を記述するDSL
