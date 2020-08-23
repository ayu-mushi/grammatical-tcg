{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings#-}
module Lib
    ( someFunc
    ) where
import Text.Parsec as Parsec
import Control.Monad (forM, forM_, replicateM, replicateM_)
import System.Random
import qualified Control.Monad.State as S
import Web.Scotty as Scotty
import System.Environment (getEnvironment)

data Card = F | X deriving (Enum,Show)

data GameState = GameState {
  _hands :: [Card],
  _deck :: [Card]
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
  deck <- S.get
  case deck of
    [] -> error "game over"
    (x:xs) -> do
      S.put xs
      return x

initialDraw :: (Monad m) => S.StateT [Card] m [Card]
initialDraw = replicateM 5 draw

parse :: Parsec Card u [Card]
parse = undefined

pick :: [Card] -> [Int] -> [Card]
pick hands ix  = map (hands!!) ix

someFunc :: IO ()
someFunc = do
  env <- getEnvironment
  let port = maybe 8000 read $ lookup "PORT" env
  scotty port $ do
    get "/" $ do
      html $ "Hello, Heroku!"

  deck <- initDeck
  print deck
  line <- map read . words <$> getLine :: IO [Int]
  print $ pick deck line
  return ()
