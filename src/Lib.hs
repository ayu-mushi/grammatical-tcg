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

import Control.Exception (finally)
import Control.Monad(forever)
import Data.Text (Text)
import Network.HTTP.Types (hContentType)
import Network.HTTP.Types.Status (status200)
import Network.Wai (Application, responseFile)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Data.IORef


import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WS

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



type Client = (Int, WS.Connection)

broadcast :: Text -> [Client] -> IO ()
broadcast msg = mapM_ (flip WS.sendTextData msg) . map snd

addClient :: WS.Connection -> [Client] -> ([Client], Int)
addClient conn cs = let i = if null cs then 0 else maximum (map fst cs) + 1
                    in  ((i, conn):cs, i)

removeClient :: Int -> [Client] -> ([Client], ())
removeClient i cs = (filter (\c -> fst c /= i) cs, ())

chat :: IORef [Client] -> WS.ServerApp
chat ref pending = do
    conn <- WS.acceptRequest pending
    identifier <- atomicModifyIORef ref (addClient conn)
    flip finally (disconnect identifier) $ forever $ do
        msg <- WS.receiveData conn
        conns <- readIORef ref
        broadcast msg conns
    where
    disconnect identifier = atomicModifyIORef ref (removeClient identifier)

app :: Application
app req respond = respond $ responseFile status200 [(hContentType, "text/html")] "index.html" Nothing


someFunc :: IO ()
someFunc = do
  env <- getEnvironment
  let port = maybe 80 read $ lookup "PORT" env
  let setting = Warp.setPort port Warp.defaultSettings
  putStrLn $ "Your server is listening at http://localhost:" ++ show port ++ "/"
  ref <- newIORef []
  Warp.runSettings setting $ websocketsOr WS.defaultConnectionOptions (chat ref) app

  return ()


  -- カードの名称と効果を記述するDSL
