{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings#-}
{-# LANGUAGE RankNTypes #-}
module Lib
    ( someFunc
    ) where
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

import Control.Exception (finally)
import Control.Monad(forever)
import qualified Data.Text as T(Text,pack, unpack)
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

-- とりあえずテキストで通信
-- とりあえず1人




type Client = WS.Connection

broadcast :: ClientValue -> T.Text -> IO ()
broadcast (ClientValue {_sente= Just s, _gote=Just g}) msg = do
  WS.sendTextData s msg
  WS.sendTextData g msg
broadcast msg _ = error "not not"

addClient :: WS.Connection -> ClientValue -> (ClientValue, Bool)
addClient conn record@(ClientValue {_sente= Nothing, _gote =Nothing}) =
  ((record {_sente=Just conn}), True)
addClient conn record@(ClientValue {_sente= Just _, _gote =Nothing}) =
  (record { _gote = Just conn }, False)
addClient conn record@(ClientValue {_sente= Just _, _gote =Just _}) =
  error "not yes"

defaultClient :: ClientValue
defaultClient = ClientValue {_sente=Nothing, _gote=Nothing}

removeClient :: Bool -> ClientValue -> (ClientValue, ())
removeClient True cs = (defaultClient, ())
removeClient False cs = (defaultClient, ())

data ClientValue = ClientValue {
  _sente :: Maybe Client
  , _gote :: Maybe Client
  }
makeLenses ''ClientValue

select :: Bool -> Lens' ClientValue (Maybe Client)
select True = sente
select False = gote

data ComingMessage = Dummy
  deriving (Show, Read)
data GoingMessage = Message String
  deriving (Show, Read)

chat :: IORef ClientValue -> WS.ServerApp
chat ref pending = do
  conn <- WS.acceptRequest pending
  identifier <- atomicModifyIORef ref (addClient conn)
  flip finally (disconnect identifier) $ forever $ do
    conns <- readIORef ref
    case conns of
      ClientValue { _sente=Just t, _gote = Just _ } -> do
        game <- initialGame
        S.runStateT (gamePlay identifier conns) game
        return ()
      ClientValue _ _ -> do
        threadDelay $ 2 * 1000
        return ()
  where
    disconnect player = atomicModifyIORef ref (removeClient player)

app :: Application
app req respond = respond $ responseFile status200 [(hContentType, "text/html")] "index.html" Nothing

-- 対称性

printAsText :: (MonadIO m) => Bool -> ClientValue -> String -> m ()
printAsText b clients str = liftIO $ WS.sendTextData (fromJust $ clients ^. (select b)) $ T.pack str

gamePlay :: Bool -> ClientValue -> S.StateT Game IO ()
gamePlay turn clients = do
  liftIO $ broadcast clients $ T.pack "Game start"

  deck <- (^. (myself . deck)) <$> S.get

  printAsText turn clients $ show deck
  line <- S.lift $ fmap ((map read . words) . T.unpack) $ WS.receiveData (fromJust $ clients^.sente)



  printAsText turn clients $ show $ pick deck line
  let choices = S.runStateT parseSentence (pick deck line)



  printAsText turn clients $ show $ choices
  ch <- S.lift $ fmap (read . T.unpack) $ WS.receiveData (fromJust $ clients^.sente)

  printAsText turn clients "where to place?"
  n <- S.lift $ fmap (read . T.unpack) $ WS.receiveData (fromJust $ clients^.sente)
  (myself . field) %= (V.// ([(n, (Just $ fst $ choices !! ch))]))


  gameState <- S.get
  liftIO $ broadcast clients $ T.pack $ show gameState


someFunc :: IO ()
someFunc = do
  env <- getEnvironment
  let port = maybe 8000 read $ lookup "PORT" env
  let setting = Warp.setPort port Warp.defaultSettings
  putStrLn $ "Your server is listening at http://localhost:" ++ show port ++ "/"
  ref <- newIORef defaultClient
  Warp.runSettings setting $ websocketsOr WS.defaultConnectionOptions (chat ref) app

  return ()


  -- カードの名称と効果を記述するDSL
