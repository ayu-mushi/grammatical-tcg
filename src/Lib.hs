{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings#-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
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
import qualified Data.ByteString.Lazy as LB(toStrict)
import qualified Data.Text.Encoding as Text(encodeUtf8, decodeUtf8)
import Math.Combinat.Trees.Binary

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson

import Control.Exception (finally)
import Control.Monad(forever)
import qualified Data.Text as T(Text,pack, unpack)
import Network.HTTP.Types (hContentType)
import Network.HTTP.Types.Status (status200)
import Network.Wai (Application, responseFile, pathInfo)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Data.IORef


import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WS

-- websocket message は Show, Read で

data Card = F | X
  deriving (Enum,Show,Eq,Read)

data Tree a = Node (Tree a) (Tree a) | Leaf a
  deriving (Show, Eq,Read)

data PlayerState = PlayerState {
  _hands :: [Card]
  , _deck :: [Card]
  , _field :: V.Vector (Maybe (Tree Card))
  } deriving (Show,Eq,Read)
makeLenses ''PlayerState

data Game = Game{
  _opponent :: PlayerState
  , _myself :: PlayerState
  } deriving (Show,Eq, Read)
makeLenses ''Game

$(Aeson.deriveJSON Aeson.defaultOptions ''Card)
$(Aeson.deriveJSON Aeson.defaultOptions ''Tree)
$(Aeson.deriveJSON Aeson.defaultOptions ''PlayerState)
$(Aeson.deriveJSON Aeson.defaultOptions ''Game)

selectPlayer :: Bool -> Lens' Game PlayerState
selectPlayer True = myself
selectPlayer False = opponent

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
removeClient True cs = error "disconnnnnected"
removeClient False cs = error "disconnnnnected"

data ClientValue = ClientValue {
  _sente :: Maybe Client
  , _gote :: Maybe Client
  }
makeLenses ''ClientValue

selectClient :: Bool -> Lens' ClientValue (Maybe Client)
selectClient True = sente
selectClient False = gote

data ComingMessage = Dummy | Select Int
  deriving (Show, Read)
data GoingMessage = Message String | Refresh Game | Choice [(Tree Card)]
  deriving (Show, Read)

$(Aeson.deriveJSON Aeson.defaultOptions ''ComingMessage)
$(Aeson.deriveJSON Aeson.defaultOptions ''GoingMessage)

startGame :: IORef ClientValue -> WS.ServerApp
startGame ref pending = do
  conn <- WS.acceptRequest pending
  identifier <- atomicModifyIORef ref (addClient conn)
  if identifier then
    flip finally (disconnect identifier) $ forever $ do
      conns <- readIORef ref
      case conns of
        ClientValue { _sente=Just _, _gote = Just _ } -> do
          liftIO $ broadcast conns $ Text.decodeUtf8 $ LB.toStrict $ Aeson.encode $ Message "Game start"
          game <- initialGame

          S.runStateT (gameLoop identifier conns) game
          return ()
        ClientValue _ _ -> do
          threadDelay $ 20 * 1000
          return ()
  else flip finally (disconnect identifier) $ forever $ do
    threadDelay $ 20 * 1000
  where
    disconnect player = atomicModifyIORef ref (removeClient player)
    gameLoop identifier conns = do
      gamePlay identifier conns
      gamePlay (not identifier) conns
      gameLoop identifier conns

toJSONText :: (Aeson.ToJSON j) => j -> T.Text
toJSONText = Text.decodeUtf8 . LB.toStrict . Aeson.encode

app :: Application
app req respond =
  if pathInfo req == [] then
    respond $ responseFile status200 [(hContentType, "text/html")] "index.html" Nothing
  else if pathInfo req == ["style.css"] then
    respond $ responseFile status200 [(hContentType, "text/css")] "style.css" Nothing
  else
    respond $ responseFile status200 [(hContentType, "text/html")] "index.html" Nothing

-- 対称性

printAsText :: (MonadIO m, Aeson.ToJSON j) => Bool -> ClientValue -> j -> m ()
printAsText b clients str = liftIO $ WS.sendTextData (fromJust $ clients ^. (selectClient b)) $ Text.decodeUtf8 $ LB.toStrict $ Aeson.encode str

broadcast :: ClientValue -> T.Text -> IO ()
broadcast clients msg = do
  liftIO $ WS.sendTextData (fromJust $ clients ^. (selectClient True)) $ msg
  liftIO $ WS.sendTextData (fromJust $ clients ^. (selectClient False)) $ msg


gamePlay :: Bool -> ClientValue -> S.StateT Game IO ()
gamePlay identifier clients = do
  deck <- (^. (myself . deck)) <$> S.get

  printAsText identifier clients $ Message $ show deck
  line <- S.lift $ fmap ((map read . words) . T.unpack) $ WS.receiveData (fromJust $ clients^.(selectClient identifier))

  printAsText identifier clients $ Message $ show $ pick deck line
  let choices = S.evalStateT parseSentence (pick deck line)

  printAsText identifier clients $ Choice choices
  ch <- S.lift $ fmap (read . T.unpack) $ WS.receiveData (fromJust $ clients^.(selectClient identifier))

  printAsText identifier clients $ Message "where to place?"
  n <- S.lift $ fmap (read . T.unpack) $ WS.receiveData (fromJust $ clients^.(selectClient identifier))
  ((selectPlayer identifier) . field) %= (V.// ([(n, (Just $ choices !! ch))]))


  gameState <- S.get
  liftIO $ broadcast clients $ Text.decodeUtf8 $ LB.toStrict $ Aeson.encode $ Refresh $ gameState


someFunc :: IO ()
someFunc = do
  env <- getEnvironment
  let port = maybe 8000 read $ lookup "PORT" env
  let setting = Warp.setPort port Warp.defaultSettings
  putStrLn $ "Your server is listening at http://localhost:" ++ show port ++ "/"
  ref <- newIORef defaultClient
  Warp.runSettings setting $ websocketsOr WS.defaultConnectionOptions (startGame ref) app

  return ()


  -- カードの名称と効果を記述するDSL
