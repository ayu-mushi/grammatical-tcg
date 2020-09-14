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
import qualified Math.Combinat.Trees.Binary as Tree

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


import Game

-- websocket message は Show, Read で
-- とりあえずテキストで通信
-- とりあえず1人




type Client = WS.Connection

data ClientValue = ClientValue {
  _sente :: Maybe Client
  , _gote :: Maybe Client
  , _audience :: [Client]
  }
makeLenses ''ClientValue



addClient :: WS.Connection -> ClientValue -> (ClientValue, Bool)
addClient conn record@(ClientValue {_sente= Nothing, _gote =Nothing}) =
  ((record {_sente=Just conn}), True)
addClient conn record@(ClientValue {_sente= Just _, _gote =Nothing}) =
  (record { _gote = Just conn }, False)
addClient conn record@(ClientValue {_sente= Just _, _gote =Just _}) =
  ((record & audience %~ (conn:), False))

defaultClient :: ClientValue
defaultClient = ClientValue {_sente=Nothing, _gote=Nothing, _audience=[]}

removeClient :: Bool -> ClientValue -> (ClientValue, ())
removeClient True cs = error "disconnnnnected"
removeClient False cs = error "disconnnnnected"

selectClient :: Bool -> Lens' ClientValue (Maybe Client)
selectClient True = sente
selectClient False = gote

data ComingMessage = Dummy | Select Int
  deriving (Show, Read)
data GoingMessage = Message String
                  | Refresh Game
                  | Choice [String]
                  | Hand [Card]
                  | YouAre Bool
                  | WhereToPlace
                  | RequireMonsterToSummon
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
          printAsText True conns $ YouAre True
          printAsText False conns $ YouAre False
          game <- initialGame

          S.runStateT (gameLoop identifier conns) game
          return ()
        ClientValue _ _ _ -> do
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
  mapM (liftIO . (`WS.sendTextData` msg)) $ clients ^. audience
  return ()


gamePlay :: Bool -> ClientValue -> S.StateT Game IO ()
gamePlay identifier clients = do
  gameState <- S.get
  liftIO $ broadcast clients $ Text.decodeUtf8 $ LB.toStrict $ Aeson.encode $ Refresh $ gameState
  d <- (^. ((selectPlayer identifier) . deck)) <$> S.get
  h <- (^. ((selectPlayer identifier) . hands)) <$> S.get

  printAsText identifier clients $ Message "手札から召喚する物を選択してください。召喚ボタンで召喚できます。"
  printAsText identifier clients $ Hand h
  printAsText identifier clients RequireMonsterToSummon

  line <- S.lift $ fmap ((map read . words) . T.unpack) $ WS.receiveData (fromJust $ clients^.(selectClient identifier))
  printAsText identifier clients $ Message $ show $ line
  printAsText identifier clients $ Message $ show $ pick h line
  let choices = S.evalStateT parseSentence (pick h line)

  printAsText identifier clients $ Message "召喚するモンスターをクリックしてください。"
  printAsText identifier clients $ Choice $ map (Tree.graphvizDotBinTree "aiueo") choices
  ch <- S.lift $ fmap (read . T.unpack) $ WS.receiveData (fromJust $ clients^.(selectClient identifier))

  printAsText identifier clients $ Message "召喚する場所をクリックしてください"
  printAsText identifier clients WhereToPlace
  n <- S.lift $ fmap (read . T.unpack) $ WS.receiveData (fromJust $ clients^.(selectClient identifier))
  ((selectPlayer identifier) . field) %= (V.// ([(n, (Just $ (choices !! ch,
                                                      Tree.graphvizDotBinTree "aiueo" (choices !! ch))))]))


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
