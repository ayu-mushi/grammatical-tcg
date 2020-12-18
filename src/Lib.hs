{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings#-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Lib where

import Control.Monad (forM, forM_, replicateM, replicateM_, guard, mzero, mplus)
import Control.Monad.Trans (MonadIO, lift,liftIO)
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
import qualified Data.List as L

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson

import Control.Exception (finally)
import Control.Monad(forever)
import qualified Data.Text as T(Text,pack, unpack)
import qualified Data.Text.Lazy as LazyT
import Network.HTTP.Types (hContentType)
import Network.HTTP.Types.Status (status200)
import Network.Wai (Application, responseFile, pathInfo)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Data.IORef
import qualified Data.Map as Map (lookup)


import Data.GraphViz.Attributes
import Data.GraphViz.Commands
import Data.GraphViz.Types(printDotGraph)
import Data.GraphViz.Types.Generalised
import Data.GraphViz.Types.Monadic

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WS


import Basic
import MyGrammer (parseSentencePeriod, GLRResult(..), TreeDecode(..), decode, doParse)
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
removeClient True cs = error "removeClient"
removeClient False cs = error "removeClient"

selectClient :: Bool -> Lens' ClientValue (Maybe Client)
selectClient True = sente
selectClient False = gote

{-data ComingMessage = Dummy | Select Int
  deriving (Show, Read)-}
data GoingMessage = Message String
                  | Refresh Game
                  | Choice [String]
                  | Hand [Card]
                  | YouAre Bool
                  | WhereToPlace
                  | RequireMonsterToSummon
  deriving (Show, Read)

-- パス

-- $(Aeson.deriveJSON Aeson.defaultOptions ''ComingMessage)
$(Aeson.deriveJSON Aeson.defaultOptions ''GoingMessage)

startGame :: IORef ClientValue -> WS.ServerApp
startGame ref pending = do
  conn <- WS.acceptRequest pending
  identifier <- atomicModifyIORef ref (addClient conn)

  game <- initialGame

  if identifier then
    flip finally (disconnect identifier) $ S.void $ (`S.runStateT` game) $ forever $ do
      conns <- liftIO $ readIORef ref
      case conns of
        ClientValue { _sente=Just _, _gote = Just _ } -> do
          count <- (^. turn_count) <$> S.get

          S.when (count==0) $ do
            liftIO $ broadcast conns $ Text.decodeUtf8 $ LB.toStrict $ Aeson.encode $ Message "Game start"
            printAsText True conns $ YouAre True
            printAsText False conns $ YouAre False

          gameLoop identifier conns

          return ()
        ClientValue _ _ _ -> do
          liftIO $ threadDelay $ 20 * 1000
          return ()
  else flip finally (disconnect identifier) $ forever $ do
    threadDelay $ 20 * 1000
  where
    disconnect player = atomicModifyIORef ref (removeClient player)
    gameLoop identifier conns = do
      conns <- liftIO $ readIORef ref
      gameState <- S.get
      if even (gameState ^. turn_count)
         then gamePlay identifier conns
         else gamePlay (not identifier) conns
      turn_count += 1

toJSONText :: (Aeson.ToJSON j) => j -> T.Text
toJSONText = Text.decodeUtf8 . LB.toStrict . Aeson.encode

app :: Application
app req respond =
  if pathInfo req == [] then
    respond $ responseFile status200 [(hContentType, "text/html")] "index.html" Nothing
  else if pathInfo req == ["style.css"] then
    respond $ responseFile status200 [(hContentType, "text/css")] "style.css" Nothing
  else if pathInfo req == ["script.js"] then
    respond $ responseFile status200 [(hContentType, "text/javascript")] "script.js" Nothing
  else do
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

deleteElm :: Int -> [a] -> [a]
deleteElm n xs = let (ys,zs) = splitAt n xs in ys ++ (tail zs)

handToNumbers ::  Bool -> ClientValue -> S.StateT Game IO [Int]
handToNumbers identifier clients = do
  h <- (^. ((selectPlayer identifier) . hands)) <$> S.get

  printAsText identifier clients $ Message "手札から召喚する物を選択してください。召喚ボタンで召喚できます。"
  printAsText identifier clients $ Hand h
  printAsText identifier clients RequireMonsterToSummon

  line <- S.lift $ fmap ((map read . words) . T.unpack) $ WS.receiveData (fromJust $ clients^.(selectClient identifier))

  printAsText identifier clients $ Message $ show $ line
  printAsText identifier clients $ Message $ show $ pick h line
  return line

inputAndParseLoop ::  Bool -> ClientValue -> S.StateT Game IO [MyTree Card]
inputAndParseLoop identifier clients = do
  line <- handToNumbers identifier clients
  h <- (^. ((selectPlayer identifier) . hands)) <$> S.get

  let parseResult = doParse (map (:[]) $ pick h line)
  case parseResult of
    (ParseOK root forest) -> do
      let choices = decode (forest_lookup forest) root :: [(MyTree Card)]
      (selectPlayer identifier) . hands .= ((`S.execState` h) $ S.forM_ (L.sortBy (flip compare) line) $ \i -> do
        l <- S.get
        S.put (deleteElm i l))
      h <- (^. ((selectPlayer identifier) . hands)) <$> S.get
      printAsText identifier clients $ Hand h
      return choices

    (ParseError tokens forest) -> whenFailed identifier clients
    (ParseEOF forest) -> whenFailed identifier clients


  where
    forest_lookup f i = fromJust $ Map.lookup i f

    whenFailed identifier clients = do
      printAsText identifier clients $ Message "パースできません。文法を確かめてください。"
      liftIO $ threadDelay $ 1000*1000
      inputAndParseLoop identifier clients

effectWhenSummoned :: (MonadIO m) => Bool -> ClientValue -> MyTree Card -> S.StateT Game m () -- TODO: 関心の分離
effectWhenSummoned turn clients (Node (Leaf Trash) [formula]) = do
  gameState <- S.get
  let handsMaisu = length $ gameState ^. (selectPlayer turn . hands)

  let how_many_card_you_trash = (evalFormula formula)
  printAsText turn clients $ Message $ "手札の枚数: " ++ show handsMaisu
  printAsText turn clients $ Message $ "Trashの効果: 手札を" ++ show how_many_card_you_trash ++ "枚捨てる。"

  replicateM_ how_many_card_you_trash $ do
    (selectPlayer turn . hands) %= trashOne
    liftIO $ threadDelay $ 1000*100
    liftIO $ broadcast clients $ Text.decodeUtf8 $ LB.toStrict $ Aeson.encode $ Refresh $ gameState

  gameState <- S.get
  let handsMaisu = length $ gameState ^. (selectPlayer turn . hands)

  printAsText turn clients $ Message $ "手札の残り枚数: " ++ show handsMaisu
  where
   trashOne [] = []
   trashOne (x:xs) = xs


effectWhenSummoned turn clients (Node (Leaf Draw) [formula]) = do
  gameState <- S.get
  let how_many_card_opponent_draw = (evalFormula formula)

  let handsMaisu = length $ gameState ^. (selectPlayer (not turn) . hands)
  printAsText turn clients $ Message $ "相手の手札の更新前枚数: " ++ show handsMaisu

  printAsText turn clients $ Message $ "Drawの効果: 相手は手札を" ++ show how_many_card_opponent_draw ++ "枚引く。"

  replicateM_ how_many_card_opponent_draw $ do
    (selectPlayer (not turn)) %= S.execState drawToHand
    liftIO $ threadDelay $ 1000*100
    liftIO $ broadcast clients $ Text.decodeUtf8 $ LB.toStrict $ Aeson.encode $ Refresh $ gameState

  gameState <- S.get
  let handsMaisu = length $ gameState ^. (selectPlayer (not turn) . hands)
  printAsText turn clients $ Message $ "相手の手札の更新後枚数: " ++ show handsMaisu

effectWhenSummoned turn clients (Node (Leaf Double) [sentence]) = do
  printAsText turn clients $ Message $ "Doubleの効果: " ++ showMyTree sentence++ "を2回行う。"
  printAsText turn clients $ Message $ "1回目↑"
  effectWhenSummoned turn clients sentence
  printAsText turn clients $ Message $ "2回目↑"
  effectWhenSummoned turn clients sentence

effectWhenSummoned turn clients (Leaf Skip) = do
  gameState <- S.get
  printAsText turn clients $ Message $ "効果前: " ++ show (gameState ^. turn_count) ++ "ターン目"
  printAsText turn clients $ Message $ "Skipの効果: "++ "相手のターンをスキップする。"
  turn_count += 1
  gameState <- S.get
  printAsText turn clients $ Message $ "効果後: " ++ show (gameState ^. turn_count) ++ "ターン目"
  return ()

effectWhenSummoned turn clients (Node (Leaf And) [sentence1, sentence2]) = do
  printAsText turn clients $ Message $ "Andの効果: " ++ showMyTree sentence1 ++ "と"++showMyTree sentence2 ++ "を続けて行う。"
  effectWhenSummoned turn clients sentence1
  effectWhenSummoned turn clients sentence2

effectWhenSummoned turn clients monster = return ()


routineEffect ::  (MonadIO m) => Bool -> ClientValue -> MyTree Card -> S.StateT Game m () -- TODO: 関心の分離
routineEffect turn clients monster = undefined


gamePlay :: Bool -> ClientValue -> S.StateT Game IO ()
gamePlay identifier clients = do
  gameState <- S.get
  liftIO $ broadcast clients $ Text.decodeUtf8 $ LB.toStrict $ Aeson.encode $ Refresh $ gameState

  -- ドロー
  (selectPlayer identifier) %= S.execState drawToHand


  -- summon
  choices <- inputAndParseLoop identifier clients

  printAsText identifier clients $ Message "召喚するモンスターをクリックしてください。"
  printAsText identifier clients $ Choice $ map (LazyT.unpack . treeToGraph) choices
  ch <- S.lift $ fmap (read . T.unpack) $ WS.receiveData (fromJust $ clients^.(selectClient identifier))

  printAsText identifier clients $ Message "召喚する場所をクリックしてください"
  printAsText identifier clients WhereToPlace
  n <- S.lift $ fmap (read . T.unpack) $ WS.receiveData (fromJust $ clients^.(selectClient identifier))
  let selected = choices !! ch
  ((selectPlayer identifier) . field) %= (V.// ([(n, (Just $ (selected,
                                                      LazyT.unpack $ treeToGraph selected)))]))

  liftIO $ broadcast clients $ Text.decodeUtf8 $ LB.toStrict $ Aeson.encode $ Refresh $ gameState

  effectWhenSummoned identifier clients selected

  gameState <- S.get

  if (isWinning identifier gameState)
     then do
       printAsText identifier clients $ Message $ "あなた (" ++ (if identifier then "先手" else "後手") ++ ") の勝ちです"
       printAsText (not identifier) clients $ Message $"あなた (" ++ (if identifier then "後手" else "先手") ++ ") の負けです"
     else return ()
  liftIO $ broadcast clients $ Text.decodeUtf8 $ LB.toStrict $ Aeson.encode $ Refresh $ gameState


-- receiveDataしてパースに失敗した場合はもう一度要求する

treeToGraph :: MyTree Card -> LazyT.Text
treeToGraph tree = printDotGraph $ digraph' $ S.void $ treeToGraph' [0] tree

  where
  treeToGraph' :: [Int] -> MyTree Card -> DotM LazyT.Text ([Int])
  treeToGraph' node_id (Leaf x) = do
    node (LazyT.pack $ show node_id) [textLabel (LazyT.pack $ showMyTree (Leaf x))]
    return node_id
  treeToGraph' node_id (Node x children) = do
    childGraphIds <- mapM (uncurry treeToGraph') (zip (map (:node_id) [0..]) children)
    node (LazyT.pack $ show node_id) [textLabel (LazyT.pack $ showMyTree x)]
    result <- mapM (((LazyT.pack $ show node_id) -->) . LazyT.pack . show) childGraphIds
    return node_id

  name node_id = LazyT.pack $ "node_"++(show node_id)

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
  -- プログラミング言語を作るDSL?
  -- hoverで情報表示
