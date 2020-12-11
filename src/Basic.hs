{-# LANGUAGE TemplateHaskell #-}
module Basic where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson
import Data.List (intercalate)

data Card = F | X | Adv | (:+:) | One | Two | Trash
  deriving (Enum,Show,Eq,Read,Ord)


data MyTree a = Leaf a | Node (MyTree a) [MyTree a] deriving (Show, Eq, Read)


showCard :: Card -> String
showCard One = "1"
showCard Two = "2"
showCard (:+:) = "+"
showCard x = show x

showMyTree :: MyTree Card -> String
showMyTree (Leaf x) = showCard x
showMyTree (Node x trees) = show x ++ "(" ++ (intercalate "," $ map showMyTree trees) ++ ")"

$(Aeson.deriveJSON Aeson.defaultOptions ''Card)
$(Aeson.deriveJSON Aeson.defaultOptions ''MyTree)
