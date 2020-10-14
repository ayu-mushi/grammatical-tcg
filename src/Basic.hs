module Basic where

data Card = F | X | Adv | (:+:) | One | Two | Trash | N
  deriving (Enum,Show,Eq,Read,Ord)

