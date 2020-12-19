{
module MyGrammer where
import qualified Control.Monad.State as S
import Basic
}

%name parseSentencePeriod
%tokentype { Card }
%error { parseError }

%token
  F { F }
  X { X }
  "+" { (:+:) }
  One { One }
  Two { Two }
  N { N }
  Trash { Trash }
  Adv { Adv }
  Draw { Draw }
  Skip { Skip }
  Double { Double }
  And { And }
  Me { Me }
  You { You }

%%

S :: {MyTree Card}
  : Pred Noun { Node $1 [$2] }
  | Adverb S { Node $1 [$2] }
  | Exe Player Num { (Node $1 [$2, $3]) }
  | Skip { (Leaf Skip) }
  | Double S { Node (Leaf Double) [$2] }
  | S And S { Node (Leaf And) [$1, $3] }

Pred :: {MyTree Card}
     : F { Leaf F }

Noun :: {MyTree Card}
     : X { Leaf X }

Adverb :: {MyTree Card}
       : Adv { Leaf Adv }

Num :: {MyTree Card}
    : One { Leaf One }
    | Two { Leaf Two }
    | Num Op Num { Node $2 [$1, $3] }

Exe :: {MyTree Card}
    : Trash { Leaf Trash }
    | Draw {Leaf Draw}
    | Exe And Exe {Node (Leaf And) [$1, $3] }

Op :: {MyTree Card}
   : "+" { Leaf (:+:) }

Player :: {MyTree Card}
   : You { (Leaf You) }
   | Me  { (Leaf Me) }
   | Player And Player { (Node (Leaf And)[$1, $3]) }


{
parseError :: [Card] -> a
parseError _ = error "Parse error"
}
