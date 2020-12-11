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

%%

S :: {MyTree Card}
  : Pred Noun { Node $1 [$2] }
  | Adverb S { Node $1 [$2] }
  | Trash Num { Node (Leaf Trash) [$2] }

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

Op :: {MyTree Card}
   : "+" { Leaf (:+:) }

{
parseError :: [Card] -> a
parseError _ = error "Parse error"
}
