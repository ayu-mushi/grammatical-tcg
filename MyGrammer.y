{
module MyGrammer where
import qualified Math.Combinat.Trees.Binary as Tree
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

S :: {Tree.Tree Card}
  : Pred Noun { Tree.Node N [$1, $2] }
  | Adverb S { Tree.Node N [$1, $2] }
  | Trash Num { Tree.Node N [Tree.Node Trash [], $2] }

Pred :: {Tree.Tree Card}
     : F { Tree.Node F [] }

Noun :: {Tree.Tree Card}
     : X { Tree.Node F [] }

Adverb :: {Tree.Tree Card}
       : Adv { Tree.Node F [] }

Num :: {Tree.Tree Card}
    : One { Tree.Node One [] }
    | Two { Tree.Node Two [] }
    | Num Op Num { Tree.Node N [$2, $1, $3] }

Op :: {Tree.Tree Card}
   : "+" { Tree.Node (:+:) []}

{
parseError :: [Card] -> a
parseError _ = error "Parse error"
}
