-- -------------------------------------------------------------- [ Parser.idr ]
-- Description : A command line argument parser
-- Copyright   : (c) Huw Campbell
-- --------------------------------------------------------------------- [ EOH ]
module Options.Applicative.Usage

import Options.Applicative.Types
import Options.Applicative.Maybe
import Control.Monad.State
import Control.Monad.Trans

data OptTree : a -> Type where
  Leaf     : a                -> OptTree a
  MultNode : List (OptTree a) -> OptTree a
  AltNode  : List (OptTree a) -> OptTree a

-- | Map a polymorphic function over all the options of a parser, and collect
-- the results in a tree.
treeMapParser : ({x : Type} -> Option x -> b) -> Parser a -> OptTree b
treeMapParser g = go False False g
  where
    go : Bool -> Bool -> ({x : Type} -> Option x -> b) -> Parser a -> OptTree b
    go _ _ _ (NilP _)     = MultNode []
    go m d f (OptP opt)   = Leaf (f opt)
    go m d f (AppP p1 p2) = MultNode [go m d f p1, go m d f p2]
    go m d f (AltP p1 p2) = AltNode  [go m d f p1, go m d f p2]
