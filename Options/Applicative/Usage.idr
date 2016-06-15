-- -------------------------------------------------------------- [ Parser.idr ]
-- Description : A command line argument parser
-- Copyright   : (c) Huw Campbell
-- --------------------------------------------------------------------- [ EOH ]
module Options.Applicative.Usage

import public Text.PrettyPrint.Leijen

import Options.Applicative.Types
import Options.Applicative.Maybe
import Control.Monad.State
import Control.Monad.Trans

data OptTree : a -> Type where
  Leaf     : a                -> OptTree a
  MultNode : List (OptTree a) -> OptTree a
  AltNode  : List (OptTree a) -> OptTree a

renderName : OptName -> Doc
renderName (ShortName c) = char '-' |+| char c
renderName (LongName s)  = string "--" |+| string s

renderNames : List OptName -> Doc
renderNames (n :: Nil) = renderName n
renderNames (n :: ns)  = parens (foldl (\x => \y => x |++| char '|' |++| renderName y) (renderName n) ns)

optDesc : Option g a -> Doc
optDesc opt = case opt of
  Opt _ (OptionReader ns _ m) => renderNames ns |++| (string m)
  Opt _ (FlagReader ns _)     => renderNames ns
  Opt _ (ArgReader _ m)       => string m

fold_tree : OptTree Doc -> Doc
fold_tree (Leaf x) = x
fold_tree (MultNode xs) = foldr ((|++|) . fold_tree) Empty xs
fold_tree (AltNode xs) = alt_node
                       . map fold_tree $ xs
  where
    alt_node : List Doc -> Doc
    alt_node (n :: Nil) = n
    alt_node (n :: ns)  = parens
                . foldl ((\x => \y => x |++| char '|' |++| y)) n
                $ ns

-- | Map a polymorphic function over all the options of a parser, and collect
-- the results in a tree.
treeMapParser : ({ g : ParamType } -> {x : Type} -> Option g x -> b) -> Parser a -> OptTree b
treeMapParser g = go False False g
  where
    go : Bool -> Bool -> ({ g : ParamType } -> {x : Type} -> Option g x -> b) -> Parser a -> OptTree b
    go _ _ _ (NilP _)     = MultNode []
    go m d f (OptP opt)   = Leaf (f opt)
    go m d f (AppP p1 p2) = MultNode [go m d f p1, go m d f p2]
    go m d f (AltP p1 p2) = AltNode  [go m d f p1, go m d f p2]

briefDesc : Parser a -> Doc
briefDesc = fold_tree . treeMapParser (optDesc)
