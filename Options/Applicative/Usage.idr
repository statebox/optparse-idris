-- -------------------------------------------------------------- [ Parser.idr ]
-- Description : A command line argument parser
-- Copyright   : (c) Huw Campbell
-- --------------------------------------------------------------------- [ EOH ]
module Options.Applicative.Usage

import public Text.PrettyPrint.Leijen

import Options.Applicative.Builder
import Options.Applicative.Chunk
import Options.Applicative.Types
import Options.Applicative.Maybe
import Options.Applicative.Run
import Control.Lens
import Control.Monad.State
import Control.Monad.Trans

%access public export

data OptTree : a -> Type where
  Leaf     : a                -> OptTree a
  MultNode : List (OptTree a) -> OptTree a
  AltNode  : List (OptTree a) -> OptTree a

record OptHelpInfo where
  constructor MkOptHelpInfo
  hinfoMulti, hinfoDefault : Bool

renderName : OptName -> Doc
renderName (ShortName c) = char '-' |+| char c
renderName (LongName s)  = string "--" |+| string s

renderNames : List OptName -> Doc
renderNames (Nil)      = Empty
renderNames (n :: Nil) = renderName n
renderNames (n :: ns)  = (foldl (\x => \y => x |++| char '|' |++| renderName y) (renderName n) ns)

optDesc : Bool -> Bool -> OptHelpInfo -> Option g a -> Chunk Doc
optDesc withHidden withParens info (Opt pp rdr) =
  render mm (pp ^. visibility == Visible || withHidden) (hinfoDefault info) (reqParens && withParens)
    where
      mm : Chunk Doc
      mm = MkChunk . Just $ case rdr of
        (OptionReader ns _ m) => renderNames ns |++| (string m)
        (FlagReader ns _)     => renderNames ns
        (ArgReader _ m)       => string m

      reqParens : Bool
      reqParens = case rdr of
        (OptionReader (_ :: _) _ _)  => True
        (OptionReader _ _ _)         => False
        (FlagReader (_ :: _ :: _) _) => True
        (FlagReader _ _)             => False
        (ArgReader _ _)              => False

      render : Chunk Doc -> Bool -> Bool -> Bool -> Chunk Doc
      render _      False  _ _ = MkChunk Nothing
      render chunk  _  True  _ = (map brackets chunk)
      render chunk  _ _ True   = (map parens   chunk)
      render chunk  _ _ _      = chunk

fold_tree : OptTree (Chunk Doc) -> Chunk Doc
fold_tree (Leaf x) = x
fold_tree (MultNode xs) = foldr ((<+>) . fold_tree) (MkChunk Nothing) xs
fold_tree (AltNode xs) = alt_node
                       . filter (not . isEmptyChunk)
                       . map fold_tree $ xs
  where
    alt_node : List (Chunk Doc) -> Chunk Doc
    alt_node (n :: Nil) = n
    alt_node (n :: ns)  = map parens
                        . foldl (chunked (\x => \y => x <+> char '|' <+> y)) n
                        $ ns


-- | Map a polymorphic function over all the options of a parser, and collect
-- the results in a tree.
treeMapParser : ({ g : ParamType } -> {x : Type} -> OptHelpInfo -> Option g x -> b) -> Parser a -> OptTree b
treeMapParser g = simplify . go False False g
  where
    simplify : OptTree a -> OptTree a
    simplify (Leaf x) = Leaf x
    simplify (MultNode xs) =
      case the (List (OptTree a)) (concatMap (remove_mult . simplify) xs) of
        [x] => x
        xs' => MultNode xs'
      where
        remove_mult (MultNode ts) = ts
        remove_mult t = [t]
    simplify (AltNode xs) =
      case the (List (OptTree a)) (concatMap (remove_alt . simplify) xs) of
        []  => MultNode []
        [x] => x
        xs' => AltNode xs'
      where
        remove_alt (AltNode ts) = ts
        remove_alt (MultNode []) = []
        remove_alt t = [t]

    has_default : Parser a -> Bool
    has_default p = isJust (evalParser p)

    go : Bool -> Bool -> ({ g : ParamType } -> {x : Type} -> OptHelpInfo -> Option g x -> b) -> Parser a -> OptTree b
    go _ _ _ (NilP _)     = MultNode []
    go m d f (OptP opt)   = case (opt ^. visibility) of
      Internal => MultNode []
      _        => Leaf (f (MkOptHelpInfo m d) opt)
    go m d f (AppP p1 p2) = MultNode [go m d f p1, go m d f p2]
    go m d f (AltP p1 p2) = AltNode  [go m d' f p1, go m d' f p2]
      where d' = d || has_default p1 || has_default p2

flattenTree : OptTree a -> List a
flattenTree (Leaf x) = [x]
flattenTree (MultNode xs) = xs >>= flattenTree
flattenTree (AltNode xs) = xs >>= flattenTree

briefDesc : Parser a -> Chunk Doc
briefDesc = fold_tree . treeMapParser (optDesc False True)

fullDesc : Parser a -> Chunk Doc
fullDesc = tabulate . catMaybes . flattenTree . treeMapParser doc
  where
    doc : OptHelpInfo -> Option g a -> Maybe (Doc, Doc)
    doc info opt = do
      let n = optDesc True False info opt
      let h = opt ^. help
      guard . not . isEmptyChunk $ n
      return (extractChunk n, h)

-- | Generate the help text for a program.
parserHelp : Parser a -> Doc
parserHelp p =
  text "Usage:" |++| extractChunk (briefDesc p) |/| with_title "Available options:" (fullDesc p)
  where
    with_title : String -> Chunk Doc -> Doc
    with_title title ch = (text title |/| extractChunk ch)
