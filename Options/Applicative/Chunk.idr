-- -------------------------------------------------------------- [ Parser.idr ]
-- Description : A command line argument parser
-- Copyright   : (c) Huw Campbell
-- --------------------------------------------------------------------- [ EOH ]
module Options.Applicative.Chunk

import public Text.PrettyPrint.WL

%default total
%access public export

Chunk : Type -> Type
Chunk = Maybe

nonEmptyChunk : Chunk a -> Bool
nonEmptyChunk = isJust

chunked : (a -> a -> a)
        -> Chunk a -> Chunk a -> Chunk a
chunked _ Nothing  y        = y
chunked _ x        Nothing  = x
chunked f (Just x) (Just y) = Just $ f x y

extractChunk : Monoid a => Chunk a -> a
extractChunk = fromMaybe neutral

combine : Chunk Doc -> Chunk Doc -> Chunk Doc
combine = chunked (|++|)

tabulate' : Int -> List (Doc, Doc) -> Chunk Doc
tabulate' _    []    = Nothing
tabulate' size table = pure $ vcat
  [ indent 2 (fillBreak size key |+| value)
  | (key, value) <- table ]

-- | Display pairs of strings in a table.
tabulate : List (Doc, Doc) -> Chunk Doc
tabulate = tabulate' 24

-- --------------------------------------------------------------------- [ EOF ]
