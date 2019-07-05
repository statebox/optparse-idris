-- -------------------------------------------------------------- [ Parser.idr ]
-- Description : A command line argument parser
-- Copyright   : (c) Huw Campbell
-- --------------------------------------------------------------------- [ EOH ]
module Options.Applicative.Chunk

import public Text.PrettyPrint.WL

%default total
%access public export

record Chunk a where
  constructor MkChunk 
  unChunk : Maybe a

nonEmptyChunk : Chunk a -> Bool
nonEmptyChunk = isJust . unChunk

Functor Chunk where
  map f (MkChunk x) = MkChunk $ map f x

Applicative Chunk where
  pure = MkChunk . pure
  (MkChunk f) <*> (MkChunk x) = MkChunk $ f <*> x

Monad Chunk where
  (MkChunk m) >>= f = MkChunk $ m >>= unChunk . f

chunked : (a -> a -> a)
        -> Chunk a -> Chunk a -> Chunk a
chunked _ (MkChunk Nothing)  y                  = y
chunked _ x                  (MkChunk Nothing)  = x
chunked f (MkChunk (Just x)) (MkChunk (Just y)) = MkChunk $ Just $ f x y

extractChunk : Monoid a => Chunk a -> a
extractChunk (MkChunk x) = fromMaybe neutral x

(<+>) : Chunk Doc -> Chunk Doc -> Chunk Doc
(<+>) = chunked (|++|)

tabulate' : Int -> List (Doc, Doc) -> Chunk Doc
tabulate' _    []    = MkChunk Nothing
tabulate' size table = pure $ vcat
  [ indent 2 (fillBreak size key |+| value)
  | (key, value) <- table ]

-- | Display pairs of strings in a table.
tabulate : List (Doc, Doc) -> Chunk Doc
tabulate = tabulate' 24

-- --------------------------------------------------------------------- [ EOF ]
