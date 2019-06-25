-- -------------------------------------------------------------- [ Parser.idr ]
-- Description : A command line argument parser
-- Copyright   : (c) Huw Campbell
-- --------------------------------------------------------------------- [ EOH ]
module Options.Applicative.Chunk

import public Text.PrettyPrint.WL

%default total
%access public export

data Chunk a = MkChunk (Maybe a)

unChunk : Chunk a -> Maybe a
unChunk (MkChunk a) = a

Functor Chunk where
  map f (MkChunk x) = MkChunk $ map f x

Applicative Chunk where
  pure = MkChunk . pure
  (MkChunk f) <*> (MkChunk x) = MkChunk (f <*> x)

Monad Chunk where
  (MkChunk m) >>= f = MkChunk $ m >>= unChunk . f

chunked : (a -> a -> a)
        -> Chunk a -> Chunk a -> Chunk a
chunked _ (MkChunk Nothing) y = y
chunked _ x (MkChunk Nothing) = x
chunked f (MkChunk (Just x)) (MkChunk (Just y)) = MkChunk (Just (f x y))

extractChunk : Monoid a => Chunk a -> a
extractChunk (MkChunk x) = fromMaybe neutral x

(<+>) : Chunk Doc -> Chunk Doc -> Chunk Doc
(<+>) = chunked (|++|)

isEmptyChunk : Chunk a -> Bool
isEmptyChunk (MkChunk Nothing)  = True
isEmptyChunk (MkChunk (Just _)) = False


tabulate' : Int -> List (Doc, Doc) -> Chunk Doc
tabulate' _ [] = MkChunk Nothing
tabulate' size table = pure $ vcat
  [ indent 2 (fillBreak size key |+| value)
  | (key, value) <- table ]

-- | Display pairs of strings in a table.
tabulate : List (Doc, Doc) -> Chunk Doc
tabulate = tabulate' 24

-- --------------------------------------------------------------------- [ EOF ]
