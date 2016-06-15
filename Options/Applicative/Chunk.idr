-- -------------------------------------------------------------- [ Parser.idr ]
-- Description : A command line argument parser
-- Copyright   : (c) Huw Campbell
-- --------------------------------------------------------------------- [ EOH ]
module Options.Applicative.Chunk

import public Text.PrettyPrint.Leijen

%default total
%access public export

data Chunk a = MkChunk (Maybe a)

instance Functor Chunk where
  map f (MkChunk x) = MkChunk $ map f x

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

-- --------------------------------------------------------------------- [ EOF ]
