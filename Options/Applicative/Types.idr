-- -------------------------------------------------------------- [ Parser.idr ]
-- Description : A command line argument parser
-- Copyright   : (c) Huw Campbell
-- --------------------------------------------------------------------- [ EOH ]
module Options.Applicative.Types
import Control.Monad.Trans

data ParseError : Type where
  ErrorMsg : String -> ParseError

data OptReader : a -> Type where
  OptionReader : String -> (String -> Either ParseError a) -> OptReader a
  FlagReader   : String -> a                   -> OptReader a
  ArgReader    :           (String -> Either ParseError a) -> OptReader a

instance Functor OptReader where
  map f (OptionReader n p) = OptionReader n (map f . p)
  map f (FlagReader n d)   = FlagReader n (f d)
  map f (ArgReader p)      = ArgReader (map f . p)

data Option : a -> Type where
  Opt : OptReader a -> Option a

instance Functor Option where
  map f (Opt rdr) = Opt (map f rdr)

data Parser : (a : Type) -> Type where
  NilP : Maybe a -> Parser a
  OptP : Option a -> Parser a
  AppP : Parser (x -> a) -> Parser x -> Parser a
  AltP : Parser a -> Parser a -> Parser a

instance Functor Parser where
  map f (NilP x) = NilP (map f x)
  map f (OptP x) = OptP (map f x)
  map f (AppP ff a) = AppP (map (f .) ff) a
  map f (AltP a b) = AltP (map f a) (map f b)

instance Applicative Parser where
  pure  = NilP . Just
  (<*>) = AppP

instance Alternative Parser where
  empty = NilP Nothing
  (<|>) = AltP

strOption : String -> Parser String
strOption n = OptP (Opt (OptionReader  n Right))

strArg : Parser String
strArg = OptP (Opt (ArgReader Right))

flag : String -> Parser Bool
flag n = OptP (Opt (FlagReader n True))

-- --------------------------------------------------------------------- [ EOF ]