-- -------------------------------------------------------------- [ Parser.idr ]
-- Description : A command line argument parser
-- Copyright   : (c) Huw Campbell
-- --------------------------------------------------------------------- [ EOH ]
module Options.Applicative.Types
import public Text.PrettyPrint.Leijen

import        Control.Monad.Trans
import        Control.Lens

%default total
%access public export

data ParamType = OptionParams
               | FlagParams
               | ArgParams

data OptProperties = MkOptProperties Bool Doc

data OptName : Type where
  ShortName : Char   -> OptName
  LongName  : String -> OptName

Eq OptName where
  (==) (ShortName a) (ShortName b) = a == b
  (==) (LongName  a) (LongName  b) = a == b
  _  == _ = False

data ParseError : Type where
  ErrorMsg : String -> ParseError

data OptReader : ParamType -> a -> Type where
  OptionReader : List OptName -> (String -> Either ParseError a) -> String -> OptReader OptionParams a
  FlagReader   : List OptName -> a                                         -> OptReader FlagParams a
  ArgReader    :                 (String -> Either ParseError a) -> String -> OptReader ArgParams a

Functor (OptReader ps) where
  map f (OptionReader n p m) = OptionReader n (map f . p) m
  map f (FlagReader n d)     = FlagReader n (f d)
  map f (ArgReader p m)      = ArgReader (map f . p) m

data Option : ParamType -> a -> Type where
  Opt : OptProperties -> OptReader ps a -> Option ps a

Functor (Option ps) where
  map f (Opt props rdr) = Opt props (map f rdr)

data Parser : (a : Type) -> Type where
  NilP : Maybe a -> Parser a
  OptP : Option ps a -> Parser a
  AppP : Parser (x -> a) -> Parser x -> Parser a
  AltP : Parser a -> Parser a -> Parser a

Functor Parser where
  map f (NilP x) = NilP (map f x)
  map f (OptP x) = OptP (map f x)
  map f (AppP ff a) = AppP (map (f .) ff) a
  map f (AltP a b) = AltP (map f a) (map f b)

Applicative Parser where
  pure  = NilP . Just
  (<*>) = AppP

Alternative Parser where
  empty = NilP Nothing
  (<|>) = AltP

-- --------------------------------------------------------------------- [ EOF ]
