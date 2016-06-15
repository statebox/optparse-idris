-- -------------------------------------------------------------- [ Parser.idr ]
-- Description : A command line argument parser
-- Copyright   : (c) Huw Campbell
-- --------------------------------------------------------------------- [ EOH ]
module Options.Applicative.Builder

import public Text.PrettyPrint.Leijen
import        Options.Applicative.Types
import        Control.Monad.Trans
import        Control.Lens

%default total
%access public export

optRdr : Lens' (Option x a) (OptReader x a)
optRdr (MkArrow f) = MkArrow (\a => case a of
      (Opt props rdr) => (Opt props) <$> f rdr
    )

optProps : Lens' (Option x a) (OptProperties)
optProps (MkArrow f) = MkArrow (\a => case a of
      (Opt props rdr) => (flip Opt rdr) <$> f props
    )

interface HasMeta d where
  meta : { f : Type -> Type } -> Functor f => LensLike' f d String

HasMeta (OptReader OptionParams a) where
  meta (MkArrow f) = MkArrow (\a => case a of
          (OptionReader n p m) => (OptionReader n p) <$> f m
        )

HasMeta (Option OptionParams a) where
  meta = optRdr . meta

HasMeta (OptReader ArgParams a) where
  meta (MkArrow f) = MkArrow (\a => case a of
          (ArgReader p m) => (ArgReader p) <$> f m
        )

HasMeta (Option ArgParams a) where
  meta = optRdr . meta

interface HasName d where
  names : { f : Type -> Type } -> Functor f => LensLike' f d (List OptName)

HasName (OptReader OptionParams a) where
  names (MkArrow f) = MkArrow (\a => case a of
         (OptionReader n p m) => (\n' => OptionReader n' p m) <$> (f n)
        )

HasName (Option OptionParams a) where
  names = optRdr . names

HasName (OptReader FlagParams a) where
  names (MkArrow f) = MkArrow (\a => case a of
         (FlagReader n d) => (\n' => FlagReader n' d) <$> f n
        )

HasName (Option FlagParams a) where
  names = optRdr . names

interface HasHelp d where
  help : { f : Type -> Type } -> Functor f => LensLike' f d Doc

HasHelp OptProperties where
  help (MkArrow f) = MkArrow (\a => case a of
         (MkOptProperties vis hdoc) => MkOptProperties vis <$> f hdoc
        )

HasHelp (Option x a) where
  help = optProps . help

defProps : OptProperties
defProps = MkOptProperties True Empty

option : (String -> Either ParseError a) -> (Option OptionParams a -> Option OptionParams a) -> Parser a
option rdr f = OptP  (f $ Opt defProps (OptionReader [] rdr "OPT"))

strOption : (Option OptionParams String -> Option OptionParams String) -> Parser String
strOption = option Right

arg : (String -> Either ParseError a) -> (Option ArgParams a -> Option ArgParams a) -> Parser a
arg rdr f = OptP (f $ Opt defProps (ArgReader rdr "ARG"))

strArg : (Option ArgParams String -> Option ArgParams String) -> Parser String
strArg = arg Right

flag : (Option FlagParams Bool -> Option FlagParams a) -> Parser a
flag f = OptP (f $ Opt defProps (FlagReader [] True))

-- --------------------------------------------------------------------- [ EOF ]
