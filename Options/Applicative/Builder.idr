-- -------------------------------------------------------------- [ Parser.idr ]
-- Description : A command line argument parser
-- Copyright   : (c) Huw Campbell
-- --------------------------------------------------------------------- [ EOH ]
module Options.Applicative.Builder

import public Text.PrettyPrint.WL
import        Options.Applicative.Types
import        Control.Monad.Trans
import        Data.Morphisms
import        Data.Profunctor.Lens

%access public export

optRdr : Lensing p => Lens' {p} (Option x a) (OptReader x a)
optRdr = lens' $ \(Opt props rdr) => (Opt props, rdr)

optProps : Lensing p => Lens' {p} (Option x a) OptProperties
optProps = lens' $ \(Opt props rdr) => (flip Opt rdr, props)

interface HasMeta d where
  meta : Lensing p => Lens' {p} d String

HasMeta (OptReader OptionParams a) where
  meta = lens' $ \(OptionReader n p m) => (OptionReader n p, m)

HasMeta (OptReader ArgParams a) where
  meta = lens' $ \(ArgReader p m) => (ArgReader p, m)

HasMeta (OptReader CmdParams a) where
  meta = lens' $ \(CmdReader ps m) => (CmdReader ps, m)

HasMeta (OptReader x a) => HasMeta (Option x a) where
  meta = optRdr . meta

interface HasName d where
  names : Lensing p => Lens' {p} d (List OptName)

HasName (OptReader OptionParams a) where
  names = lens' $ \(OptionReader n p m) => (\n' => OptionReader n' p m, n)

HasName (OptReader FlagParams a) where
  names = lens' $ \(FlagReader n d) => (flip FlagReader d, n)

HasName (OptReader x a) => HasName (Option x a) where
  names = optRdr . names

short : HasName d => Char -> d -> d
short c = over names (\ns => ShortName c :: ns)

long : HasName d => String -> d -> d
long c = over names (\ns => LongName c :: ns)

interface HasSubCommands d a where
  cmds : Lensing p => Lens' {p} d (List (String, (Parser a)))

HasSubCommands (OptReader CmdParams a) a where
  cmds = lens' $ \(CmdReader ps m) => (flip CmdReader m, ps)

HasSubCommands (Option CmdParams a) a where
  cmds = optRdr . cmds

cmd : HasSubCommands d a => String -> Parser a -> d -> d
cmd n p = over cmds (\ns => (n,p) :: ns)

interface HasHelp d where
  help : Lensing p => Lens' {p} d Doc

HasHelp OptProperties where
  help = lens' $ \(MkOptProperties vis hdoc) => (MkOptProperties vis, hdoc)

HasHelp (Option x a) where
  help = optProps . help

interface HasVisibility d where
  visibility : Lensing p => Lens' {p} d Visibility

HasVisibility OptProperties where
  visibility = lens' $ \(MkOptProperties vis hdoc) => (flip MkOptProperties hdoc, vis)

HasVisibility (Option x a) where
  visibility = optProps . visibility

hide : HasVisibility d => d -> d
hide = visibility .~ Hidden

internal : HasVisibility d => d -> d
internal = visibility .~ Internal

interface HasValue (d : Type -> Type) where
  value : Lensing p => Lens {p} (d a) (d b) a b

HasValue (OptReader FlagParams) where
  value = lens' $ \(FlagReader n d) => (FlagReader n, d)

HasValue (Option FlagParams) where
  value = lens' $ \(Opt props (FlagReader n d)) => (Opt props . FlagReader n, d)

defProps : OptProperties
defProps = MkOptProperties Visible empty

option : (String -> Either ParseError a) -> (Option OptionParams a -> Option OptionParams a) -> Parser a
option rdr f = OptP $ f $ Opt defProps (OptionReader [] rdr "OPT")

strOption : (Option OptionParams String -> Option OptionParams String) -> Parser String
strOption = option Right

arg : (String -> Either ParseError a) -> (Option ArgParams a -> Option ArgParams a) -> Parser a
arg rdr f = OptP $ f $ Opt defProps (ArgReader rdr "ARG")

strArg : (Option ArgParams String -> Option ArgParams String) -> Parser String
strArg = arg Right

flag' : a -> (Option FlagParams a -> Option FlagParams a) -> Parser a
flag' d f = OptP $ f $ Opt defProps (FlagReader [] d)

flag : (Option FlagParams Bool -> Option FlagParams a) -> Parser a
flag f = OptP $ f $ Opt defProps (FlagReader [] True)

subparser : (Option CmdParams a -> Option CmdParams a) -> Parser a
subparser f = OptP $ f $ Opt defProps (CmdReader [] "COMMAND")

-- --------------------------------------------------------------------- [ EOF ]
