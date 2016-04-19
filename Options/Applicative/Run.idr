-- -------------------------------------------------------------- [ Parser.idr ]
-- Description : A command line argument parser
-- Copyright   : (c) Huw Campbell
-- --------------------------------------------------------------------- [ EOH ]
module Options.Applicative.Run

import Options.Applicative.Types
import Options.Applicative.Maybe
import Control.Monad.State
import Control.Monad.Trans

data OptWord : Type where
  ParsedWord :  OptName -> Maybe String -> OptWord

parseWord : String -> Maybe OptName
parseWord s = case unpack s of
  ('-' :: '-' :: w) => Just (LongName (pack w))
  ('-' :: w :: rs)  => Just (ShortName w)
  _                 => Nothing

searchParser : {a : Type} -> Parser a -> ({r : Type} -> Option r -> MaybeT (StateT (List String) (Either ParseError)) r) -> MaybeT (StateT (List String) (Either ParseError)) (Parser a)
searchParser (NilP x) _ = empty
searchParser (OptP o) f = map (NilP . Just) (f o)
searchParser (AppP p1 p2) f = (<|>)
  ( (\p1' => p1' <*> p2) <$> searchParser p1 f )
  ( (\p2' => p1 <*> p2') <$> searchParser p2 f )
searchParser (AltP p1 p2) f = (<|>)
  ( searchParser p1 f )
  ( searchParser p2 f )

stepParser : {a : Type} -> Parser a -> String -> MaybeT (StateT (List String) (Either ParseError)) (Parser a)
stepParser p arg = case (parseWord arg) of
  Nothing => searchParser p $ \opt => case opt of
    Opt (ArgReader fa) => lift $ lift (fa arg)
    _                  => empty
  Just w  => searchParser p $ \opt => case opt of
    Opt (FlagReader w' a)    => case elem w w' of
      True  => lift $ lift (Right a)
      False => empty
    Opt (OptionReader w' fa) => case elem w w' of
      True  => do
        args <- lift $ ST (\x => return (x, x))
        case args of
          (a :: rest) => do
            lift $ ST (\y => return ((), rest))
            lift $ lift (fa a)
          _ => lift $ lift (Left $ ErrorMsg "Input required after option ")
      False => empty
    _ => empty

evalParser : Parser a -> Maybe a
evalParser (NilP r) = r
evalParser (OptP _) = Nothing
evalParser (AppP p1 p2) = evalParser p1 <*> evalParser p2
evalParser (AltP p1 p2) = evalParser p1 <|> evalParser p2

parseError : String -> ParseError
parseError arg = ErrorMsg msg
  where
    msg = case unpack arg of
      ('-'::_) => "Invalid option `" ++ arg ++ "'"
      _        => "Invalid argument `" ++ arg ++ "'"

runParser : Parser a -> List String -> Either ParseError (a, List String)
runParser p Nil = maybeToEither (ErrorMsg "Not enough input") $ map (\p' => (p', Nil)) (evalParser p)
runParser p args@(arg :: argt) = do
  x <- runStateT (runMaybeT $ stepParser p arg) argt
  case x of
    (Just p', args') => runParser p' args'
    _                => maybeToEither (parseError arg) $ map (\x' => (x', args)) (evalParser p)

-- --------------------------------------------------------------------- [ EOF ]
