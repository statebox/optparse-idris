-- -------------------------------------------------------------- [ Parser.idr ]
-- Description : A command line argument parser
-- Copyright   : (c) Huw Campbell
-- --------------------------------------------------------------------- [ EOH ]
module Options.Applicative.Maybe
import Control.Monad.Trans

%default total
%access public export

record MaybeT (m : Type -> Type) (a : Type) where
  constructor MT
  runMaybeT :  m (Maybe a)

Functor f => Functor (MaybeT f) where
    map f (MT g) = MT (map (map f) g)

Monad f => Applicative (MaybeT f) where
    pure x = MT (pure (Just x))

    (MT f) <*> (MT a) = MT (do g <- f
                               case g of
                                 Just h  => map (map h) a
                                 Nothing => pure Nothing)

Monad f => Alternative (MaybeT f) where
    empty = MT (pure Nothing)
    (MT a) <|> (MT b) = MT (do a' <- a
                               case a' of
                                 Just a'' => pure (Just a'')
                                 _        => b)


MonadTrans MaybeT where
    lift x = MT (map Just x)

Monad f => Monad (MaybeT f) where
    (MT f) >>= k = MT (do g <- f
                          case g of
                            Just a  => runMaybeT (k a)
                            Nothing => (pure Nothing))

-- --------------------------------------------------------------------- [ EOF ]
