{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -fno-warn-orphans #-}

{- |

Module      :  Happstack.Server.MonadPeel
Copyright   :  (c) Nils Schweinsberg, 2012
License     :  BSD-style

Maintainer  :  Nils Schweinsberg <mail@nils.cc>
Stability   :  experimental
Portability :  non-portable (extended exceptions)

This module defines instances of 'MonadTransPeel' and 'MonadPeelIO' for
Happstacks data types 'ServerPartT', 'FilterT' and 'WebT'.

To use these instances, add

> import Happstack.Server.MonadPeel ()

to the import list of your Haskell module.

-}

module Happstack.Server.MonadPeel
  (
  ) where

import Control.Monad.Error
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Trans.Peel
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Peel
import Happstack.Server.Internal.Monads
import Happstack.Server.Internal.Types


--------------------------------------------------------------------------------
-- TransPeel instances

instance MonadTransPeel (FilterT (FilterFun a)) where
  peel = return $ \m -> do
    (x,w) <- runWriterT $ unFilterT m
    return $ FilterT $ do
      tell w
      return x

instance MonadTransPeel WebT where
  peel = return $ \m -> do
    mxew <- runMaybeT $ runWriterT $ unFilterT $ runErrorT $ unWebT m
    return $ WebT $
      case mxew of
           Just (x,_) ->
             case x of
                  Right a -> return a
                  Left  b -> throwError b
           Nothing -> mzero

runWebT :: WebT m a -> m (Maybe (Either Response a, FilterFun Response))
runWebT m =
  runMaybeT $ runWriterT $ unFilterT $ runErrorT $ unWebT m

instance MonadTransPeel ServerPartT where
  peel = ServerPartT $ asks $ \r m -> do
    x <- runWebT $ runReaderT (unServerPartT m) r
    return $
      case x of
           Just (Right a,_) -> return a
           _                -> mzero


--------------------------------------------------------------------------------
-- PeelIO instances

instance MonadPeelIO m => MonadPeelIO (FilterT (FilterFun a) m) where
  peelIO = liftPeel peelIO

instance MonadPeelIO m => MonadPeelIO (WebT m) where
  peelIO = liftPeel peelIO

instance MonadPeelIO m => MonadPeelIO (ServerPartT m) where
  peelIO = liftPeel peelIO
