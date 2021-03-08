{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Confused

where

import RIO
import Control.Lens (Getter)


type CanKeycode c = (Eq c, Show c, Display c)

data KeyEvent c where
  KeyEvent :: CanKeycode c => Bool -> c -> KeyEvent c

--------------------------------------------------------------------------------

class CanKeyIO a where

  type Keycode  a :: *
  type KeyIOCfg a :: *

  getKeyL   :: MonadIO m       => Getter a (m (KeyEvent (Keycode a)))
  putKeyL   :: MonadIO m       => Getter a (KeyEvent (Keycode a) -> m ())
  withKeyIO :: MonadUnliftIO m => KeyIOCfg a -> (a -> m b) -> m b

--------------------------------------------------------------------------------

class CanKeyIO a => HasKeyIO env a where
  keyIOL :: Getter env a


getKey :: (MonadReader env m, MonadIO m, HasKeyIO env a)
  => m (KeyEvent (Keycode a))
getKey = do
  env <- ask
  env^.keyIOL.getKeyL
