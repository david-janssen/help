{-# LANGUAGE TemplateHaskell #-}



module Confused

where

import RIO
import Control.Lens.TH
import Control.Exception.Lens

--------------------------------------------------------------------------------
-- Base exceptions
--------------------------------------------------------------------------------

-- | Base-level exception type
data FileException
  = LockedException   Int  -- ^ File was locked
  | NotFoundException Text -- ^ File couldn't be found
  deriving Show
makeClassyPrisms ''FileException

-- | Another base-level exception
data TCPException
  = TimeoutException  Int   -- ^ Some activity timed out
  | ForbiddenException Text -- ^ Some activity was forbidden
  deriving Show
makeClassyPrisms ''TCPException

-- | Making the base exceptions exceptions and hooking them up to SomeException
instance Exception FileException
instance Exception TCPException
instance AsFileException SomeException where _FileException = exception
instance AsTCPException  SomeException where _TCPException  = exception

--------------------------------------------------------------------------------
-- App exception
--------------------------------------------------------------------------------

-- | Create an app-level sum-type of exceptions
data AppException
  = AppFileException FileException
  | AppTCPException  TCPException
  deriving Show
makeClassyPrisms ''AppException

-- | Hook this one up too
instance Exception AppException
instance AsAppException SomeException where _AppException = exception

-- | Hook AppException into the AsClasses of its contained exceptions
instance AsFileException AppException where _FileException = _AppFileException
instance AsTCPException  AppException where _TCPException  = _AppTCPException

--------------------------------------------------------------------------------
-- Demonstration
--------------------------------------------------------------------------------

-- | Activity that throws a particular FileException
go :: IO ()
go = throwing _LockedException 0

-- | Activity that throws an AppException containing a FileException
--
-- NOTE: I don't think I should have to explicitly throw at the App-level. What
-- I am hoping to achieve is to throw, for example, _LockedException, and then
-- catch it with an _AppException.
goApp :: IO ()
goApp = throwing (_AppException . _LockedException) 0

-- | The stuff that already works
works :: IO ()
works = do
  -- Catching a specific base-level error
  catching _LockedException go $ \e ->
    print $ "1. _LockedException caught: " <> show e

  -- Catching all base-level errors within the ADT
  catching _FileException go $ \e ->
    print $ "2. _FileException caught: " <> show e

  -- Correctly doesn't work
  handling_ _FileException (print "3. Could not catch with TCPException") $
    catching_ _TCPException go $
      print $ "this correctly never gets called"

  -- This works, but requires the error thrown at an explicit level
  catching _AppException goApp $ \e ->
    print $ "4. _AppException caught explicit _AppException: " <> show e

-- | Stuff I would like to work but doesn't...
noWork :: IO ()
noWork = do

  -- This fails with "*** Exception: LockedException 0"
  catching _AppException go $ \e ->
    print $ "1. _AppException caught: " <> show e
