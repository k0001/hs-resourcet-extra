module Control.Monad.Trans.Resource.Extra
   ( -- * Acquire
    acquire1
   , acquireType1
   , mkAcquire1
   , mkAcquireType1
   , acquireReleaseSelf
   , unAcquire

    -- * MonadResource
   , registerType
   , releaseType
   , unprotectType
   , acquireReleaseKey

    -- * MonadMask
   , runResourceT
   , withAcquire
   , withAcquireRelease

    -- * Restore
   , Restore (..)
   , getRestoreIO
   , withRestoreIO

    -- * Async
   , asyncRestore

    -- * IO
   , once
   , onceK
   ) where

import Control.Concurrent.Async qualified as Async
import Control.Concurrent.MVar
import Control.Exception.Safe qualified as Ex
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift qualified as U
import Control.Monad.Trans.Resource qualified as R
import Control.Monad.Trans.Resource.Internal qualified as R
import Data.Acquire.Internal qualified as A
import Data.IORef
import Data.IntMap.Strict qualified as IntMap
import Data.Kind

--------------------------------------------------------------------------------

-- | Like 'A.mkAcquire1', but the acquire function is provided the current
-- 'Restore'-like function.
acquire1 :: ((forall x. IO x -> IO x) -> IO a) -> (a -> IO ()) -> A.Acquire a
acquire1 acq rel = acquireType1 acq \a _ -> rel a

-- | Like 'A.mkAcquireType1', but the acquire function is provided the current
-- 'Restore'-like function.
acquireType1
   :: ((forall x. IO x -> IO x) -> IO a)
   -> (a -> A.ReleaseType -> IO ())
   -> A.Acquire a
acquireType1 acq rel = A.Acquire \res -> do
   rel1 <- onceK $ uncurry rel
   a <- acq res
   pure $ A.Allocated a $ curry rel1 a

-- | Like 'A.mkAcquire', but the release function will be run at most once.
-- Subsequent executions of the release function will be no-ops.
mkAcquire1 :: IO a -> (a -> IO ()) -> A.Acquire a
mkAcquire1 acq = acquire1 \_ -> acq

-- | Like 'A.mkAcquireType', but the release function will be run at most once.
-- Subsequent executions of the release function will be no-ops.
mkAcquireType1 :: IO a -> (a -> A.ReleaseType -> IO ()) -> A.Acquire a
mkAcquireType1 acq = acquireType1 \_ -> acq

-- | Build an 'A.Acquire' having access to its own release function.
--
-- The release function will be run at most once. Subsequent executions of
-- the release function will be no-ops.
acquireReleaseSelf :: A.Acquire ((A.ReleaseType -> IO ()) -> a) -> A.Acquire a
acquireReleaseSelf (A.Acquire f) = A.Acquire \restore -> do
   A.Allocated g rel0 <- f restore
   rel1 <- onceK rel0
   pure $ A.Allocated (g rel1) rel1

-- | Removes the 'A.Acquire' and 'A.Allocated' wrappers.
unAcquire
   :: (MonadIO m)
   => A.Acquire a
   -> (forall x. IO x -> IO x)
   -- ^ 'Restore'-like function.
   -> m (a, A.ReleaseType -> IO ())
unAcquire (A.Acquire f) restore = liftIO do
   A.Allocated a rel <- f restore
   pure (a, rel)

--------------------------------------------------------------------------------

-- | Like 'R.runResourceT', but requires only 'Ex.MonadMask'.
runResourceT :: (Ex.MonadMask m, MonadIO m) => R.ResourceT m a -> m a
runResourceT (R.ResourceT r) = do
   istate <- liftIO R.createInternalState
   Ex.mask \restoreM -> do
      a <-
         restoreM (r istate) `Ex.catchAsync` \e -> liftIO do
            R.stateCleanupChecked (Just e) istate
            Ex.throwM e
      liftIO $ R.stateCleanupChecked Nothing istate
      pure a

-- | Like 'withAcquireRelease', but doesn't take the extra release function.
withAcquire :: (Ex.MonadMask m, MonadIO m) => A.Acquire a -> (a -> m b) -> m b
withAcquire (A.Acquire f) g = do
   Restore restoreIO <- getRestoreIO
   Ex.mask \restoreM -> do
      A.Allocated x free <- liftIO $ f restoreIO
      b <- Ex.withException (restoreM (g x)) \e ->
         liftIO $ free $ A.ReleaseExceptionWith e
      liftIO $ free A.ReleaseNormal
      pure b

-- | @'withAcquireRelease' acq \\release a -> act@ acquires the @a@ and
-- automaticaly releases it when @mb@ returns or throws an exception.
-- If desired, @release@ can be used to release @a@ earlier.
withAcquireRelease
   :: (Ex.MonadMask m, MonadIO m)
   => A.Acquire a
   -> ((A.ReleaseType -> IO ()) -> a -> m b)
   -> m b
withAcquireRelease (A.Acquire f) g = do
   Restore restoreIO <- getRestoreIO
   Ex.mask \restoreM -> do
      A.Allocated x free <- liftIO $ f restoreIO
      -- Wrapper so that we don't perform `free` again if `g` already did.
      free1 <- onceK free
      b <- Ex.withException (restoreM (g free1 x)) \e ->
         liftIO $ free1 $ A.ReleaseExceptionWith e
      liftIO $ free1 A.ReleaseNormal
      pure b

--------------------------------------------------------------------------------

-- | Like 'R.register', but gives access to the 'A.ReleaseType' too.
registerType
   :: (R.MonadResource m) => (A.ReleaseType -> IO ()) -> m R.ReleaseKey
registerType = R.liftResourceT . R.ResourceT . flip R.registerType

-- | Like 'R.release', but allows specifying the 'A.ReleaseType' too.
releaseType :: (MonadIO m) => R.ReleaseKey -> A.ReleaseType -> m ()
releaseType rk rt = liftIO $ maybe mempty ($ rt) =<< unprotectType rk

-- | Like 'R.unprotect', but allows specifying the 'A.ReleaseType' too.
unprotectType
   :: (MonadIO m) => R.ReleaseKey -> m (Maybe (A.ReleaseType -> IO ()))
unprotectType (R.ReleaseKey istate key) = liftIO do
   atomicModifyIORef' istate \case
      R.ReleaseMap next rf im
         | Just g <- IntMap.lookup key im ->
            (R.ReleaseMap next rf (IntMap.delete key im), Just g)
      rm -> (rm, Nothing)

-- | 'acquireReleaseKey' will 'unprotectType' the 'R.ReleaseKey',
-- and use 'A.Acquire' to manage the release action instead.
acquireReleaseKey :: R.ReleaseKey -> A.Acquire ()
acquireReleaseKey rk =
   void $ A.mkAcquireType (unprotectType rk) (maybe mempty id)

--------------------------------------------------------------------------------

-- | Wrapper around a “restore” function like the one given
-- by @'mask' (\\restore -> ...)@, in a particular 'Monad' @m@.
type Restore :: (Type -> Type) -> Type
newtype Restore m = Restore (forall x. m x -> m x)

-- | Get the current 'Restore' action in 'IO', wrapped in 'Restore'.
getRestoreIO :: (MonadIO m) => m (Restore IO)
getRestoreIO =
   -- Ugly, but safe. Check the implementation in base.
   liftIO $ Ex.mask \f -> pure (Restore f)

-- | Get the current 'Restore' action in 'IO', without the 'Restore' wrapper.
withRestoreIO
   :: (Ex.MonadMask m, MonadIO m) => ((forall x. IO x -> IO x) -> m a) -> m a
withRestoreIO f = getRestoreIO >>= \(Restore g) -> f g

--------------------------------------------------------------------------------

-- | Like 'R.resourceFork', but uses 'Async.Async' to communicate with the
-- background thread.
--
-- The 'Async.Async' is initially 'Ex.mask'ed. A 'Restore'-like function is
-- provided to restore to the call-site masking state.
--
-- As a convenience, the 'Async.Async' may optionally be safely 'Async.link'ed
-- by this function, too.
asyncRestore
   :: (U.MonadUnliftIO m)
   => Bool
   -- ^ Whether to 'Async.link' the 'Async.Async'.
   -> ((forall x. IO x -> IO x) -> R.ResourceT m a)
   -- ^ You may use 'U.liftIOOp' on this 'Restore'-like function.
   -> R.ResourceT m (R.ReleaseKey, Async.Async a)
asyncRestore link k =
   R.ResourceT \r -> U.withRunInIO \m2io -> Ex.mask \restoreIO -> do
      let R.ResourceT !f = k restoreIO
      R.stateAlloc r
      aa <- Async.async do
         a <- Ex.withException (m2io (f r)) \e ->
            R.stateCleanup (A.ReleaseExceptionWith e) r
         a <$ R.stateCleanup A.ReleaseNormal r
      when link $ Async.link aa
      key <- R.register' r $ Async.uninterruptibleCancel aa
      pure (key, aa)

--------------------------------------------------------------------------------

-- | @'once' ma@ creates a wrapper for @ma@ which will execute @ma@ at most
-- once. Further executions of the same wrapped @ma@ are a no-op. It's safe to
-- attempt to use the wrapper concurrently; only one thread will get to execute
-- the actual @ma@ at most.
once :: (MonadIO m, MonadIO n, Ex.MonadMask n) => n () -> m (n ())
once = fmap ($ ()) . onceK . const

-- | Kleisli version of 'once'.
onceK :: (MonadIO m, MonadIO n, Ex.MonadMask n) => (a -> n ()) -> m (a -> n ())
onceK kma = do
   done <- liftIO $ newMVar False
   pure \a ->
      Ex.bracket
         (liftIO $ takeMVar done)
         (\_ -> liftIO $ putMVar done True)
         (\d -> unless d (kma a))
