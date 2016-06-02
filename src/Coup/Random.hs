module Coup.Random where

-- newtype RandomT m a = RandomT { unRandomT :: StateT StdGen m a } deriving (Functor, Monad, MonadTrans)

-- runRandomT :: (Monad m) => RandomT m a -> StdGen -> m (StdGen, a)
-- runRandomT = runStateT . unRandomT

-- next :: (Random a) => (a, a) -> RandomT m a
-- next = undefined

