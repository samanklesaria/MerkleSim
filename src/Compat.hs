module Compat where
import Control.Monad.State.Strict
import Control.Monad

stateM :: Monad m => (s -> m (s, a)) -> StateT s m a
stateM f = do
  s <- get
  (s', a) <- lift $ f s
  put s'
  return a


mapAccumM :: (Monad m, Traversable t) => (s -> a -> m (s, b)) -> s -> t a -> m (t b, s)
mapAccumM f s t = runStateT (mapM (stateM . flip f) t) s
