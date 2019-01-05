module Definition where

-- I believe the left fmap acts as a glue to the structure that will be interpreted in the end 
data Free f a
  = Pure a
  | Free (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Free m) = Free $ fmap (fmap f) m

instance Functor f => Applicative (Free f) where
  pure a = Pure a
  Pure f <*> Pure a = Pure (f a)
  Pure f <*> Free m = Free $ (fmap f) <$> m
  Free m <*> param = Free ((<*> param) <$> m)

instance Functor f => Monad (Free f) where
  return a = pure a
  Pure a >>= f = f a
  Free m >>= f = Free $ fmap (>>= f) m
