-- https://markkarpov.com/post/free-monad-considered-harmful.html
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

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

-- m -> Pure
-- e -> Impure
-- So basically the idea is to extract values from effect, to treat them as pure values?
foldFree :: Functor f => (a -> b) -> (f b -> b) -> Free f a -> b
foldFree m _ (Pure a) = m a
foldFree m e (Free f) = e (fmap (foldFree m e) f)

------------------------
data Terminal a
  = ReadLn (String -> a)
  | PrintLn String
            a
  deriving (Functor)

--- TerminalM  = Free Terminal :: * -> *
type TerminalM = Free Terminal

liftF :: Functor f => f a -> Free f a
liftF = Free . fmap return

getLn :: TerminalM String
getLn = Free $ ReadLn return

printLn :: String -> TerminalM ()
printLn s = liftF $ PrintLn s ()

terminalProgram :: TerminalM ()
terminalProgram = do
  a <- getLn
  b <- getLn
  printLn (a ++ " " ++ b)
{-
terminalInterpreter :: TerminalM a -> IO a
terminalInterpreter terminal = do
  a <-
    foldFree
      (\case
         ReadLn f -> f <$> getLine
         PrintLn str a -> a <$ putStrLn str)
      (\(Pure a) -> a)
      terminal
  return a
  -}
