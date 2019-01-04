-- Data types à la carte
{-# LANGUAGE TypeOperators #-}

module DTALC where

data Expr0
  = Val0 Int
  | Add0 Expr0
         Expr0

eval :: Expr0 -> Int
eval (Val0 a) = a
eval (Add0 a b) = eval a + eval b

render :: Expr0 -> String
render (Val0 a) = show a
render (Add0 a b) = show $ render a ++ " + " ++ render b

---------------------------
data Exp f =
  In (f (Exp f))

data Val a =
  Val Int

type IntExp = Exp Val

data Add e =
  Add e
      e

type AddExp = Exp Add

data (f :+: g) e
  = L (f e)
  | R (g e)

-- 10 + 11
-- In (f (Exp f))
-- In (LR (Exp LR))
-- In (R (Add (Exp L) (Exp L)))
-- In (R ( Add (In (L (Exp L))) (In (L (Exp L))))) 
-- In before a applied Exp 
addExample :: Exp (Val :+: Add)
addExample = In (R ((Add (In (L (Val 10))) (In (L (Val 11))))))

-- Right now Val is binded to a Int
-- So  (a -> b) cannot exist just endomorphisms. 
instance Functor Val where
  fmap f (Val a) = Val a

instance Functor Add where
  fmap f (Add a a') = Add (f a) (f a')

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (L e) = L (f <$> e)
  fmap f (R e) = R (f <$> e)

-- foldExp f (In (R (Add ( In (L (Val 10))) (In (L (Val 11))))))
-- foldExp f (In t) = f $ (foldExp f) <$> (R (Add  (In (L (Val 10))) (In (L (Val 11)))))
-- foldExp f (In t) = f $ (foldExp f) <$> (L (Val 10))
--					  (L (Val 11)) -- this product is parallelizable, so all products can be?
--  f contains the evaluation, so:
--  fold removes the `In` prefix
--  f evaluate the expression
--
--		*
--	*		*
--   *     * 	     *	   *      	
--
--Evaluation will always occur first in the end of branchs.
foldExp :: Functor f => (f a -> a) -> Exp f -> a
foldExp f (In t) = f $ (foldExp f) <$> t
