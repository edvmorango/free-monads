-- Data types à la carte
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DTALC where

data Expr0
  = Val0 Int
  | Add0 Expr0
         Expr0

eval :: Expr0 -> Int
eval (Val0 a) = a
eval (Add0 a b) = eval a + eval b

render0 :: Expr0 -> String
render0 (Val0 a) = show a
render0 (Add0 a b) = show $ render0 a ++ " + " ++ render0 b

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

class Functor f =>
      Eval f
  where
  evalAlgebra :: f Int -> Int

instance Eval Val where
  evalAlgebra (Val a) = a

-- `a` and `b` already are evaluated
instance Eval Add where
  evalAlgebra (Add a b) = a + b

instance (Eval f, Eval g) => Eval (f :+: g) where
  evalAlgebra (L l) = evalAlgebra l
  evalAlgebra (R r) = evalAlgebra r

eval0 :: Eval f => Exp f -> Int
eval0 exp = foldExp evalAlgebra exp

infixl 6 ⊕

inject :: (g :<: f) => g (Exp f) -> Exp f
inject = In . inj

--  any f who supports Val
--  Val :<: f
val :: (Val :<: f) => Int -> Exp f
val a = inject (Val a)

(⊕) :: (Add :<: f) => Exp f -> Exp f -> Exp f
x ⊕ y = inject (Add x y)

class (Functor sub, Functor sup) =>
      sub :<: sup
  where
  inj :: sub a -> sup a

-- reflexive
instance Functor f => f :<: f where
  inj = id

-- supports  f at left, so the coproduct supports f
-- Overlaps because of the instance above
instance {-# OVERLAPS #-} (Functor f, Functor g) => f :<: (f :+: g) where
  inj = L

-- g supports f, so the coproduct supports f
instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = R . inj

injectExample :: Exp (Add :+: Val)
injectExample = val 8 ⊕ val 5 ⊕ val 10

data Mul x =
  Mul x
      x

instance Functor Mul where
  fmap f (Mul x x') = Mul (f x) (f x')

instance Eval Mul where
  evalAlgebra (Mul x x') = x * x'

infixl 7 ⊗

(⊗) :: (Mul :<: f) => Exp f -> Exp f -> Exp f
x ⊗ x' = inject (Mul x x')

injectMulExample :: Exp (Val :+: Mul)
injectMulExample = (val 2) ⊗ (val 5)

-- Maybe don't work because of overlaps
--injectMulAddExample :: Exp (Val :+: Mul :+: Add)
--injectMulAddExample = (val 2) ⊗ (val 5) ⊕ (val 5)
-----------------------------------------------------
class Render f where
  render :: f (Exp f) -> String

pretty :: Render f => Exp f -> String
pretty (In t) = render t

instance Render Val where
  render (Val i) = show i

instance Render Add where
  render (Add a a') = show $ (pretty a) ++ " + " ++ (pretty a')

instance Render Mul where
  render (Mul a a') = show $ (pretty a) ++ " * " ++ (pretty a')

-- Occurs check: cannot construct the infinite type: g ~ f :+: g
--instance (Render f, Render g) => Render (f :+: g) where
--  render (L l) = render l
--  render (R r) = render r
--
------------------------------------------------------------------------------
-- Free monads
data Term f a
  = Pure a
  | Impure (f (Term f a))

-- The left-adjoint grants the structure?
-- Wants to operate in `a`
instance Functor f => Functor (Term f) where
  fmap f (Pure x) = Pure (f x)
  fmap f (Impure t) = Impure $ fmap (fmap f) t

instance Functor f => Applicative (Term f) where
  pure x = Pure x
  f <*> (Impure t) = Impure $ fmap (f <*>) t

instance (Applicative f) => Monad (Term f) where
  return x = Pure x
  (Pure x) >>= f = f x
  (Impure t) >>= f = Impure (fmap (>>= f) t)

data Incr t =
  Incr Int
       t

data Recall t =
  Recall (Int -> t)

injectTerm :: (g :<: f) => g (Term f a) -> Term f a
injectTerm = Impure . inj

incr :: (Incr :<: f) => Int -> Term f ()
incr i = injectTerm $ Incr i (Pure ())

recall :: (Recall :<: f) => Term f Int
recall = injectTerm $ Recall pure

tick :: Term (Recall :+: Incr) Int
tick = do
  y <- recall
  _ <- incr 1
  return y
