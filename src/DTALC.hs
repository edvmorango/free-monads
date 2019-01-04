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

-- 10 + 10
-- In (f (Exp f))
-- In (LR (Exp LR))
-- In (R (Add (Exp L) (Exp L)))
-- In (R (In ( Add (L (Exp L))) (In (L (Exp L))))) 
-- In before a applied Exp 
addExample :: Exp (Val :+: Add)
addExample = In (R ((Add (In (L (Val 10))) (In (L (Val 10))))))
