-- Data types à la carte
module DTALC where

data Expr
  = Val Int
  | Add Expr
        Expr

eval :: Expr -> Int
eval (Val a) = a
eval (Add a b) = eval a + eval b

render :: Expr -> String
render (Val a) = show a
render (Add a b) = show $ render a ++ " + " ++ render b
