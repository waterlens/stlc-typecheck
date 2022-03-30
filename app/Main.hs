module Main where

import Data.Maybe (fromMaybe)

newtype Variable = Variable String
  deriving (Show, Eq)

data Type
  = BoolTy
  | Arrow Type Type
  deriving (Show, Eq)

data Term
  = Atom Bool
  | Var Variable
  | App Term Term
  | Lam Variable Type Term
  deriving (Show, Eq)

data Context
  = Empty
  | Extend Context (Variable, Type)
  deriving (Show, Eq)

extend :: (Variable, Type) -> Context -> Context
extend (v, t) c = Extend c (v, t)

find :: Variable -> Context -> Maybe Type
find v Empty = Nothing
find v (Extend c (v1, t1)) =
  if v == v1
    then Just t1
    else find v c

check :: Context -> Term -> Maybe Type
check c (Atom a) = Just BoolTy
check c (Var v) = find v c
check c (App a b) = do
  ta <- check c a
  tb <- check c b
  case (ta, tb) of
    (Arrow t1 t2, t3) -> if t1 == t3 then Just t1 else Nothing
    _ -> Nothing
check c (Lam v t b) = do
  t1 <- check (extend (v, t) c) b
  Just (Arrow t t1)

main :: IO ()
main = do
  print (check Empty (Atom True))
  print (check Empty (Lam (Variable "x") BoolTy (Var (Variable "x"))))
  print (check (Extend Empty (Variable "y", BoolTy)) (App (Lam (Variable "x") BoolTy (Var (Variable "x"))) (Var (Variable "y"))))
                                                                                                                                                                                                                                                                                                                                    