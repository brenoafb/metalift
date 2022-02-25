{-# LANGUAGE TemplateHaskell #-}
module Main where

import Language.Haskell.TH
import Control.Monad.Fix

import Metalift
import Preprocess
import Var
import Lib

one :: Int
one = 1

two :: Int
two = 2

expr = [| (\x -> (+) x one) one
  |]

caseExpr = [|
    case one of
      0 -> True
      _ -> False
  |]


data Expr = Add Expr Expr
          | Num Int
          deriving (Eq, Show)

eval :: Expr -> Int
eval e =
  case e of
    Add e1 e2 -> eval e1 + eval e2
    Num n -> n

eval' :: Expr -> Expr
eval' e =
  case e of
    Num n -> Num n
    Add e1 e2 ->
      let (Num n1) = eval' e1
          (Num n2) = eval' e2
      in Num ((+) n1 n2)

fixEval =
  fix (\f e ->
   case e of
     Add e1 e2 -> (+) (f e1) (f e2)
     Num n -> n)

fixEval' =
  fix (\f e ->
   case e of
     Add e1 e2 -> Num ((+) (getNum (f e1)) (getNum (f e2)))
     Num n -> Num n)

getNum :: Expr -> Int
getNum (Num n) = n
getNum _ = error "Cannot extract number from expression"

$(myDec >>= lift)

myExpr = Add (Add (Num 1) (Num 2)) (Num 3)

main :: IO ()
main = putStrLn "metalift"
