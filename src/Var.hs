module Var where

type Tag = String

data V a = Obj a
         | Chc Tag (V a) (V a)
         deriving (Eq, Show)

instance Functor V where
  fmap f (Obj x) = Obj (f x)
  fmap f (Chc t v1 v2) = Chc t (fmap f v1) (fmap f v2)

instance Applicative V where
  pure = Obj
  (Obj f) <*> v = fmap f v
  (Chc t f1 f2) <*> v = Chc t (f1 <*> v) (f2 <*> v)

instance Monad V where
  (Obj x) >>= f = f x
  (Chc t v1 v2) >>= f = Chc t (v1 >>= f) (v2 >>= f)

select :: Tag -> V a -> V a
select t (Obj x) = Obj x
select t (Chc t' v1 v2)
  | t == t' = select t v1
  | otherwise = Chc t' (select t v1) (select t v2)
