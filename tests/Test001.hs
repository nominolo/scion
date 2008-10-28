{-# OPTIONS_GHC -fglasgow-exts #-}
module Main where

import Data.Maybe ( fromMaybe )

main = print (fromMaybe 0 (Just 3))
{-
data Foo = Foo0
         | Foo2 Foo Foo

instance Show Foo where
    show Foo0 = "*"
    show (Foo2 f1 f2) = "(" ++ show f1 ++ "-" ++ show f2 ++ ")"

f x ys =
    [ (x, y) | Just y <- ys
             , length y > 3 ]

Just x = Just g

g = x
-}
data T :: * -> * where
  T :: Eq a => a -> T a

x = undefined

T i = T 33

f (T i) = i == 42

b = i == x

r x y = case compare x y of
          GT -> x + y
          _  -> x - y