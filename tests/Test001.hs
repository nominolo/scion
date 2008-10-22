module Main where

import Data.Maybe ( fromMaybe )

main = print (fromMaybe 0 (Just 3))

data Foo = Foo0
         | Foo2 Foo Foo

instance Show Foo where
    show Foo0 = "*"
    show (Foo2 f1 f2) = "(" ++ show f1 ++ "-" ++ show f2 ++ ")"

f x ys =
    [ (x, y) | Just y <- ys
             , length y > 3 ]
