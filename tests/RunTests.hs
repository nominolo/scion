module Main where

import Test.QuickCheck

import Scion.Inspect

check1000 :: Testable prop => String -> prop -> IO ()
check1000 s p = putStrLn ("> " ++ s) >> quickCheckWith stdArgs{ maxSuccess = 1000 } p

main = do
    putStrLn "================================================"
    putStrLn ">>> Running Test Suite"
    check1000 "prop_invCmpOverlap" prop_invCmpOverlap