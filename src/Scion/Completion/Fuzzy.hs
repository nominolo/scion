{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Scion.Utils
-- Copyright   : (c) Thomas Schilling 2008
-- License     : BSD-style
--
-- Maintainer  : nominolo@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Fuzzy completion support.
--
-- Fuzzy completion uses a scoring algorithm to rate completions.  For example
-- @wf@ may complete to @withFile@ since it matches both words (distinguished
-- by CamelCase.)  Using a dot separates forces the first part to complete on
-- a package name or alias.  For example @m.fo@ matches @Data.Map.fold@.  All
-- matching is case-insensitive so it is faster to type.
--
module Scion.Completion.Fuzzy where

import Data.Maybe

import qualified Debug.Trace


-- TODO: use something like tries?

-- TODO: efficiency?

data Matching = Matching { matchings :: [(Int, String)]
                         , fullMatch :: String
                         }

-- TODO: package prefixes
-- findMatches :: [Char] -> [String] -> [Matching]
-- findMatches short candidates = concat $ catMaybes (findMatch short) candidates

{-
    abc -> ABC
           AdBeC
           AaBC
           aABC
-}

traceOn = False
trace s a | traceOn = Debug.Trace.trace s a
          | True    = a

--findMatch :: [Char] -> String -> [[]]
findMatch short candidate = go 0 short candidate
  where
    -- recursively looks for matchings
    go :: Int -> [Char] -> String -> [[(Int, Char)]]
    go !_n []  _ys = []
    go !_n _xs []  = []
    go !n xs@(x:xs') (y:ys)
       | x == y = (case go (n+1) xs' ys of
                    [] -> [[(n,x)]]
                    ms -> [(n,x) : m | m <- ms ])
                  ++ go (n+1) xs ys
       | True  = go (n+1) xs ys

-- wrong
occsToChunks :: [(Int, Char)] -> [(Int,String)]
occsToChunks cs = go cs [] (0 :: Int)
  where
    go ((n,c):os) cs !cn
       | cn == n = go os (c:cs) (n+1)
       | otherwise = (cn, reverse cs) : go os [c] (n+1)
    go [] cs !cn = [(cn,reverse cs)]