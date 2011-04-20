{-# LANGUAGE MultiParamTypeClasses #-}
module Scion.Utils.Convert where

-- | A type class representing things we can convert from and to.
class Convert from to where
  convert :: from -> to

instance Convert Int Integer where
  convert = fromIntegral

instance Convert Integer Int where
  convert = fromInteger