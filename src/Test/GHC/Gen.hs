{-# LANGUAGE CPP #-}
module Test.GHC.Gen where

#ifdef DEBUG
import Test.QuickCheck

import SrcLoc
import FastString

instance Arbitrary SrcSpan where
  arbitrary = sized $ \s -> do
      let file = mkFastString "<testing>"
      l_from <- choose (1, s+1)
      c_from <- choose (0, s)
      l_len <- choose (0, s)
      c_len <- choose (1, s+1)
      return $ 
        mkSrcSpan 
          (mkSrcLoc file l_from c_from)
          (mkSrcLoc file (l_from+l_len) (c_from+c_len))
          -- XXX: if l_len > 0 then c_len + c_from >= 0 is enough
{-
instance Show SrcSpan where
  show s
    | not (isGoodSrcSpan s) = "<unhelpful span>"
    | isOneLineSpan s = show (srcSpanStartLine s) ++ ":" ++
                        let c1 = srcSpanStartCol s
                            c2 = srcSpanEndCol s - 1
                        in if c1 < c2
                             then show c1 ++ "-" ++ show c2
                             else show c1
    | otherwise = 
        show (srcSpanStartLine s) ++ ":" ++ show (srcSpanStartCol s) ++ "-"
          ++ show (srcSpanEndLine s) ++ ":" ++ show (srcSpanEndCol s)
-}
#endif
