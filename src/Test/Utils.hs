module Test.Utils where

import GHC
import GHC.Paths
import Outputable
import MonadUtils
import Bag

import Scion
import Scion.Inspect

import Data.Generics.UniplateStr
import Data.Generics.Biplate

newtype TestName = TestName String deriving (Eq, Show, Ord)
nm :: String -> TestName
nm = TestName

instance Outputable TestName where ppr (TestName x) = text x
instance OutputableBndr TestName where
    pprBndr _ (TestName x) = char '|' <> text x <> char '|'


pprint :: Outputable p => p -> IO ()
pprint p = runGhc (Just libdir) $ liftIO $ putStrLn $ showSDoc $ ppr p


t_001 = pprint $ universe (VarPat (nm "foo"))
t_002 = pprint $ universe $ BangPat (noLoc (VarPat (nm "b")))
t_003 = pprint $ universe $ ListPat [noLoc (VarPat (nm "b")),
                                     noLoc (BangPat (noLoc (VarPat (nm "a"))))]
                                    undefined
t_004 = pprint $ universe $ ConPatIn (noLoc (nm "Foo"))
                                     (PrefixCon [noLoc (VarPat (nm "b"))])
t_005 = pprint $ universe $ ConPatIn (noLoc (nm ":="))
                                     (InfixCon (noLoc (VarPat (nm "a")))
                                               (noLoc (VarPat (nm "b"))))
t_006 = pprint $ universe $ 
          ConPatIn (noLoc (nm "Rec"))
                   (RecCon (HsRecFields [HsRecField (noLoc (nm "f1"))
                                                    (noLoc (VarPat (nm "a")))
                                                    False]
                                        Nothing))
t_007 = pprint $ (universeOn biplate bs :: [Pat TestName])--[ n | n@(VarPat (TestName _)) <- universeBi bs ]

bs = [ VarPat (nm "a"), VarPat (nm "b"), VarPat (nm "c")]