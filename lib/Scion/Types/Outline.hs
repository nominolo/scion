
module Scion.Types.Outline 
( OutlineDef(..),
  extractNames,trimLocationFile)
  where


import GHC

import Scion.Types.Notes

data OutlineDef=OutlineDef {
	od_name::Either Name String,
	od_type::String,
	od_loc::Location,
	od_block::Location,
	od_parentName::Maybe Name
	}

	
extractNames:: [OutlineDef] -> [Name]
extractNames=foldl (\l od->case od_name od of
	Left n->(n:l)
	Right _->l) [] 
	
trimLocationFile:: [OutlineDef] -> [OutlineDef]
trimLocationFile = map (\a@OutlineDef{od_loc=l,od_block=b}->a{od_loc=trimFile l,od_block=trimFile b})
