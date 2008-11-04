
module WriteDotGraph (writeDotGraph) where

import Data.List (nub, intercalate)

writeDotGraph :: (Show node, Eq node) => [(node, [String], node)] -> String
writeDotGraph edges = 
  unlines (
      [header
      ,graphDefaultAtribs
      ,nodeDefaultAtribs
      ,edgeDefaultAtribs
      ,otherDefaults]
    ++ map makeNode nodes
    ++ map makeEdge edges
    ++ [footer]
  )
  where nodes = nub $ concat [ [a,b] | (a,_,b) <- edges ]
        makeNode name = "\t" ++ show (show name) ++ " [];"
        makeEdge (node1, attribs, node2) = 
            "\t" ++ show (show node1) ++ " -> " ++ show (show node2) 
                     ++ "[" ++ intercalate "," attribs ++ "];"

header = "digraph g {"
footer = "}"

graphDefaultAtribs = "\tgraph [fontsize=7, fontcolor=black, color=black];" 
nodeDefaultAtribs  = "\tnode [label=\"\\N\", width=\"0.3\", shape=plaintext];"
edgeDefaultAtribs  = "\tedge [fontsize=5,colorscheme=rdylgn11];"
otherDefaults = "\tranksep=5;\n\tratio=auto"