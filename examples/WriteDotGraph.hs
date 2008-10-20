
module WriteDotGraph (writeDotGraph) where

import Data.List (nub)

writeDotGraph :: (Show node, Eq node) => [(node, node)] -> String
writeDotGraph edges = 
  unlines (
      [header
      ,graphDefaultAtribs
      ,nodeDefaultAtribs
      ,edgeDefaultAtribs]
    ++ map makeNode nodes
    ++ map makeEdge edges
    ++ [footer]
  )
  where nodes = nub $ concat [ [a,b] | (a,b) <- edges ]
        makeNode name = "\t" ++ show (show name) ++ " [];"
        makeEdge (node1, node2) = 
            "\t" ++ show (show node1) ++ " -> " ++ show (show node2) ++ "[];"

header = "digraph g {"
footer = "}"

graphDefaultAtribs = "\tgraph [fontsize=14, fontcolor=black, color=black];" 
nodeDefaultAtribs  = "\tnode [label=\"\\N\", width=\"0.75\", shape=ellipse];"
edgeDefaultAtribs  = "\tedge [fontsize=10];"
