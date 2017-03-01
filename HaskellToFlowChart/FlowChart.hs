module FlowChart where


import Prelude
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass
import StringTools


main = undefined

type Title = String
type Code  = String
type BranchPtr = String

data Path = TitlePath (Title, Tree) deriving (Show)
instance Pretty Path where
  pPrint (TitlePath (title, tree)) = text (nestString Bracket title)
                                $+$ pPrint tree
                                $+$ text ""
                                
data Tree = Branch UMLetActiviyAIO 
          | Node Code 
          deriving (Show)
instance Pretty Tree where
  pPrint (Branch uml) = pPrint uml
  pPrint (Node uml)   = pPrint uml


data UMLetActiviyAIO = Empty 
                     | Cell Code UMLetActiviyAIO 
                     | If [Path] 
                     deriving (Show)
instance Pretty UMLetActiviyAIO where
  pPrint Empty = text ""
  pPrint (Cell code uml) = text code
                       $+$ pPrint uml
  pPrint (If paths) = text "If"
                  $+$ foldl prettyAndMerge (text "") paths
                  $+$ text "EndIf"

prettyAndMerge :: Doc -> Path -> Doc 
prettyAndMerge acc path = acc $+$ (pPrint path)

prepunctuate :: Doc -> [Doc] -> [Doc]
prepunctuate _ []     = []
prepunctuate p (d:ds) = d : map (p <>) ds

tab :: Doc
tab = text "\t"