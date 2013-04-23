module Print (
 Doc (..)
 , (<>)
) where

import Data.Char

data Doc = Empty
         | Literal String
         | String String
         | Number Double
         | Pair (Doc, Doc)
         | Enclose String Doc String
         | Concat (Doc,Doc)
instance Show Doc where
	show (Literal str)		         = str
	show (String str) 		         = ("\"" ++ str ++ "\"")
	show (Number num)  		         = (show num)
	show (Concat (doc1,doc2))      = showAll ( Concat (doc1,doc2) )
	show (Pair (doc1,doc2))	       = show doc1 ++ ": " ++ show doc2
	show (Enclose open doc close)  = open ++ (show doc) ++ close
	show _ 	 		  	           	   = ""

(<>) :: Doc -> Doc -> Doc 	-- concatenates Doc
(<>) doc1 doc2 = Concat (doc1,doc2)

showAll :: Doc -> String
showAll ( Concat (obj1, (Empty)) ) = show (obj1)
showAll ( Concat (doc1,doc2) )	   = show doc1 ++ ", " ++ show doc2
showAll _                          = ""
