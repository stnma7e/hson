module Json
(
  JData (..)
  , JValue (..)
  , renderJson
) where

import Print

data JValue = JValue (String, JData)
data JData  = JNull
            | JBool Bool
            | JString String
            | JNumber Double
            | JObject [JValue]
            | JArray [JData]
            
renderJson :: JValue -> Doc
renderJson (JValue (jval,val2)) = Print.Pair ((Print.String jval), (renderJData val2))

renderJData :: JData -> Doc
renderJData (JNull)          = Print.Literal "null"
renderJData (JBool True)     = Print.Literal "true"
renderJData (JBool False)    = Print.Literal "false"
renderJData (JString str)    = Print.String str
renderJData (JNumber num)    = Print.Number num
renderJData (JObject ([]))   = Print.Enclose "{" Print.Empty "}"
renderJData (JObject (x:xs)) = Print.Enclose "{" ((renderJson x) <> (renderJValueList xs)) "}"
renderJData (JArray ([]))    = Print.Enclose "[" Print.Empty "]"
renderJData (JArray (x:xs))  = Print.Enclose "[" ((renderJData x) <> (renderJDataList xs)) "]"

renderJValueList :: [JValue] -> Doc
renderJValueList (x:xs) = (renderJson x) <> (renderJValueList xs)
renderJValueList _      = Print.Empty

renderJDataList :: [JData] -> Doc
renderJDataList (x:xs) = (renderJData x) <> (renderJDataList xs)
renderJDataList _      = Print.Empty