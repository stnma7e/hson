module Main where

import System.IO
import System.Directory

import Json
import Print

main :: IO ()
loop :: Handle -> IO ()

main = do
    let fname = "test.json"
    outh <- openFile fname ReadWriteMode
    let jnum  = JValue ("num1", (JNumber 1.01))
    let jnull = JValue ("null1", JNull)
    let jbool = JValue ("bool1", (JBool True))
    let jstr  = JValue ("str1", (JString "string"))
    let jobj  = JValue ("obj1", (JObject [jnum,jnull,jbool,jstr]))
    let jarry = JValue ("arry1", (JArray [(JObject [jnum,jnull,jbool,jstr]),(JObject [jnum,jnull,jbool,jstr])]))
    hPutStrLn outh (show (Print.Enclose "{ "
        (
            (renderJson jarry) 
         <> (renderJson (JValue ("emptyObj", (JObject []))))
         <> (renderJson (JValue ("emptyArry", (JArray []))))
        ) " }") )
    hSeek outh AbsoluteSeek 0
    loop outh
    hClose outh
    -- removeFile fname

loop inh = do
    iseof <- hIsEOF inh
    if iseof
        then return ()
    else do inpStr <- hGetLine inh
            putStrLn inpStr
            loop inh
