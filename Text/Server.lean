import Std.Http

open Std Http Server

def serverConfig : Config where
  maxBodySize  := 1024 * 1024
  maxChunkSize := 1024 * 1024

structure Text where

instance : Handler Text where
  onRequest _ _ :=
    Response.ok |>.text "Hello, World!"
