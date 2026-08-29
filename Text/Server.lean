import Std.Http

open Std Http Server

def serverConfig : Config where
  maxBodySize  := 1024 * 1024
  maxChunkSize := 1024 * 1024

structure Text where

instance : Handler Text where
  onRequest _ req :=
    match req.line.method, req.line.uri.path with
    | .get,  { segments := #[],    absolute := true } =>
      Response.ok |>.text "hi GET"
    | .post, { segments := #[],    absolute := true } =>
      Response.ok |>.text "hi POST"
    | .get,  { segments := #[seg], absolute := true } =>
      Response.ok |>.text <| toString seg
    | _, _ =>
      Response.notFound |>.text "not found"
