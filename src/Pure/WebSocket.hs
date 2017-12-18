{-# LANGUAGE CPP #-}
#ifdef USE_TEMPLATE_HASKELL
{-# LANGUAGE TemplateHaskell #-}
#endif
{-# LANGUAGE ViewPatterns #-}
module Pure.WebSocket 
  (
#ifdef USE_TEMPLATE_HASKELL
    mkRequest, 
    mkMessage, 
#endif
    module Export
  ) where

#ifdef __GHCJS__
import Pure.WebSocket.GHCJS    as Export
#else
import Pure.WebSocket.GHC      as Export
#endif

import Pure.WebSocket.API      as Export
import Pure.WebSocket.Dispatch as Export
import Pure.WebSocket.Endpoint as Export
import Pure.WebSocket.Message  as Export
import Pure.WebSocket.Request  as Export
import Pure.WebSocket.TypeRep  as Export

import Data.Char
import Data.Proxy

#ifdef USE_TEMPLATE_HASKELL
import Language.Haskell.TH
import Language.Haskell.TH.Lib

mapHead f [] = []
mapHead f (x:xs) = f x : xs

#ifdef USE_TEMPLATE_HASKELL
processName str = (mkName str,mkName $ mapHead toLower str)

mkRequest :: String -> TypeQ -> Q [Dec]
mkRequest (processName -> (dat,rq))  ty = do
  rr <- ty
  case rr of
    (AppT (AppT ArrowT req) rsp) -> do
      let dataDec = DataD [] dat [] Nothing [] []
          proxyFunTy  = SigD rq (ConT ''Proxy `AppT` ConT dat)
          proxyFunDec = FunD rq [ Clause [] (NormalB (ConE 'Proxy)) [] ]
          requestInstanceDec = InstanceD Nothing [] (ConT ''Request `AppT` ConT dat)
            [ TySynInstD ''Req (TySynEqn [ ConT dat ] (AppT (AppT (TupleT 2) (ConT ''Int)) req))
            , TySynInstD ''Rsp (TySynEqn [ ConT dat ] rsp)
            ]
      return [dataDec,proxyFunTy,proxyFunDec,requestInstanceDec]
    _ -> error $ "Invalid request type for " ++ show dat

mkMessage :: String -> TypeQ -> Q [Dec]
mkMessage (processName -> (dat,msg)) ty = do
  message <- ty
  let dataDec = DataD [] dat [] Nothing [] []
      proxyFunTy  = SigD msg (ConT ''Proxy `AppT` ConT dat)
      proxyFunDec = FunD msg [ Clause [] (NormalB (ConE 'Proxy)) [] ]
      messageInstanceDec = InstanceD Nothing [] (ConT ''Message `AppT` ConT dat)
        [ TySynInstD ''M (TySynEqn [ ConT dat ] message) ]
  return [dataDec,proxyFunTy,proxyFunDec,messageInstanceDec]
#endif