module Nuclear.API.Unification where

import Ef.Base

import Nuclear.API.Interface
import Nuclear.API.Implementation

import Unsafe.Coerce

data UnifiedAPI localMessages localRequests remoteMessages remoteRequests
  = UnifiedAPI
      (API localMessages localRequests)
      (API remoteMessages remoteRequests)

unify :: API localMessages localRequests -> API remoteMessages remoteRequests -> UnifiedAPI localMessages localRequests remoteMessages remoteRequests
unify = UnifiedAPI

enact :: forall msg req messages requests messages' requests' remoteMessages remoteRequests self super.
         ( Monad super, MonadIO super
         , EnactEndpoints MessageAPI msg self super messages messages'
         , EnactEndpoints RequestAPI req self super requests requests'
         )
      => UnifiedAPI messages requests remoteMessages remoteRequests
      -> Implementation msg req self super messages' requests'
      -> Narrative self super (ActiveAPI self super messages requests)
enact u@(UnifiedAPI local _) (Implementation mhs rhs) = do
  let API mapi rapi = local
  amapi <- enactEndpoints mapi mhs
  arapi <- enactEndpoints rapi rhs
  let active = ActiveAPI amapi arapi
  return active
