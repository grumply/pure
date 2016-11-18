module Nuclear.API.Unification where

import Ef.Base

import Nuclear.API.Interface
import Nuclear.API.Implementation

import Unsafe.Coerce

data UnifiedAPI localMessages localRequests remoteMessages remoteRequests
  = UnifiedAPI
      (API localMessages localRequests)
      (API remoteMessages remoteRequests)
  | forall self super.
    LiveUnifiedAPI
      (API localMessages localRequests)
      (API remoteMessages remoteRequests)
      (ActiveAPI self super localMessages localRequests)

unify :: API localMessages localRequests -> API remoteMessages remoteRequests -> UnifiedAPI localMessages localRequests remoteMessages remoteRequests
unify = UnifiedAPI

-- Build a unified API from two sets of endpoints.
unifiedAPI :: ( Ma (Traits traits) (Messages self)
              , '[State (UnifiedAPI localMessages localRequests remoteMessages remoteRequests)] <: self
              , '[State (UnifiedAPI localMessages localRequests remoteMessages remoteRequests)] <. traits
              , Monad super
              , MonadIO super
              )
            => UnifiedAPI localMessages localRequests remoteMessages remoteRequests
            -> Implementation msg req self super localMessages localRequests
            -> Trait (State (UnifiedAPI localMessages localRequests remoteMessages remoteRequests)) traits super
unifiedAPI unified _ = state unified -- the Bool flag controls access to this undefined, so should be safe

enact :: forall msg req messages requests messages' requests' remoteMessages remoteRequests self super.
         ('[State (UnifiedAPI messages requests remoteMessages remoteRequests)] <: self
         , Monad super, MonadIO super
         , EnactEndpoints MessageAPI msg self super messages messages'
         , EnactEndpoints RequestAPI req self super requests requests'
         )
      => UnifiedAPI messages requests remoteMessages remoteRequests
      -> Implementation msg req self super messages' requests'
      -> Narrative self super (ActiveAPI self super messages requests)
enact u (Implementation mhs rhs) = do
  let API mapi rapi =
        case u of
          UnifiedAPI localAPI _ -> localAPI
          LiveUnifiedAPI localAPI _ _ -> localAPI
  uapi <- get
  case uapi :: UnifiedAPI messages requests remoteMessages remoteRequests of
    LiveUnifiedAPI localAPI remoteAPI active ->
      return $ unsafeCoerce active
    UnifiedAPI localAPI remoteAPI -> do
      amapi <- enactEndpoints (mapi :: MessageAPI messages) mhs
      arapi <- enactEndpoints (rapi :: RequestAPI requests) rhs
      let active = ActiveAPI amapi arapi
      put (LiveUnifiedAPI localAPI remoteAPI (unsafeCoerce active))
      return active
