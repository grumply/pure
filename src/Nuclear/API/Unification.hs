module Nuclear.API.Unification where

import Ef.Base

import Nuclear.API.Interface
import Nuclear.API.Implementation

import Unsafe.Coerce

-- data Unified msg req self super messages requests messages' requests'
--   where
--     Unify
--       :: FullAPI messages requests
--       -> Implementation msg req self super messages' requests'
--       -> Unified msg req self super messages requests messages' requests'

-- unify :: FullAPI messages requests -> Implementation msg req self super messages' requests' -> Unified msg req self super messages requests messages' requests'
-- unify = Unify

-- enact :: ( Monad super, MonadIO super
--          , EnactEndpoints MessageAPI msg self super messages messages'
--          , EnactEndpoints RequestAPI req self super requests requests'
--          )
--       => Unified msg req self super messages requests messages' requests'
--       -> Narrative self super (ActiveAPI self super messages requests)
-- enact (Unify local (Impl mhs rhs)) = do
--   let API mapi rapi = local
--   amapi <- enactEndpoints mapi mhs
--   arapi <- enactEndpoints rapi rhs
--   let active = ActiveAPI amapi arapi
--   return active
