{-# language CPP, DuplicateRecordFields #-}
module Pure.Auth (module Export) where

-- Note that the API is not exposed by default, as it is generally
-- abstracted over with Pure.Auth.GHC on the server and Pure.Auth.Access
-- on the client. However, it is available for import separately.

import Pure.Auth.Data as Export
import Pure.Auth.Access as Export

#ifndef __GHCJS__
import Pure.Auth.GHC as Export
#endif
