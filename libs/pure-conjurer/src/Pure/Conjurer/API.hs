module Pure.Conjurer.API where

import Pure.Conjurer.Context
import Pure.Conjurer.Interactions
import Pure.Conjurer.Name
import Pure.Conjurer.Previewable
import Pure.Conjurer.Producible
import Pure.Conjurer.Resource

import Data.JSON
import Data.Websocket as WS

import Data.Proxy
import Data.Typeable

data InteractResource resource
instance Identify (InteractResource resource)
instance (Typeable resource) => Request (InteractResource resource) where
  type Req (InteractResource resource) = (Int,(Context resource,Name resource,Action resource))
  type Rsp (InteractResource resource) = Maybe (Reaction resource)
  
interactResource :: Proxy (InteractResource resource)
interactResource = Proxy

data CreateResource resource
instance Identify (CreateResource resource)
instance (Typeable resource) => Request (CreateResource resource) where
  type Req (CreateResource resource) = (Int,(Context resource,Resource resource))
  type Rsp (CreateResource resource) = Maybe (Name resource)

createResource :: Proxy (CreateResource resource)
createResource = Proxy

data ReadResource resource
instance Identify (ReadResource resource)
instance (Typeable resource) => Request (ReadResource resource) where
  type Req (ReadResource resource) = (Int,(Context resource,Name resource))
  type Rsp (ReadResource resource) = Maybe (Resource resource)

readResource :: Proxy (ReadResource resource)
readResource = Proxy

data UpdateResource resource
instance Identify (UpdateResource resource)
instance (Typeable resource) => Request (UpdateResource resource) where
  type Req (UpdateResource resource) = (Int,(Context resource,Name resource,Resource resource))
  type Rsp (UpdateResource resource) = Bool

updateResource :: Proxy (UpdateResource resource)
updateResource = Proxy

data DeleteResource resource
instance Identify (DeleteResource resource)
instance (Typeable resource) => Request (DeleteResource resource) where
  type Req (DeleteResource resource) = (Int,(Context resource,Name resource))
  type Rsp (DeleteResource resource) = Bool

deleteResource :: Proxy (DeleteResource resource)
deleteResource = Proxy

data PreviewResource resource
instance Identify (PreviewResource resource)
instance (Typeable resource) => Request (PreviewResource resource) where
  type Req (PreviewResource resource) = (Int,(Context resource,Resource resource))
  type Rsp (PreviewResource resource) = Maybe (Context resource,Name resource,Preview resource,Product resource,Resource resource)

previewResource :: Proxy (PreviewResource resource)
previewResource = Proxy

data AmendResource resource
instance Identify (AmendResource resource)
instance (Typeable resource) => Request (AmendResource resource) where
  type Req (AmendResource resource) = (Int,(Context resource,Name resource,Amend resource))
  type Rsp (AmendResource resource) = Bool

amendResource :: Proxy (AmendResource resource)
amendResource = Proxy

data PreviewAmendResource resource
instance Identify (PreviewAmendResource resource)
instance (Typeable resource) => Request (PreviewAmendResource resource) where
  type Req (PreviewAmendResource resource) = (Int,(Context resource,Name resource,Amend resource))
  type Rsp (PreviewAmendResource resource) = Maybe (Context resource,Name resource,Preview resource,Product resource,Resource resource)

previewAmendResource :: Proxy (PreviewAmendResource resource)
previewAmendResource = Proxy

data ReadProduct resource
instance Identify (ReadProduct resource)
instance (Typeable resource) => Request (ReadProduct resource) where
  type Req (ReadProduct resource) = (Int,(Context resource,Name resource))
  type Rsp (ReadProduct resource) = Maybe (Product resource)

readProduct :: Proxy (ReadProduct resource)
readProduct = Proxy

data ReadPreview resource
instance Identify (ReadPreview resource)
instance (Typeable resource) => Request (ReadPreview resource) where
  type Req (ReadPreview resource) = (Int,(Context resource,Name resource))
  type Rsp (ReadPreview resource) = Maybe (Preview resource)

readPreview :: Proxy (ReadPreview resource)
readPreview = Proxy

data ReadListing resource
instance Identify (ReadListing resource)
instance (Typeable resource) => Request (ReadListing resource) where
  type Req (ReadListing resource) = (Int,Context resource)
  type Rsp (ReadListing resource) = Maybe [(Name resource,Preview resource)]

readListing :: Proxy (ReadListing resource)
readListing = Proxy

type PublishingAPI resource = 
  '[ CreateResource resource
   , ReadResource resource
   , UpdateResource resource
   , DeleteResource resource
   , PreviewResource resource
   , AmendResource resource
   , PreviewAmendResource resource
   , InteractResource resource
   ]

type ReadingAPI resource =
  '[ ReadProduct resource
   , ReadPreview resource
   , ReadListing resource
   ]

publishingAPI 
  :: forall resource. 
    ( Typeable resource
    , ToJSON (Context resource), FromJSON (Context resource)
    ) => API '[] (PublishingAPI resource)
publishingAPI = api msgs reqs
  where
    msgs = WS.non
    reqs = createResource @resource
       <:> readResource @resource
       <:> updateResource @resource
       <:> deleteResource @resource
       <:> previewResource @resource
       <:> amendResource @resource
       <:> previewAmendResource @resource
       <:> interactResource @resource
       <:> WS.non

readingAPI 
  :: forall resource. 
    ( Typeable resource
    , ToJSON (Context resource), FromJSON (Context resource)
    ) => API '[] (ReadingAPI resource)
readingAPI = api msgs reqs
  where
    msgs = WS.non
    reqs = readProduct @resource
       <:> readPreview @resource
       <:> readListing @resource
       <:> WS.non

