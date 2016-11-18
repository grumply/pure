{-# language UndecidableInstances #-}
module Nuclear.API.Interface where

import Data.Proxy

import Unsafe.Coerce

import Nuclear.Message
import Nuclear.Request

{- Example API

data Manager_ a
manager :: Proxy Manager_
manager = Proxy

data Server_ a
server :: Proxy Server_
server = Proxy

data Put_ a
put :: Proxy Put_
put = Proxy

data Get_ a
get :: Proxy Get_
get = Proxy

data Post_ a
post :: Proxy Post_
post = Proxy

int :: Proxy Int
int = Proxy

double :: Proxy Double
double = Proxy

type MAPI
  = '[ Manager_ (Post_ Int)
     , Manager_ (Put_ Int)
     , Manager_ (Put_ Double)
     , Server_ (Post_ Double)
     , Server_ (Put_ Double)
     ]

type NAPI
  = '[ Manager_ (Get_ Int)
     , Manager_ (Get_ Double)
     , Server_ (Get_ Double)
     ]

theAPI :: API MAPI NAPI
theAPI = api mesages nuclear
  where

    messages =
           manager |>
             post <&> put <| int <++>
             only put <| double
      <||> server |>
            post <&> put <| double

    nuclear =
           manager |>
             only get <| int <++>
             only get <| double
      <||> server |>
             only get  <| double

-}

class ToMessageAPI messages where
  makeMessageAPI :: PList messages -> MessageAPI messages

instance ToMessageAPI '[] where
  makeMessageAPI _ = MessageNull

instance (Message x, ToMessageAPI xs) => ToMessageAPI (x ': xs) where
  makeMessageAPI (PCons p ps) = MessageCons p (makeMessageAPI ps)

data MessageAPI messages
  where
    MessageNull
      :: MessageAPI '[]

    MessageCons
      :: Message message
      => Proxy message
      -> MessageAPI messages
      -> MessageAPI (message ': messages)

-- deletes the /first/ occurence of message in messages; failure to witness the message in messages is a type error.
class (Remove message messages ~ messages') => DeleteMessageEndpoint message messages messages' where
  deleteMessageEndpoint :: Proxy message -> MessageAPI messages -> MessageAPI messages'

instance (Remove message (message ': messages) ~ messages) => DeleteMessageEndpoint message (message ': messages) messages where
  deleteMessageEndpoint _ (MessageCons _ mapi) = mapi

instance ( Remove message messages ~ messages'
         , DeleteMessageEndpoint message messages messages'
         , Remove message (x ': messages) ~ messages''
         , messages'' ~ (x ': messages')
         ) => DeleteMessageEndpoint message (x ': messages) messages'' where
  deleteMessageEndpoint p (MessageCons mh mapi) =
    MessageCons mh (deleteMessageEndpoint p mapi)

class ToRequestAPI requests where
  makeRequestAPI :: PList requests -> RequestAPI requests

instance ToRequestAPI '[] where
  makeRequestAPI _ = RequestNull

instance (Request x, ToRequestAPI xs) => ToRequestAPI (x ': xs) where
  makeRequestAPI (PCons p ps) = RequestCons p (makeRequestAPI ps)

data RequestAPI requests
  where
    RequestNull
      :: RequestAPI '[]

    RequestCons
      :: Request request
      => Proxy request
      -> RequestAPI requests
      -> RequestAPI (request ': requests)

-- deletes the /first/ occurence of request in requests; failure to witness the request in requests is a type error.
class (Remove request requests ~ requests') => DeleteRequestEndpoint request requests requests' where
  deleteRequestEndpoint :: Proxy request -> RequestAPI requests -> RequestAPI requests'

instance (Remove request (request ': requests) ~ requests) => DeleteRequestEndpoint request (request ': requests) requests where
  deleteRequestEndpoint _ (RequestCons _ napi) = napi

instance ( -- Remove request requests ~ requests' -- constraint not necessary, but descriptive
           DeleteRequestEndpoint request requests requests'
         , Remove request (x ': requests) ~ requests''
         , requests'' ~ (x ': requests')
         ) => DeleteRequestEndpoint request (x ': requests) requests'' where
  deleteRequestEndpoint p (RequestCons mh napi) =
    RequestCons mh (deleteRequestEndpoint p napi)

data API messages requests
  = API (MessageAPI messages) (RequestAPI requests)

api :: (ToMessageAPI messages, ToRequestAPI requests)
    => PList messages -> PList requests -> API messages requests
api ms rs = API (makeMessageAPI ms) (makeRequestAPI rs)

data PList elems
  where

    PNull
      :: PList '[]

    PCons
      :: Proxy elem
      -> PList elems
      -> PList (elem ': elems)

type family (f :: k -> k') |$| (xs :: [k]) :: [k'] where
  f |$| '[] = '[]
  f |$| (x ': xs) = (f x) ': (f |$| xs)

type family (fs :: [k -> k']) |&| (x :: k) :: [k'] where
  '[] |&| x = '[]
  (f ': fs) |&| x = (f x) ':  fs |&| x

infixr 2 |>
class Ap (f :: k -> k') (ys :: [k]) where
  (|>) :: ((f |$| ys) ~ ys') => Proxy f -> PList ys -> PList ys'
instance Ap f '[] where
  (|>) _ _ = PNull
instance (Ap f ys) => Ap f (y ': ys) where
  (|>) pf (PCons ph hs') =
      PCons (pf <<@>> ph) (pf |> hs')

infixr 4 <|
class On (fs :: [k -> k']) (x :: k) where
  (<|) :: ((fs |&| x) ~ fs') => PList fs -> Proxy x -> PList fs'
instance On '[] x where
  (<|) _ _ = PNull
instance On xs k => On (x ': xs) k where
  (<|) (PCons pf hs) pk =
    PCons (pf <<@>> pk) (hs <| pk)

infixr 6 <<@>>
(<<@>>) :: forall f a b. (b ~ f a) => Proxy (f :: k -> k') -> Proxy (a :: k) -> Proxy (b :: k')
(<<@>>) _ _ = Proxy :: Proxy (f a :: k')

empty :: PList '[]
empty = PNull

only :: Proxy elem -> PList '[elem]
only x = x <:> empty

infixr 5 <&>
(<&>) :: Proxy elem -> Proxy elem' -> PList '[elem, elem']
(<&>) xs y = xs <:> only y

infixr 5 <:>
(<:>) :: Proxy (elem :: k) -> PList (elems :: [k]) -> PList ((elem ': elems) :: [k])
(<:>) = PCons

infixr 3 <++>
class (Append elems elems' ~ elems'') => And (elems :: [k]) (elems' :: [k]) (elems'' :: [k]) where
  (<++>) :: PList elems -> PList elems' -> PList elems''

instance (Append '[] xs ~ xs) => And '[] xs xs where
  (<++>) l (PCons x xs) = PCons x (l <++> xs)

instance (Remove x ys ~ ys, And xs ys zs', zs ~ (x ': Append xs ys), zs ~ (z ': zs')) => And (x ': xs) ys zs where
  (<++>) (PCons (x :: Proxy x) xs) ys = x <:> (xs <++> ys)

infixr 1 <||>
(<||>) :: And xs ys zs => PList xs -> PList ys -> PList zs
(<||>) = (<++>)

type family Append xs ys
  where
    Append '[] ys = ys
    Append (x ': xs) ys = x ': (Append xs ys)

type family Remove a xs
  where

    Remove a '[] = '[]

    Remove a (a ': xs) = Remove a xs

    Remove a (x ': xs) = x ': Remove a xs

type family Insert a xs
  where

    Insert a '[] = '[a]

    Insert a (a ': xs) = a ': xs

    Insert a (x ': xs) = x ': Insert a xs

type family Elem a as :: Bool
  where

    Elem a '[] = 'False

    Elem a (a ': xs) = 'True

    Elem a (x ': as) = Elem a as
