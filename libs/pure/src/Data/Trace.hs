{-# language BlockArguments, DerivingStrategies, DeriveGeneric, DeriveAnyClass, ConstraintKinds, FlexibleContexts, RankNTypes, ScopedTypeVariables, StandaloneDeriving, AllowAmbiguousTypes, DataKinds, TypeOperators, TypeFamilies, TypeApplications, UndecidableInstances, FlexibleInstances, MultiParamTypeClasses, GADTs, PolyKinds #-}
module Data.Trace 
  ( Trace(..)
  , SomeTrace
  , Tracing
  , tracer
  , tracingWith
  , tracing
  , traces 
  , Emits(..)
  ) where

import Data.Foldable (for_)
import Data.JSON (ToJSON(..),FromJSON(..),Result(..),Value,fromJSON)
import Data.Key (Key,keyIO,timestamp,keyToFingerprint,fingerprintToKey,unsafeFromKey,toKey)
import Data.Marker (Marker)
import Data.Time (Time,time)
import Data.Type.Equality
import Data.View (Exists(..),with,Producer,stream,yield)
import Data.IORef (newIORef,mkWeakIORef,readIORef,IORef)
import GHC.Fingerprint (Fingerprint(..))
import GHC.Generics (Generic)
import GHC.Types (Constraint)
import GHC.Weak (deRefWeak,Weak)
import GHC.TypeLits
import System.IO.Unsafe (unsafePerformIO)
import Data.Typeable (Typeable,typeOf,typeRepFingerprint)
import GHC.TypeLits

instance ToJSON Fingerprint where
  toJSON = toJSON . fingerprintToKey
instance FromJSON Fingerprint where
  parseJSON = fmap keyToFingerprint . parseJSON

-- | A class for constructing and inspecting `SomeTrace`, the generic trace.
-- This class is much like `Exception` from `Control.Exception`. To use it,
-- simply declare `instance Trace MyDataType` for `MyDateType` that has both
-- `ToJSON` and `FromJSON` instances.
class (Typeable t, ToJSON t, FromJSON t) => Trace t where
  toTrace :: [Key] -> Time -> t -> SomeTrace
  fromTrace :: SomeTrace -> Maybe ([Key],Time,t)

  toTrace ks t v = SomeTrace (typeRepFingerprint (typeOf v)) ks t (toJSON v)
  fromTrace (SomeTrace fp ks t v)
    | fp == typeRepFingerprint (typeOf (undefined :: t))
    , Success x <- fromJSON v 
    = Just (ks,t,x)

    | otherwise 
    = Nothing

instance Trace Value 

-- | A generic wrapper for a trace that is serializable and aggregable.
-- This is much like the `SomeException` type in `Control.Exception`, with
-- the `Trace` class serving as an equivalent to the `Exception` class.
--
-- This type is trivially an instance of `Trace`. 
--
-- Note: `SomeTrace` shouldn't be nested, as extracting a `SomeTrace` from
-- within a `SomeTrace` is not supported by the `Trace` class - `fromTrace` for
-- `SomeTrace` simply pulls out the associated tracing context (`[Key]`) and the
-- `Time` it was generated.
--
data SomeTrace = SomeTrace Fingerprint [Key] Time Value
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

instance Trace SomeTrace where
  toTrace _ _ = id
  fromTrace st@(SomeTrace _ ks t _) = Just (ks,t,st)

-- | The core tracing type is a constraint that can associate the constrained
-- code with a tracing hierarchy at a given depth. This, critically, does not
-- associate the type of value being traced with the tracing hierarchy.
-- 
-- The phantom `domain`, importantly, allows disassociating two independent 
-- trace networks. This allows a plurality of independent trace strategies to
-- exist in a single computational context. In general, though, most code will
-- exist in a single tracing domain. It is, however, necessary to allow such a
-- modularization. 
--
-- I do warn: intermingling tracing domains can make it difficult to establish a
-- clear understanding of the trace networks. Be sure that your domains are
-- well-founded for a clean and clear understanding; the domain should clearly 
-- convey what is being traced.
--
-- Note: The `domain` is not associated with the `domain` from that of, say, the
-- `Auth` system, where the domain corresponds to some data or application
-- context. Here, the `domain` is associated with either an application concept
-- or an analytics concept. Perhaps you wish to use tracing to support analytics
-- of both system performance and user interaction:
--
-- > data Performance
-- > data Interaction
--
-- You may then use intermingled code for both tracing domains with the
-- guarantee that the traces end up with the correct handlers - the
-- `Performance` domain will being handled by a `tracer` that was instantiated
-- for the `Performance` domain, and likewise for `Interaction`. You would,
-- likely, want to specialize an `emit` for each, perhaps specializing the
-- traces to a particular type (if you want monomorphic trace data). As a 
-- simplification, you may even use the type of trace data as the domain:
--
-- > data Performance = Entry Txt | Exit Txt
-- >
-- > perf :: forall n. Tracing Performance n => Performance -> IO ()
-- > perf = emit @Performance @n
-- >
-- > entry, exit :: forall n. Tracing Performance n => Txt -> IO ()
-- > entry = perf @n . Entry
-- > exit  = perf @n . Exit
--
type Tracing domain n = (Exists (Traces domain n), Producer SomeTrace)

-- | Given a means of handling `SomeTrace`, witness the `Tracing 0` context.
-- This is where you configure how traces should flow out of your traced code -
-- how they are managed, how they are stored, where they go, etc....
--
-- On a client, this would often dispatch `SomeTrace` across the network.
-- On a server, this could, say, dispatch `SomeTrace` to a file, across the
-- network, straight to an analysis algorithm, or to stdout.
--
-- You may wish to do sampling here, based on a criteria that is desirable for
-- your domain - perhaps you want to emit all start/end events of a certain 
-- type, and sample all other events with equal probability.
--
-- Perhaps you wish to log every critical trace, and write all traces to a log.
--
-- Note: This is the only way to establish the base `Tracing domain 0` context,
-- which is a necessary and sufficient condition to call `tracing`.
--
-- Note: The `Tracing domain 0` tracing hierarchy is empty. `tracer` does not
-- initialize a root tracing context, it simply establishes, as a base-case,
-- the context in which a tracing domain may be constructed and managed.
tracer :: forall domain x. (SomeTrace -> IO ()) -> (Tracing domain 0 => x) -> x
tracer h x = stream h (with (Traces [] :: Traces domain 0) x)

newtype Traces (domain :: k) (n :: Nat) = Traces [Key]

-- | `traces` retrieves the `[Key]` associated with a trace context hierarchy.
-- This method is exported, but not for operational purposes; it is intended for
-- introspection and not usually necessary for constructing traces or working in
-- a tracing context. See `tracing` and `tracer` to understand how this is 
-- constructed and how it is managed.
traces :: forall domain n. Tracing domain n => [Key]
traces = let Traces ts = it :: Traces domain n in ts

-- | Emit a trace within the given tracing `domain` at the given depth `n`.
-- This must be called within an established tracing context of the given domain
-- and depth. To establish a tracing context, see `tracer` and `tracing`. Trying
-- to `emit` at a depth of `0` in any domain, is a compile-time error to prevent
-- the creation of orphan traces.
--
-- Raw usage without domain-oriented specialization might look like this:
--
-- > tracer @MyDomain (Txt.putStrLn . pretty) do -- constructs `Tracing 0` context
-- >   tracing @MyDomain @0 do                   -- constructs `Tracing 1` context
-- >     emit @MyDomain @1 MyTraceData           -- emits from `Tracing 1` context
--
-- Note that any tracing data may be emitted in any domain at any level (other
-- than 0), as long as there is a `Trace` instance for that type, similarly to
-- how `throw` can throw any type that has an `Exception` instance.
class Emits domain n where
  emit :: forall t. Trace t => t -> IO ()

instance {-# OVERLAPS #-} 
  TypeError (Text "Forbidden trace depth of 0 for domain " :<>: ShowType domain) 
  => Emits domain 0 
  where emit = undefined

instance {-# OVERLAPPABLE #-} Tracing domain n => Emits domain n where
  emit t = do
    now <- time
    yield (toTrace (traces @domain @n) now t)

-- | `tracing` produces a new trace context relative to a specified parent
-- context, `n`. The new context will be `n + 1`. Thus, `tracing` must be
-- called within an existing tracing context. At the base case, `tracer`
-- supplies the empty trace context: `Traces domain 0`. The empty trace
-- context, it is important to note, has no associated `Key`s - that is,
-- `traces @domain @0 == []`. It is otherwise impossible to construct a 
-- `Traces domain 0` and it is required that one exists to use `tracing`.
--
-- If the supplied context is concrete, say `2`, the derived uses may be
-- concrete. For instance,
--
-- > tracer @MyDomain (Txt.putStrLn . pretty) do
-- >   tracing @MyDomain @0 @SomeTraceDataType Nothing Nothing do
-- >     <..>
--
-- The above call to `tracing` establishes a context in which you may call 
--
-- > emit @MyDomain @1 SomeTraceData
--
-- If you have code that needs tracing at an arbitrary depth, you may construct
-- it like so:
--
-- > someTracedCode :: forall domain n. Tracing domain n => IO ()
-- > someTracedCode = do
-- >   <..>
-- >   emit @domain @n SomeTraceData
-- >   <..>
--
-- If you wish to call `tracing` in such a context, you must use `n + 1` as the
-- emit context, or pass the `n + 1` to a context that requires some `m :: Nat`:
--
-- > someTracedCode :: forall domain n. Tracing domain n => IO ()
-- > someTracedCode = do
-- >   <..>
-- >   tracing @domain @n @SomeTraceDataType Nothing Nothing do
-- >     emit @domain @(n + 1) SomeTraceData
-- >     otherTracedCode @domain @(n + 1)
-- >   <..>
-- > 
-- > otherTracedCode :: forall domain n. Tracing domain n => IO ()
-- > otherTracedCode = do
-- >   emit @domain @n SomeTraceData
--
-- You may nest these as necessary, and just append `+ 1`, as necessary:
--
-- > someTracedCode :: forall domain n. Tracing domain n => IO ()
-- > someTracedCode = do
-- >   <..>
-- >   tracing @domain @n @SomeTraceDataType def def do
-- >     tracing @domain @(n + 1) @SomeTraceDataType def def do
-- >       emit @domain @((n + 1) + 1) SomeTraceData
-- >   <..>
--
-- Note that you may `emit` any value that has an associated `Trace` instance
-- at any level. 
--
-- You may optionally emit traces when the context is constructed, and when it
-- is garbage collected by supplying `Just SomeTraceData` as the first, and
-- second arguments, respectively. 
--
-- Note the final trace is not guaranteed to be emitted timely, as it is emitted
-- when the context is cleaned up and the `Weak` associated with the `Key` 
-- constructed for the new context is finalized. If you need timely final traces,
-- you must annotate the code yourself in the location of your choosing.
-- 
-- Note: There are no specified operational semantics with respect to exceptions 
-- within `tracing`; there is no catching and re-throwing to emit the final 
-- trace.
--
tracingWith :: forall domain n begin end x. (Trace begin, Trace end, Tracing domain n) => Maybe begin -> Maybe end -> (Tracing domain (n + 1) => x) -> x
tracingWith minitial mfinal x = unsafeGetKey `seq` with (Traces (unsafeGetKey : traces @domain @n) :: Traces domain (n + 1)) x
  where

    unsafeGetKey :: Key
    unsafeGetKey = unsafePerformIO do
      mior <- deRefWeak trace
      case mior of
        Nothing -> error "Invariant broken: tracing no longer available."
        Just ior -> readIORef ior

    {-# NOINLINE trace #-}
    trace = unsafePerformIO do
      k <- keyIO
      for_ minitial (yield . toTrace (k : traces @domain @n) (timestamp k))
      ior <- newIORef k
      mkWeakIORef ior do
        now <- time
        for_ mfinal (yield . toTrace (k : traces @domain @n) now)

-- | A synonym for `tracingWith @domain @n Nothing Nothing`.
tracing :: forall domain n x. Tracing domain n => (Tracing domain (n + 1) => x) -> x
tracing = tracingWith @domain @n (Nothing @SomeTrace) (Nothing @SomeTrace)
