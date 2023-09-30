{-# language BlockArguments, DerivingStrategies, DeriveGeneric, DeriveAnyClass, ConstraintKinds, FlexibleContexts, RankNTypes, ScopedTypeVariables, StandaloneDeriving, AllowAmbiguousTypes, DataKinds, TypeOperators, TypeFamilies, TypeApplications, UndecidableInstances, FlexibleInstances, MultiParamTypeClasses, GADTs, PolyKinds #-}
module Data.Trace 
  ( Trace(..)
  , SomeTrace
  , Tracing
  , Tracer
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

{- |

The Data.Trace module provides a versatile framework for tracing that supports arbitrary JSON-isoformable data. It offers:

- A generic wrapper (`SomeTrace`) to encapsulate traceable data
- Easy-to-use inspection mechanisms via the `Trace` typeclass
- Modular handling of tracing through `tracer`
- Type-safe (up to arbitrary trace data types in the wrapper) and hierarchy-agnostic tracing using `tracing` and the `emit` method from the `Emit` type class.

## Features

1. **Mixed-Domain Tracing**: Different tracing schemes for various application
   domains can coexist without conflict.
  
2. **Flexible Depth**: Allows for tracing at an arbitrary or fixed depth. The
   "depth" corresponds to the contextual layers of the calling or wrapping code.

3. **Custom Handlers**: Extend or modify tracing capabilities by providing
   custom trace handlers, enabling features like data sampling or custom 
   egress management.

## Basic Usage

1. **Define Domains**: Specify the domains you wish to trace. These could either
   be standalone or associated with a specific type of trace data. For example:

   ```haskell
   data Interaction
   data Performance
   ```

2. **Describe Trace Data**: Identify the kinds of tracing data you'll produce.
   You can either define these yourself or use a pre-existing set from a
   library tailored for your application domain. For example:

   ```haskell
   data ClientPerformance = PageRenderStart Txt | FooterRenderStart Txt | PageUnloaded Txt
   data ServerPerformance = Entry Txt | Exit Txt | ...
   -- etc ...
   ```

   The only constraint is that they be JSON-isoformable (ToJSON <=> FromJSON).
   You may delineate the types and hierarchy of the trace data as you see fit,
   but there are approaches that, depending on your domain, may simplify
   analysis. Since you will likely need to pattern-match on your trace data,
   extracted from the `SomeTrace`s, it may benefit you to write a top-level
   ADT to wrap the sub-components of your tracing domain for simpler dispatch.
   The basic guidelines for design of ADTs apply.

3. **Instrument Your Code**: Decide on the hierarchy of your tracing data, if
   you choose to implement one. A hierarchy is optional and may depend on the
   complexity of your application.
    
   If a hierarchy is relevant, decide on whether you need a depth-independent
   or fixed-depth approach. A fixed-depth approach is less modular but may be
   convenient depending on your needs. On the other hand, depth-independent
   tracing is more flexible but may require more work in instrumentation. In
   some cases, traced code will be independent of everything except a consumer
   of a tracing domain `Tracing domain 0`.

   For example, consider the production client-side page performance trace data
   using a fixed-depth approach:

   ```haskell
   type PageTracing n = Tracing ClientPerformance n -- Define a type alias for clarity

   -- Wrap the view in a trace that starts with a `PageRenderStart` and ends with a `PageUnloaded`.
   tracePage :: (PageTracing 1 => View) -> (PageTracing 0 => View)
   tracePage v = 
     tracingWith @ClientPerformance @0 
       (Just (PageRenderStart slug)) 
       (Just (PageUnloaded slug))
       v

   -- Emit a trace event for the client performance at this point in the view.
   pageTrace :: PageTracing 1 => ClientPerformance -> View -> View
   pageTrace t = eager (emit @ClientPerformance @1 t) v

   -- Create a view for a page and trace different components of it.
   page :: PageTracing 0 => Page -> View
   page Page { slug } =  -- Assume `slug` is an identifier for the page
     tracePage do
       Article <||>
         [ pageTrace (HeaderRenderStart slug) do  -- Trace when header starts rendering
            <... render page header ...>
         , pageTrace (ContentRenderStart slug) do  -- Trace when content starts rendering
            <... render page content proper ...>
         , pageTrace (AsideRenderStart slug) do  -- Trace when aside starts rendering
            <... render page aside ...>
         , pageTrace (FooterRenderStart slug) do  -- Trace when footer starts rendering
         ]
   ```

   Tracing in this way can be used to keep track of performance across client
   types when traces are sent back to a server or for informing real-time design
   decisions during development to inform how and when a design should make use
   of asynchrony. If tracing is no longer needed, you may keep the code
   instrumented and replace the handler supplied to `tracer` with a no-op, such
   as `def` or `const (pure ())`.

4. **Store and/or Analyze the Traces**: Decide what to do with the produced
   traces.

   The range of options here is vast, and your choice will likely depend on your
   application's specific needs and the type of insights you hope to gain.

   - **Persistence**: If you want to store the traces for later analysis, you
     could employ a logging tracer that appends the traces to file, or send them
     to an aggregating endpoint.

   - **Real-time Analysis**: If you prefer immediate insights, you can implement
     real-time analytics. This could involve identifying anomalous behavior,
     calculating performance metrics, and/or triggering alerts. This can also be
     a way to reduce storage requirements by only saving traces that you flag as
     significant.

   - **Aggregation and Cross-Domain Analysis**: Sometimes, a single trace might
     not provide much information, but an aggregate view across different
     domains could be enlightening. For instance, analyzing traces from both the
     client and the server could provide a more holistic view of system
     performance and user interaction. You might wish to identify bottlenecks
     in your server pipeline that correlate with lowered user retention. 

   This library is designed to allow easy creation, instrumentation, and 
   inspection, while being agnostic of the specific approach to aggregation and
   analysis. 

-}


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
-- ## Nesting
-- `SomeTrace` shouldn't be nested, as extracting a nested `SomeTrace` is not
-- supported by the `Trace` instance. `fromTrace @SomeTrace` pulls out the
-- associated tracing context (`[Key]`) and the `Time` it was generated.
--
-- TODO: 
--   Switch to a stable version of `Fingerprint`.
--
--   Related discussions: 
--     https://mail.haskell.org/pipermail/haskell-cafe/2014-October/116355.html
--     https://stackoverflow.com/questions/32444236/serialisable-stable-representation-of-a-type
--
--   A potential replacement:
--     https://hackage.haskell.org/package/hashabler-1.2.1
--
data SomeTrace = SomeTrace Fingerprint [Key] Time Value
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

-- | SomeValue is a workaround for the case in which a type Fingerprint changes
-- but you still need to inspect old trace data. With stable fingerprints, this
-- would be unnecessary.
newtype SomeValue = SomeValue Value 
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)
instance Trace SomeValue where
  toTrace = error "Construction of SomeValue traces is disallowed."
  fromTrace (SomeTrace _ ks t v) = Just (ks,t,SomeValue v)

instance Trace SomeTrace where
  toTrace _ _ = id
  fromTrace st@(SomeTrace _ ks t _) = Just (ks,t,st)

-- | Core tracing constraint for associating code with a specific tracing
-- hierarchy and depth.
--
-- ## Tracing Domains
-- The phantom `domain` parameter allows for the isolation of independent
-- tracing hierarchies. This enables multiple, concurrent tracing strategies
-- within a single computational context.
-- - Caution: 
--     Mixing tracing domains can complicate trace interpretation.
--     Make sure your domains are clearly defined.
--
-- ## Domain Context
-- Note that the `domain` here is not related to other systems like `Auth`,
-- where the domain might refer to application data. Instead, the `domain`
-- parameter should reflect either an application or analytics concept. For
-- example, for analyzing system performance and user interaction:
-- > data Performance
-- > data Interaction
-- These can coexist in the same codebase, with each domain having its own
-- trace handlers.
--
-- ## Type Safety
-- This constraint doesn't tie the value being traced to a specific tracing
-- hierarchy, offering flexibility in what can be traced.
--
type Tracing domain n = (Exists (Traces domain n), Producer SomeTrace)

-- | A synonym for `Tracing domain 0`. This is introduced by `tracer` only.
type Tracer domain = Tracing domain 0

-- | Initializes the base `Tracing domain 0` context by specifying how traces
-- are to be managed.
--
-- ## Configuration
-- Use this function to configure the trace management strategy, such as
-- storage, routing, or analysis.
-- - On a client, you might dispatch `SomeTrace` data across the network
--   or simply print to stdout.
-- - On a server, you could route `SomeTrace` data to a file, a network
--   endpoint, an analysis algorithm, or simply print to stdout during
--   development.
--
-- ## Sampling Strategies
-- You may wish to implement custom sampling in your handler based on
-- domain-specific criteria. For example:
-- - Emit all start/end events of a specific type.
-- - Sample other events with uniform probability.
-- - Log every critical trace while also writing all traces to a file.
--
-- ## Base Case
-- Note that this is the only way to establish the base `Tracing domain 0`
-- context, which is essential for using `tracing`.
-- 
-- ## Trace Hierarchy
-- The initialized `Tracing domain 0` context does not contain any tracing
-- hierarchy by default. It simply provides a framework within which a
-- domain-specific tracing hierarchy can be constructed and managed.
tracer :: forall domain x. (SomeTrace -> IO ()) -> (Tracing domain 0 => x) -> x
tracer h x = stream h (with (Traces [] :: Traces domain 0) x)

newtype Traces (domain :: k) (n :: Nat) = Traces [Key]

-- | Retrieve the `[Key]` associated with the current tracing context hierarchy.
--
-- ## Intended Usage
-- Although this function is exported, it's primarily intended for introspective purposes,
-- rather than for operational use in trace construction or management.
--
-- ## Context Management
-- Refer to `tracing` and `tracer` for details on how trace context hierarchies are established
-- and managed.
traces :: forall domain n. Tracing domain n => [Key]
traces = let Traces ts = it :: Traces domain n in ts

-- | Emit a trace within a specified `domain` and at a given depth `n`.
--
-- ## Requirements
-- - Must be invoked within an existing tracing context for the same `domain` and depth.
-- - Emitting at a depth of `0` is a compile-time error to prevent orphan traces.
--
-- ## Establishing Context
-- For establishing a tracing context, see `tracer` and `tracing`.
--
-- ## Basic Usage Example
-- ```
-- tracer @MyDomain (Txt.putStrLn . pretty) do -- Constructs a `Tracing MyDomain 0` context
--   tracing @MyDomain @0 do                   -- Constructs a `Tracing MyDomain 1` context
--     emit @MyDomain @1 MyTraceData           -- Emits trace data from `Tracing MyDomain 1` context
-- ```
--
-- ## Trace Types
-- Any data type may be emitted at any level (other than `0`) as long as it has
-- a `Trace` instance. This is analogous to how `throw` can handle any type with
-- an `Exception` instance.
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

-- | The `tracing` function establishes a new trace context relative to an
-- existing one, incrementing the depth (`n`) by 1.
--
-- ## Overview
-- - Must be called within an existing tracing context.
-- - The base case is provided by the `tracer` function, supplying `Traces domain 0`.
-- - Can optionally emit traces upon entry and exit.
--
-- ## Basic Usage
-- ```
-- tracer @MyDomain (Txt.putStrLn . pretty) do
--   tracing @MyDomain @0 @SomeTraceDataType do
--     -- nested code
-- ```
-- Within this context, you can use `emit @MyDomain @1 SomeTraceData`.
--
-- ## Dynamic Depth
-- To trace at an arbitrary depth `n`, define your traced code like so:
-- ```
-- someTracedCode :: forall domain n. Tracing domain n => IO ()
-- someTracedCode = do
--   -- your code
--   emit @domain @n SomeTraceData
--   -- your code
-- ```
-- And then use `tracing` with `n + 1` as the emit context:
-- ```
-- tracing @domain @n @SomeTraceDataType do
--   emit @domain @(n + 1) SomeTraceData
-- ```
--
-- ## Lifecycle Tracing
-- Using `tracingWith`, you may optionally emit traces when the context is
-- initialized, and/or when it is garbage collected by supplying the optional
-- `begin` and `end` arguments.
--
-- ## Nesting Caution
-- Creating the same trace context (`n + 1`), through `tracing`, in a nested
-- fashion will produce ambiguous traces. Separate them into distinct functions
-- to avoid this.
--
-- ## Trace Finalization
-- Note the final trace is not guaranteed to be emitted timely, as it is emitted
-- when the context is cleaned up and the `Weak` associated with the `Key` 
-- constructed for the new context is finalized. If you need timely final traces,
-- you must annotate the code yourself in the location of your choosing.
--
-- ## Exceptions
-- There are no specified operational semantics with respect to exceptions 
-- within `tracing`; there is no catching and re-throwing to emit a final trace.
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
