{-# language BlockArguments, DerivingStrategies, DeriveGeneric, DeriveAnyClass, ConstraintKinds, FlexibleContexts, RankNTypes, ScopedTypeVariables, StandaloneDeriving, AllowAmbiguousTypes, DataKinds, TypeOperators, TypeFamilies, TypeApplications, UndecidableInstances, FlexibleInstances, MultiParamTypeClasses, GADTs, PolyKinds, ExplicitNamespaces, ViewPatterns #-}
module Data.Trace 
  ( Trace(..)
  , SomeTrace
  , Tracing
  , Tracer
  , Key
  , keyIO
  , tracer
  , tracerWith
  , tracing
  , tracingWith
  , tracingView
  , traces 
  , emit
  , Flow
  , Flows
  , flow
  , flows
  , mapFlow
  , mapFlows
  , buildFlow
  , buildFlows
  , mergeFlows
  ) where

import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.IORef (newIORef,mkWeakIORef,readIORef,IORef)
import Data.JSON (ToJSON(..),FromJSON(..),Result(..),Value,fromJSON)
import Data.Key (Key,keyIO,timestamp,keyToFingerprint,fingerprintToKey,unsafeFromKey,toKey)
import Data.List as List (groupBy,sortBy,length)
import Data.Marker (Marker)
import Data.Maybe (mapMaybe,fromJust,listToMaybe)
import Data.Ord (compare)
import Data.Time (Time,time)
import Data.Tree (Tree(..),Forest)
import Data.View (Exists(..),with,Producer,stream,yield,State,state,modify,View,lazy)
import GHC.Fingerprint (Fingerprint(..))
import GHC.Generics (Generic)
import GHC.Types (Constraint)
import GHC.Weak (deRefWeak,Weak)
import GHC.TypeLits (TypeError,ErrorMessage(..),Nat,type (+))
import System.IO.Unsafe (unsafePerformIO)
import Data.Typeable (Typeable,typeOf,typeRepFingerprint)

{- |

Data.Trace is a library for hierarchical tracing of ToJSON + FromJSON data.

- A generic wrapper (`SomeTrace`) to encapsulate trace data 
- Easy-to-use inspection mechanisms via the `Trace` type class
- Simple instrumentation via the `Tracing` constraint
- Modular handling of tracing through `tracer`
- Type-safe and hierarchy-agnostic tracing using `tracing` and `emit` 

tl;dr: Create your trace data types and add `Trace` instances. Instrument your
code with a `Tracing` constraint. Satisfy the `Tracing` constraint with `tracer`
as a base-case and `tracing` otherwise. Use `emit` to produce traces.

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
   data ClientPerformance 
     = PageRenderStart Txt 
     | HeaderRenderStart Txt
     | ContentRenderStart Txt
     | AsideRenderStart Txt
     | FooterRenderStart Txt 
     | ...
   data ServerPerformance 
     = Entry Txt 
     | Exit Txt 
     | ...
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
   -- Define a type alias for clarity
   type PageTracing n = Tracing ClientPerformance n 

   -- Wrap the view in a trace that starts with a `PageRenderStart` and ends
   -- with a `PageUnloaded`.
   tracePage :: (PageTracing 1 => View) -> (PageTracing 0 => View)
   tracePage = tracing @ClientPerformance @0

   -- Emit a trace event for the client performance at this point in the view.
   pageTrace :: PageTracing 1 => ClientPerformance -> View -> View
   pageTrace t = eager (emit @ClientPerformance @1 t) v

   -- Create a view for a page and trace different components of it.
   page :: PageTracing 0 => Page -> View
   page Page { slug } =  -- Assume `slug` is an identifier for the page
     tracePage do
       lazy (pageTrace (PageRenderStart slug)) do
         Article <||>
           [ pageTrace (HeaderRenderStart slug) do -- Trace header render start
               <... render page header ...>
           , pageTrace (ContentRenderStart slug) do -- Trace content render start 
               <... render page content proper ...>
           , pageTrace (AsideRenderStart slug) do -- Trace aside render start
               <... render page aside ...>
           , pageTrace (FooterRenderStart slug) do -- Trace footer render start
               <... render page footer ...>
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
  fromTraceUnsafe :: SomeTrace -> Maybe ([Key],Time,t)

  toTrace ks t v = SomeTrace (typeRepFingerprint (typeOf v)) ks t (toJSON v)
  fromTrace (SomeTrace fp ks t v)
    | fp == typeRepFingerprint (typeOf (undefined :: t))
    , Success x <- fromJSON v 
    = Just (ks,t,x)

    | otherwise 
    = Nothing
  fromTraceUnsafe (SomeTrace fp ks t v)
    | Success x <- fromJSON v 
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

instance Trace SomeTrace where
  toTrace _ _ = id
  fromTrace st@(SomeTrace _ ks t _) = Just (ks,t,st)

traceKeys :: SomeTrace -> [Key]
traceKeys (SomeTrace _ ks _ _) = ks

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
-- This is a synonym for `tracerWith [root]` where `root` is dynamically
-- generated once for a given `tracer`.
{-# INLINE tracer #-}
tracer :: forall domain x. (SomeTrace -> IO ()) -> (Tracing domain 0 => x) -> x
tracer h x = stream h (with (Traces [root] :: Traces domain 0) x)
  where
    {-# NOINLINE root #-}
    root :: Key
    root = unsafePerformIO keyIO

-- | Initial the base `Tracing domain 0` context by specifying the root keys
-- and how traces are to be managed. 
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
{-# INLINE tracerWith #-}
tracerWith :: forall domain x. [Key] -> (SomeTrace -> IO ()) -> (Tracing domain 0 => x) -> x
tracerWith [] _ _ = error "Invalid root trace depth: 0"
tracerWith root h x = stream h (with (Traces root :: Traces domain 0) x)

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
-- Any data type may be emitted at any level as long as it has a `Trace`
-- instance. This is analogous to how `throw` can handle any type with
-- an `Exception` instance.
emit :: forall domain n t. (Tracing domain n, Trace t) => t -> IO ()
emit t = do
  now <- time
  yield (toTrace (traces @domain @n) now t)

{-
-- Unfortunately, this approach does not work in depth-independent helpers since
-- `n` cannot be inferred. It works well for depth-dependent contexts, though.
-- I'll have to figure out another way to guarantee that all traces are at 
-- `depth > 0`. I have considered having tracer introduce a Key, but that ties
-- all traced contexts to a tracer in a way that I'm not sure is desirable.

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
-}

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
-- ## Nesting Caution
-- Creating the same trace context (`n + 1`), through `tracing`, in a nested
-- fashion will produce ambiguous traces. Separate them into distinct functions
-- to avoid this.
--
-- ## Stable Keys
-- The way `tracing` works may result in unexpected key behavior. For example:
-- ```
-- demo :: forall d n. Tracing d n => IO () -> IO ()
-- demo f = do
--   tracing @d @n do
--     emit @d @(n + 1) SomeStartTrace 
--     f
--     emit @d @(n + 1) SomeEndTrace
--
-- main = do
--   tracer @() (putStrLn . fromTxt . pretty) do
--     demo @() @0 (print "1")
--     demo @() @0 (print "2")
-- ```
-- You would likely expect the above code to produce 3 unique keys: 
--   - one in the root `tracer`
--   - one in the first call to `demo`
--   - one in the second call to `demo`
-- Instead, only produce 2 unique keys: 
--   - one in the root tracer
--   - one in the first call to `demo`
-- The key produced in the first call to demo is then reused. If you
-- mark `demo` as INLINE, you will get 3 unique keys, as you might
-- expect.
--
-- Now consider this implementation of `test`:
-- ```
-- test n = do
--   tracer @() (putStrLn . fromTxt . pretty) do
--     replicateM n (demo @() @0 (pure ()))
-- ```
-- In this case, no matter what `n` you pass to `test` or what inlining you
-- mark `demo` with, you will only get 2 unique keys from `test`.
-- 
-- ### Workarounds
-- 1. Use `tracingWith` to manually supply a unique `Key` generated with 
--    `key` or `keyIO`. Exercise caution to avoid key re-use.
-- 2. If dealing with a `View` hierarchy, `tracingView` ensures a unique `Key`
--    for each call.
--
-- ### Why Care?
-- Tracing is often done with respect to the code, and not the operational
-- semantics, so stable keys can be a powerful tool to aggregate associated
-- traces in a way that is often complex or cumbersome to do manually. 
--
{-# INLINE tracing #-}
tracing :: forall domain n x. Tracing domain n => (Tracing domain (n + 1) => x) -> x
tracing = with (Traces (traces @domain @n ++ [k]) :: Traces domain (n + 1)) 
  where
    {-# NOINLINE k #-}
    k :: Key
    k = unsafePerformIO keyIO

-- | A specialization of `tracing` that allows for supplying a custom `Key`.
-- See the comments in [Stable Keys] for `tracingWith` to understand why this
-- might be necessary.
{-# INLINE tracingWith #-}
tracingWith :: forall domain n x. Tracing domain n => Key -> (Tracing domain (n + 1) => x) -> x
tracingWith k = with (Traces (traces @domain @n ++ [k]) :: Traces domain (n + 1)) 

-- | A specialization of `tracingView` that guarantees a new trace hierarchy on
-- each call. See the comments in [Stable Keys] for `tracingWith` to understand
-- why this might be necessary.
{-# INLINE tracingView #-}
tracingView :: forall domain n. Tracing domain n => (Tracing domain (n + 1) => View) -> View
tracingView v = 
  lazy keyIO do
    with (Traces (traces @domain @n ++ [it]) :: Traces domain (n + 1)) do
      v

type Flow a = Tree (Key,[(Time,a)])
type Flows a = Forest (Key,[(Time,a)])

-- | Contruct an empty `Flow a` from a `[Key]`.
--
-- This is purely a convenience to construct a `Flow` when the only data
-- available is a `[Key]`, which can occur at some tracing roots.
keyFlow :: [Key] -> Flow a
keyFlow = go
  where
    go [] = error "invariant broken: SomeTrace has depth 0."
    go [k] = Node (k,[]) []
    go (k:ks) = Node (k,[]) [go ks]

-- | Construct a `Flow a` from a `[Key]`, a `Time` and an `a`.
--
-- This is used as a base case for some flow merges.
toFlow :: [Key] -> Time -> a -> Flow a
toFlow ks0 at a = go ks0
  where
    go [] = error "invariant broken: SomeTrace has depth 0."
    go [k] = Node (k,[(at,a)]) []
    go (k:ks) = Node (k,[]) [go ks]

-- | Integrate a `SomeTrace` into a `Flow a`. 
addToFlow :: Trace a => SomeTrace -> Flow a -> Flow a
addToFlow (SomeTrace fp (kl:ks) t v) f@(Node (kr,ts) cs) =
  let k = min kl kr in head (addToFlows (SomeTrace fp (k : ks) t v) [Node (k,ts) cs])
addToFlow _ _ = error "invariant broken: SomeTrace has depth 0."

-- | Merge a `Flows a` into a `Flow a` by taking the minimum root `Key` and 
-- merging the traces and children as expected.
mergeFlows :: Flows a -> Flow a
mergeFlows fs =
  Node (minimum ks,merge (flip compare `on` fst) ts)
    (merge (flip compare `on` (fst . rootLabel)) cs)
  where
    ks = fmap (\(Node (k,_) _) -> k) fs
    ts = fmap (\(Node (_,ts) _) -> ts) fs
    cs = fmap (\(Node _ cs) -> cs) fs

    -- A helper to merge a list of sorted lists. Should be O(kn log k) for k lists
    -- of average length n. In theory, this should be slightly better than the 
    -- concatenate-and-sort impelementation when k^k < n.
    merge :: forall a. (a -> a -> Ordering) -> [[a]] -> [a]
    merge ord xss
      | ks ** ks < ns = mergesort xss
      | otherwise     = simplesort xss
      where
        ks, ns :: Double
        ks = fromIntegral (List.length xss)
        ns = fromIntegral (sum (fmap List.length xss))

        simplesort = sortBy ord . concat 

        mergesort []   = []
        mergesort [xs] = xs
        mergesort xss  = mergesort (work xss)
          where
            work :: [[a]] -> [[a]]
            work []          = []
            work [xs]        = [xs]
            work (xs:ys:xss) = work (go xs ys : work xss)
              where
                go :: [a] -> [a] -> [a]
                go [] ys = ys
                go xs [] = xs
                go (x:xs) (y:ys)
                  | LT <- ord x y = x : go xs (y:ys)
                  | otherwise     = y : go (x:xs) ys

-- | Integrate a `SomeTrace` into a `Flows a` via `fromTrace`.
addToFlows :: forall a. Trace a => SomeTrace -> Flows a -> Flows a
addToFlows (fromTrace -> Just (ks,at,t)) = go ks
  where
    go :: [Key] -> Flows a -> Flows a
    go ks [] = [toFlow ks at t]
    go (k:ks) fs@(f@(Node (key,ts) children) : rest)
      | k > key            = toFlow (k:ks) at t : fs
      | k == key, [] <- ks = Node (key,addTrace ts) children : rest
      | k == key           = Node (key,ts) (go ks children) : rest
      | otherwise          = f : go (k:ks) rest

    addTrace :: [(Time,a)] -> [(Time,a)]
    addTrace [] = [(at,t)]
    addTrace ts@(x : rest)
      | at >= fst x = (at,t) : ts
      | otherwise   = x : addTrace rest
addToFlows _ = id

-- | Construct a `Flows a` from a given `[SomeTrace]`. Note that `a` may be
-- specified as `SomeTrace`.
--
-- > buildFlows == foldr addFlow []
--
buildFlows :: forall a. Trace a => [SomeTrace] -> Flows a
buildFlows = foldr (addToFlows @a) []

-- | Construct a `Flow a` from a given `[SomeTrace]`. Note that `a` may be
-- specified as `SomeTrace`.
--
-- > buildFlow == mergeFlows . buildFlows
--
buildFlow :: forall a. Trace a => [SomeTrace] -> Flow a
buildFlow = mergeFlows . buildFlows

-- | Analyze a `Flow SomeTrace` for instances of the type `a`.
-- If there are no `a`s, `Nothing` is returned. On success, 
-- the `Flow` will be as shallow as possible.
-- 
-- > mapFlow == listToMaybe . mapFlows . (:[])
--
mapFlow :: Trace a => Flow SomeTrace -> Maybe (Flow a)
mapFlow = listToMaybe . mapFlows . (:[])

-- | Analyze a `Flows SomeTrace` for instances of the type `a`.
-- The `Flow`s will be as shallow as possible; `mapFlows` prunes.
mapFlows :: forall a. Trace a => Flows SomeTrace -> Flows a
mapFlows = mapMaybe go
  where
    go :: Flow SomeTrace -> Maybe (Flow a)
    go (Node (k,mapMaybe extract -> ts) (mapFlows -> cs)) 
      | [] <- ts
      , [] <- cs 
      = Nothing

      | otherwise 
      = Just (Node (k,ts) cs)

    extract :: (Time,SomeTrace) -> Maybe (Time,a)
    extract (t,st) =
      case fromTrace st of
        Nothing -> Nothing
        Just (_,_,a) -> Just (t,a)

-- | Construct a `Flow` tracer context. This tracer will integrate all `emit`ted
-- values into the `Flow` state that is supplied to the `View`.
{-# INLINE flow #-}
flow :: forall domain a. (Typeable a, Trace a) => ((Exists (Flow a), Tracing domain 0) => View) -> View
flow x = 
  with (Traces [root] :: Traces domain 0) do
    state (keyFlow [root] :: Flow a) do
      stream (modify . addToFlow @a) do
        x
  where
    {-# NOINLINE root #-}
    root :: Key
    root = unsafePerformIO keyIO

-- | Construct a `Flows` tracer context. This tracer will integrate all 
-- `emit`ted values into the `Flows` state that is supplied to the `View`.
{-# INLINE flows #-}
flows :: forall domain a. (Typeable a, Trace a) => ((Exists (Flows a), Tracing domain 0) => View) -> View
flows x = 
  state ([] :: Flows a) do
    with (Traces [root] :: Traces domain 0) do
      stream (modify . addToFlows @a) do
        x
  where
    {-# NOINLINE root #-}
    root :: Key
    root = unsafePerformIO keyIO
