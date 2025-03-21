{-# LANGUAGE UndecidableInstances, InstanceSigs, MultiParamTypeClasses, ImpredicativeTypes, NoMonomorphismRestriction, UndecidableSuperClasses, AllowAmbiguousTypes, RecursiveDo, NondecreasingIndentation, PartialTypeSignatures, OverloadedLists, ApplicativeDo, MonomorphismRestriction, MagicHash, UnboxedTuples, BlockArguments, TypeApplications, TypeOperators, DerivingVia, TupleSections, ViewPatterns, ScopedTypeVariables, RecordWildCards, ConstraintKinds, GADTs, TypeFamilies, FlexibleContexts, QuantifiedConstraints, DataKinds #-}
-- Avoid lambda is not a safe rule in this module. 
{-# HLINT ignore "Avoid lambda" #-}
module Pure where

import Control.Applicative ( Applicative(..), Alternative(..) )
import Control.Arrow
import Control.Category ( Category(..) )
import Control.Concurrent.Async ( async, wait, Async )
import Control.Exception ( Exception(..) )
import Control.Monad ( MonadPlus(..), void, ap, liftM2, foldM )
import Control.Monad.Catch ( MonadThrow(..), MonadCatch(..), MonadMask(..), ExitCase(..) )
import Control.Monad.Fix ( MonadFix(..) )
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Primitive ( unsafeIOToPrim, unsafePrimToIO, PrimBase(..), PrimMonad(..) )
import Control.Monad.Zip ( MonadZip(..) )
import Data.Functor ((<&>))
import Data.Primitive.MVar ( MVar, newMVar, takeMVar, putMVar, readMVar, tryTakeMVar )
import Data.Unique
import Data.Void ( Void, absurd )
import GHC.Base ( State#, seq#, inline, RealWorld, runRW#, unsafeCoerce#, Constraint )
import Prelude hiding ((.),id)
import Unsafe.Coerce ( unsafeCoerce )

-- type Program a = () |= a

-- {-# INLINE run #-}
-- run :: Program a -> a
-- run (Proof a) = unsafeCoerce# runRW# a

one :: forall a c. (c => Exists a) => c |= a
one = proof (it :: a)

type PRep (c :: Constraint) (a :: *) = State# RealWorld -> (# State# RealWorld, a #)

-- I prefer this formulation as the constraint is guaranteed solvable 
-- from any code that can witness a token with the same constraint type,
-- but there is much code in GHC's base and the RTS that are designed for
-- RealWorld tokens, so I use that, instead. Since they are functionally
-- equivalent from the perspective of `prove`, it shouldn't matter much.
--
-- data Satisfied (c :: Constraint)
-- satisfied# :: forall c. c => State# (Satisfied c)
-- satisfied# = unsafeCoerce# realWorld#

infixr 9 |=
newtype (c :: Constraint) |= a where
  Proof :: (c => PRep c a) -> c |= a

-- Note that 'pure' is implemented in terms of 'proof',
-- but 'proof' cannot be implemented in terms of 'pure'.
{-# INLINE [0] proof #-}
proof :: (c => a) -> (c |= a)
proof a = Proof (# , a #)

eager :: (c => a) -> (c |= a)
eager a = Proof (seq# a)

force :: c |= a -> c |= a
force p = eager (prove p)

unP :: c |= a -> c => PRep c a
unP (Proof p) = p

embed :: (c => d) => d |= a -> c |= a
embed (Proof p) = Proof \s -> case p s of (# new_s, a #) -> (# a `seq` new_s, a #)

compile :: c |= a -> c |= (() |= a)
compile ca = proof (proof (prove ca))

compileF :: forall c a b. (a -> c |= b) -> c |= (a -> () |= b)
compileF acb = fmap (fmap pure) (proof (\a -> prove (acb a)))

-- push :: (a -> c |= b) -> c |= (a -> b)
-- push f = proof (\a -> prove (f a))

{-# INLINE [1] prove #-}
prove :: (c |= a) -> (c => a)
prove (Proof p) = case runRW# p of (# _, a #) -> a

pfix :: (a -> c |= a) -> c |= a
pfix f = proof (let (prove -> a) = f a in a)

invert :: (c |= a -> a) -> (a -> c |= a)
invert f a = proof (f (proof a))

wfix :: c |= (c |= a -> a) -> (c => a)
wfix f = prove (cfix (prove f))

kfix :: c |= (c |= a -> a) -> c |= a
kfix f = proof (wfix f)

cfix :: (c |= a -> a) -> c |= a
cfix f = pfix (invert f)

(=>>) :: c |= a -> (c |= a -> b) -> c |= b
(=>>) ca cab = proof (cab ca)

(<<=) :: (c |= a -> b) -> c |= a -> c |= b
(<<=) cab ca = proof (cab ca)

(=<=) :: (c |= b -> d) -> (c |= a -> b) -> c |= a -> d
f =<= g = f . extend g where
  extend :: ((c |= a) -> b) -> (c |= a) -> c |= b
  extend cab ca = proof (cab ca)

(=>=) :: (c |= a -> b) -> (c |= b -> d) -> c |= a -> d
f =>= g = g . extend f where
  extend :: ((c |= a) -> b) -> (c |= a) -> c |= b
  extend cab ca = proof (cab ca)

(<@@>) :: c |= a -> c |= (a -> b) -> c |= b
(<@@>) = liftA2 (flip id)

-- join == retract
retract :: (m ~ (|=) c) => m (m a) -> m a
retract p = proof (prove (prove p))

protract :: (c |= a -> b) -> c |= a -> c |= b
protract f p = proof (f p)

unsafeIOToP :: forall c a. (c => IO a) -> c |= a
unsafeIOToP ioa = proof (prove @c (unsafeIOToPrim ioa))

-- pToIO :: forall c a. c |= a -> (c => IO a)
-- pToIO = unsafePrimToIO

instance Semigroup a => Semigroup (c |= a) where
  {-# INLINE (<>) #-}
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (c |= a) where
  {-# INLINE mempty #-}
  mempty = pure mempty

instance Functor ((|=) c) where
  {-# INLINE fmap #-}
  fmap :: (a -> b) -> c |= a -> c |= b
  fmap f (Proof p) = Proof \s ->
    case p s of (# new_s, a #) -> (# new_s, f a #)

instance Applicative ((|=) c) where
  {-# inline pure #-}
  pure :: a -> c |= a
  pure a = Proof (# , a #)

  {-# INLINE (<*>) #-}
  (<*>) :: c |= (a -> b) -> c |= a -> c |= b
  (<*>) = ap

  {-# inline (*>) #-}
  (*>) :: c |= a -> c |= b -> c |= b
  (*>) ca cb = ca >>= const cb

  {-# INLINE liftA2 #-}
  liftA2 :: (a -> b -> x) -> (c |= a) -> (c |= b) -> (c |= x)
  liftA2 = liftM2

instance Monad ((|=) c) where
  {-# inline (>>=) #-}
  (>>=) :: (c |= a) -> (a -> (c |= b)) -> (c |= b)
  Proof ca >>= acb = Proof \s ->
    case ca s of { (# new_s, a #) ->
    case acb a of { Proof f ->
    f new_s
    } }

  {-# INLINE (>>) #-}
  (>>) :: (c |= a) -> (c |= b) -> (c |= b)
  (>>) = (*>)

instance MonadFail ((|=) c) where
  {-# INLINE fail #-}
  fail :: String -> c |= a
  fail s =
    proof do
      prove @c do
        unsafeIOToPrim (fail s)

instance MonadFix ((|=) c) where
  {-# INLINE mfix #-}
  mfix :: (a -> c |= a) -> c |= a
  mfix f = eager (let a = prove (f a) in a)
  -- mfix f = unsafeIOToP do
  --   mv <- newEmptyMVar
  --   ans <- unsafeDupableInterleaveIO do
  --     catch (readMVar mv) \BlockedIndefinitelyOnMVar -> do
  --       throwIO NonTermination
  --   result <- unsafePToIO (f ans)
  --   putMVar mv result
  --   return result

instance MonadCatch ((|=) c) where
  {-# INLINE catch #-}
  catch :: forall e a. Exception e => (c |= a) -> (e -> c |= a) -> c |= a
  catch ca eca =
    proof do
      prove @c do
        unsafeIOToPrim (catch (unsafePrimToIO ca) (unsafePrimToIO . eca))

instance MonadThrow ((|=) c) where
  {-# INLINE throwM #-}
  throwM :: forall e a. Exception e => e -> c |= a
  throwM e =
    proof do
      prove @c do
        unsafeIOToPrim (throwM e)

instance MonadMask ((|=) c) where
  {-# INLINE mask #-}
  mask :: ((forall a. (c |= a) -> c |= a) -> c |= b) -> c |= b
  mask f =
    proof do
      prove @c do
        unsafeIOToPrim do
          mask \g ->
            unsafePrimToIO (f (unsafeIOToPrim . g . unsafePrimToIO))

  {-# INLINE uninterruptibleMask #-}
  uninterruptibleMask :: ((forall a. (c |= a) -> c |= a) -> c |= b) -> c |= b
  uninterruptibleMask f =
    proof do
      prove @c do
        unsafeIOToPrim do
          uninterruptibleMask \g ->
            unsafePrimToIO (f (unsafeIOToPrim . g . unsafePrimToIO))

  {-# INLINE generalBracket #-}
  generalBracket :: (c |= x) -> (x -> ExitCase y -> c |= z) -> (x -> c |= y) -> c |= (y, z)
  generalBracket acquire release use =
    proof do
      prove @c do
        unsafeIOToPrim do
          generalBracket
            (unsafePrimToIO acquire)
            (\x ecy -> unsafePrimToIO (release x ecy))
            (unsafePrimToIO . use)

instance MonadZip ((|=) c)  where
  {-# INLINE mzipWith #-}
  mzipWith :: (x -> y -> z) -> (c |= x) -> (c |= y) -> (c |= z)
  mzipWith = liftA2

instance Alternative ((|=) c) where
  {-# INLINE empty #-}
  empty :: c |= a
  empty = unsafeIOToPrim empty

  {-# INLINE (<|>) #-}
  (<|>) :: (c |= a) -> (c |= a) -> c |= a
  (<|>) l r =
    proof do
      prove @c do
        unsafeIOToPrim do
          (<|>)
            (unsafePrimToIO l)
            (unsafePrimToIO r)

instance MonadPlus ((|=) c) where
  {-# INLINE mzero #-}
  mzero :: c |= a
  mzero =
    proof do
      prove @c do
        unsafeIOToPrim mzero

  {-# INLINE mplus #-}
  mplus :: (c |= a) -> (c |= a) -> c |= a
  mplus l r =
    proof do
      prove @c do
        unsafeIOToPrim do
          mplus
            (unsafePrimToIO l)
            (unsafePrimToIO r)

instance MonadIO ((|=) c) where
  {-# INLINE liftIO #-}
  liftIO :: IO a -> c |= a
  liftIO = unsafeIOToPrim

instance PrimMonad ((|=) c) where
  type PrimState ((|=) c) = RealWorld
  {-# INLINE primitive #-}
  primitive :: PRep c a -> c |= a
  primitive = Proof

instance c => PrimBase ((|=) c) where
  {-# INLINE internal #-}
  internal :: (c |= a) -> PRep c a
  internal (Proof p) = p

type Global a = Shared () a

global :: a -> c |= Global a
global = shared

modifyGlobal :: Global a -> (a -> c |= a) -> c |= a
modifyGlobal = modifyShared

type Shared c a = MVar (PrimState ((|=) c)) a

shared :: a -> c |= Shared c a
shared = newMVar

modifyShared :: forall c a. Shared c a -> (a -> c |= a) -> c |= a
modifyShared v f = do
  a <- takeShared v
  a' <- f a
  putShared v a'
  pure a'

modifyShared_ :: forall c a. Shared c a -> (a -> c |= a) -> c |= ()
modifyShared_ v f = void (modifyShared v f)

readShared :: Shared c a -> c |= a
readShared = readMVar

putShared :: Shared c a -> a -> c |= ()
putShared = putMVar

takeShared :: Shared c a -> c |= a
takeShared = takeMVar

type Var c a = MVar (PrimState ((|=) c)) a

var :: a -> c |= Var c a
var = newMVar

modifyVar :: forall c a. Var c a -> (a -> c |= a) -> c |= a
modifyVar v f = do
  a <- takeMVar v
  a' <- f a
  putMVar v a'
  pure a'

modifyVar_ :: forall c a. Var c a -> (a -> c |= a) -> c |= ()
modifyVar_ v f = void (modifyVar v f)

readVar :: Var c a -> c |= a
readVar = readMVar

writeVar :: Var c a -> a -> c |= ()
writeVar v a = tryTakeMVar v >> putMVar v a

data Fork m a = Fork { thread :: Async a , awaits :: m a }

fork :: PrimBase m => m a -> m (Fork m a)
fork ma = unsafeIOToPrim do
  a <- async (unsafePrimToIO ma)
  pure Fork { thread = a , awaits = unsafeIOToPrim (wait a) }

infixr 0 -->
newtype (-->) a b = Function { unFunction :: Exists a |= b }
  deriving (Functor,Applicative,Monad
           ,MonadIO,PrimMonad,PrimBase
           ,MonadPlus,Alternative,MonadZip
           ,MonadMask,MonadThrow,MonadCatch
           ,MonadFix,MonadFail
           ) via ((|=) (Exists a))

uses :: (Exists a => b) -> a --> b
uses = func

func :: (Exists a => b) -> a --> b
func f = Function (proof f)

eval :: a -> (a --> b) -> b
eval a ab = with a (prove (unFunction ab))

($) :: (a --> b) -> a -> b
($) = flip eval

instance Category (-->) where
  id :: a --> a
  id = func it

  (.) :: forall b c a. (b --> c) -> (a --> b) -> (a --> c)
  (.) bc ab = func (eval (eval it ab) bc)

instance Arrow (-->) where
  arr :: (b -> c) -> b --> c
  arr f = func (f it)

  first :: forall b c d. (b --> c) -> (b, d) --> (c, d)
  first bc =
    func do
      let (b,d) :: (b,d) = it
      (eval b bc,d)

instance ArrowZero (-->) where
  zeroArrow :: b --> c
  zeroArrow = Function mzero

instance ArrowPlus (-->) where
  (<+>) :: (b --> c) -> (b --> c) -> b --> c
  (<+>) (Function l) (Function r) = Function (l `mplus` r)

instance ArrowChoice (-->) where
  left :: forall b c d. (b --> c) -> Either b d --> Either c d
  left bs =
    func do
      either
        (\b -> Left (eval b bs))
        Right
        (it :: Either b d)

instance ArrowApply (-->) where
  app :: forall b c. (b --> c, b) --> c
  app =
    func do
      let (bc,b) = it :: (b --> c,b)
      eval b bc

instance ArrowLoop (-->) where
  loop :: forall b d c. (b, d) --> (c, d) -> b --> c
  loop bdcd =
    func do
      let (c,d) = eval (it,d) bdcd
      c

class Exists a where it :: a
instance Exists Void where it = absurd (it :: Void)

newtype Witness a r = Witness (Exists a => r)

{-# INLINE with #-}
with :: forall a r. a -> (Exists a => r) -> r
with a w = inline (unsafeCoerce (Witness w :: Witness a r)) a

newtype Consumer a = Consumer { runConsumer :: a -> () |= () }
type Producer a = Exists (Consumer a)

consume :: forall a b c. (a -> c |= ()) -> (Producer a => c |= b) -> c |= b
consume f b = do
  g <- compileF f
  with (Consumer g) b

yields :: forall a c. (c => Producer a) => a -> c |= ()
yields a = do
  Consumer f <- proof it
  f $$ a

modify :: forall a b c d. (d => Modify c a) => (a -> d |= (a,b)) -> d |= b
modify f = do
  v <- var undefined
  g <- compileF \a -> do
    (a',b) <- f a
    writeVar v b
    pure a'
  yields @(a -> c |= a) (g $$)
  readVar v

modify_ :: forall a c d. (d => Modify c a) => (a -> c |= a) -> d |= ()
modify_ f = do
  Consumer c <- proof it
  embed @d (c f)

type Modify c a = Producer (a -> c |= a)

(<<) :: (a --> b) -> a -> b
f << x = eval x f

($$) :: (d => c) => (a -> c |= b) -> a -> (d |= b)
f $$ a = embed (f a)

state
  :: forall s c a
   . (Modify c s => c |= s)
  -> (Modify c s => s --> c |= s)
  -> (Modify c s => s --> c |= a)
  -> c |= a
state initial f a = mdo
  st :: MVar RealWorld st <- var undefined

  let
    update :: (st ~ (s,s --> c |= s)) => (s -> c |= s) -> (st -> c |= st)
    update f' (latest,after) = ((f' $$ latest) >>= (after <<)) <&> (,after)

  (s0,ca) <- consume (\(g :: (s -> c |= s)) -> modifyVar_ st (update g)) do
    s <- initial
    pure ((s,f),a << s)

  writeVar st s0
  ca

data Reactive c a = Reactive { those :: a, them :: [(Unique,a --> c |= a)] }

link :: forall a c b. (a --> c |= b) -> c |= (a --> b)
link f = do
  g <- compileF (`eval` f)
  pure (func (prove (g it)))

reactive
  :: forall s c a
   . (Modify c (Reactive c s) => c |= s)
  -> (Modify c (Reactive c s) => s --> c |= s)
  -> (Modify c (Reactive c s) => s --> c |= a)
  -> c |= a
reactive initial upd a =
  state @(Reactive c s) initialize update do
    func @(Reactive c s) do
      let Reactive {..} = it :: Reactive c s
      eval those a
  where
    initialize :: Modify c (Reactive c s) => c |= Reactive c s
    initialize = do
      s <- initial
      u <- liftIO newUnique
      pure (Reactive s [(u,upd)])

    update :: Reactive c s --> (c |= Reactive c s)
    update = func do
      let Reactive {..} = it :: Reactive c s
      new <- foldM (\s -> eval s . snd) those them
      pure (Reactive new them)
