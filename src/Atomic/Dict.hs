{-# language CPP #-}
{-# language ImpredicativeTypes #-}
module Atomic.Dict where

import Ef
import Ef.State

-- import Control.Lens
import Data.Typeable

import qualified Data.HashMap.Strict as Map

import Data.Txt
import Atomic.Cond
import Atomic.ToTxt
import Atomic.FromTxt
import Atomic.Default
import Atomic.UnsafeEq

import GHC.Exts
import Data.String
import Debug.Trace

import Unsafe.Coerce

data Val = forall a. Typeable a => Val a
instance IsString Val where -- convenience, be careful
  fromString str = Val (fromString str :: Txt)
instance ToTxt Val where
  toTxt (Val v) =
    case cast v of
      Nothing -> mempty
      Just x   -> x
instance FromTxt Val where
  fromTxt = Val
-- item :: (Typeable a) => Iso a (Maybe a) Val Val
-- item = iso Val (\(Val v) -> cast v)

data Dict = Dict
  { dict_map :: Map.HashMap Txt Val
  , dict_upd :: (Maybe (forall ms. Dict -> Ef ms IO ()))
  }
instance Eq Dict where
  (==) (Dict s _) (Dict s' _) = reallyUnsafeEq s s'
instance Default Dict where
  def = Dict Map.empty Nothing
instance Monoid Dict where
  mempty = def
  mappend (Dict l (Just lu)) (Dict r _) = Dict (l <> r) (Just $ unsafeCoerce lu)
  mappend (Dict l _) (Dict r (Just ru)) = Dict (l <> r) (Just $ unsafeCoerce ru)
instance GHC.Exts.IsList Dict where
  type Item Dict = (Txt,Val)
  fromList xs = def { dict_map = Map.fromList xs } 
  toList st = Map.toList (dict_map st)

setDict :: ('[State () Dict] <: ms, Monad c) => Dict -> Ef ms c ()
setDict st = do
  st' <- get
  case dict_upd st' of
    Nothing  -> return ()
    Just upd -> (unsafeCoerce upd) st { dict_upd = dict_upd st' }

-- Guarantee a value exists in a `Dict`, i.e. insert if non-existent. Print a trace statement
-- if debugging is enabled via DEBUGDICT or DEVEL.
(%|) :: forall a c ms. ('[State () Dict] <: ms, Monad c, Typeable a) => Txt -> a -> Dict -> Ef ms c ()
(%|) k v (Dict st upd) =
  let new = Dict (Map.insert k (Val v) st) upd in
    -- prints a trace statement if the type of value at the given key index is unexpected
    case Map.lookup k st of
      Just (Val s) -> let { t1 = typeOf (undefined :: a); t2 = typeOf v } in
        if t1 == t2 then
          return ()
        else
#if defined(DEBUGDICT) || defined(DEVEL)
          trace ("Type mismatch in existence check (%|) at key: " ++ show k ++ "; expected type: " ++ show t1 ++ "; got type: " ++ show t2 ++ ". Not overwriting!") (return ())
#else
          return ()
#endif
      _ -> setDict new

-- Given a `Dict` and a function `Dict -> Dict`, call `setDict` on the result
-- of the transformation; like `&` with an implicit `setDict` call.
--
-- > d &| ("name" =| nm) . ("pass" =| p)
infixr 4 &|
(&|) :: ('[State () Dict] <: ms, Monad c) => Dict -> (Dict -> Dict) -> Ef ms c ()
(&|) st upd = setDict (upd st)

-- Index a value in a `Dict`. Failure produces an error.
(!|) :: forall a. Typeable a => Dict -> Txt -> a
(!|) (Dict st _) k =
  case Map.lookup k st of
    Just (Val s) ->
      case cast s of
        Just x -> x
        _      -> error ("Invalid dict index (!|) for key: " ++ show k ++  "; expected type: " ++ show (typeOf (undefined :: a)) ++ "; got type: " ++ show (typeOf s))
    Nothing -> error ("Invalid dict index (!|); value not found for key: " ++ show k)

-- Look up a conditional value in a `Dict` and return `nil` if nothing is found. If
-- developmental debugging is enabled, cast failure will print a trace statement
-- containing the expected and retrieved value types.
(?|) :: forall a. Typeable a => Dict -> Txt -> Maybe a
(?|) (Dict st _) k =
  case Map.lookup k st of
    Just (Val s) ->
      case cast s of
        Just a   -> Just a
#if defined(DEBUGDICT) || defined(DEVEL)
        _        -> trace ("Invalid dict lookup (?|) for key: " ++ show k ++  "; expected type: " ++ show (typeOf (undefined :: a)) ++ "; got type: " ++ show (typeOf s)) Nothing
#else
        _        -> Nothing
#endif
    _            -> Nothing

-- Look up a conditional value in a `Dict` and return `nil` if nothing is found. If
-- developmental debugging is enabled, cast failure will print a trace statement
-- containing the expected and retrieved value types.
(??|) :: forall a. (Typeable a, Cond a) => Dict -> Txt -> a
(??|) (Dict st _) k =
  case Map.lookup k st of
    Just (Val s) ->
      case cast s of
        Just a   -> a
#if defined(DEBUGDICT) || defined(DEVEL)
        _        -> trace ("Invalid default conditional dict lookup (??|) for key: " ++ show k ++  "; expected type: " ++ show (typeOf (undefined :: a)) ++ "; got type: " ++ show (typeOf s)) nil
#else
        _        -> nil
#endif
    _            -> nil

-- Modify a value in a `Dict` that then must have `setDict` called upon it to update.
-- `&|` can be used along with `~|` for a convenient syntax. Note that this update
-- will only work with concrete types since `Dict` values are typeable-polymorphic.
-- Note that the type of the result of the update is the same as the type of the
-- value; it is best practice not to change the type of a value at a key.
--
-- > d &| ("count" ~| (+ (1 :: Int)))
(~|) :: forall a. Typeable a => Txt -> (a -> a) -> Dict -> Dict
(~|) k f (Dict st upd) =
  case Map.lookup k st of
    Nothing ->
#if defined(DEBUGDICT) || defined(DEVEL)
      trace ("Dict lookup failed in update (~|) at key: " ++ show k ++ "; expected type: " ++ show (typeOf (undefined :: a)) ++ "; got type: " ++ show (typeOf s) ++ ". Update not performed!") (Dict st upd)
#else
      Dict st upd
#endif
    Just (Val s) ->
      case cast s of
        Just a  -> Dict (Map.insert k (Val (f a)) st) upd
        Nothing ->
#if defined(DEBUGDICT) || defined(DEVEL)
          trace ("Dict type mismatch in update (~|) at key: " ++ show k ++ "; expected type: " ++ show (typeOf (undefined :: a)) ++ "; got type: " ++ show (typeOf s) ++ ". Update not performed!") (Dict st upd)
#else
          Dict st upd
#endif

-- Set a value in a `Dict` that then must have `setDict` called on it to update.
-- `&|` can be used along with `=|` for a convenient syntax.
--
-- > d &| ("name" =| nm) . ("pass" =| p)
(=|) :: forall a. Typeable a => Txt -> a -> Dict -> Dict
(=|) k v (Dict st upd) =
  let new = Dict (Map.insert k (Val v) st) upd in
#if defined(DEBUGDICT) || defined(DEVEL)
    -- convenient overwrite tracing in the case of type mismatch since Dict are, implicitly, poorly typed.
    case Map.lookup k st of
      Just (Val s) -> let { t1 = typeOf (undefined :: a); t2 = typeOf v } in
        if t1 == t2 then
          new
        else
          trace ("Type mismatch in overwrite at key: " ++ show k ++ "; expected type: " ++ show t1 ++ "; got type: " ++ show t2 ++ ". Ovewriting!") new
      _ -> new
#else
    new
#endif

-- Set a `Txt` value in a `Dict` that then must have `setDict` called on it to update.
-- `&|` can be used along with `=:|` for a convenient syntax. This is a convience to
-- avoid calling `toTxt` on values in the presence of the `OverloadedStrings` pragma.
--
-- > d &| ("name" =:| "User Name") . ("pass" =:| "Password")
(=:|) :: Txt -> Txt -> Dict -> Dict
(=:|) k v (Dict st upd) =
  let  new = Dict (Map.insert k (Val v) st) upd in
#if defined(DEBUGDICT) || defined(DEVEL)
    -- convenient overwrite tracing in the case of type mismatch since Dict are, implicitly, poorly typed.
    case Map.lookup k st of
        Just (Val s) -> let { t1 = typeOf (undefined :: a); t2 = typeOf v } in
          if t1 == t2 then
            new
          else
            trace ("Dict type mismatch in Txt overwrite (=:|) at key: " ++ show k ++ "; expected type: " ++ show t1 ++ "; got type: " ++ show t2 ++ ". Overwriting!") new
        _ -> new
#else
    new
#endif

-- Remove a value from a `Dict` by key. Partial application of remove is
-- useful when combined with the `&|` update operator.
--
-- > d &| (remove "name") . (remove "pass")
remove :: Txt -> Dict -> Dict
remove k (Dict st upd) = Dict (Map.delete k st) upd

-- Synonym for `remove`.
--
-- > d &| ("name" -|) . ("pass" -|)
(-|) :: Txt -> Dict -> Dict
(-|) = remove

