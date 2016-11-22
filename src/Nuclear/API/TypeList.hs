module Nuclear.API.TypeList where

-- Type families for manipulating lists.

-- Append two lists. Used to guarantee the correctly ordered result
-- of a class that admits a ++ operator of some kind whose type
-- represents the types of its members.
type family Appended xs ys
  where
    Appended '[] ys = ys
    Appended (x ': xs) ys = x ': (Appended xs ys)

-- Remove all occurences of type from a list. Used to guarantee that
-- if an element of a specified type is removed from a list, the list
-- no longer contains that element.
type family Removed a xs
  where

    Removed a '[] = '[]

    Removed a (a ': xs) = Removed a xs

    Removed a (x ': xs) = x ': Removed a xs

-- Insert a type into a list if it does not exist. Used to gurantee
-- that if an element is inserted into a list, that list will contain
-- the element.
-- TODO: Generalize uses of this to a simple cons plus an (Elem a as ~ 'False) constraint.
type family Insert a xs
  where

    Insert a '[] = '[a]

    Insert a (a ': xs) = a ': xs

    Insert a (x ': xs) = x ': Insert a xs

-- Check for the existence of a type in a list. Used to guarantee existence
-- at compile-time; a guarantee of correctness of a specification with some
-- witness of that specification in part.
type family Elem a as :: Bool
  where

    Elem a '[] = 'False

    Elem a (a ': xs) = 'True

    Elem a (x ': as) = Elem a as
