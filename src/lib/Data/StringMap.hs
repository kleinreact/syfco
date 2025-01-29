-----------------------------------------------------------------------------
-- |
-- Module      :  Data.StringMap
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- A simple data structure to map strings to integers.
--
-----------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------

module Data.StringMap
    ( StringMap
    , empty
    , lookup
    , insert
    ) where

-----------------------------------------------------------------------------

import Prelude hiding (lookup)

-----------------------------------------------------------------------------

-- | Internal data structure of the mapping.

data StringMap =
    Empty
  | Leaf (String, Int)
  | Node (Maybe Int, [(Char, StringMap)])

-----------------------------------------------------------------------------

-- | Returns the empty mapping.

empty
  :: StringMap

empty = Empty

-----------------------------------------------------------------------------

-- | Lookups a string in the mapping.

lookup
  :: String -> StringMap -> Maybe Int

lookup str mapping = case mapping of
  Empty       -> Nothing
  Leaf (e,v)  -> if e == str then Just v else Nothing
  Node (v,xs) -> case str of
    []   -> v
    x:xr -> case findMatch x xs of
      Just mapping' -> lookup xr mapping'
      _             -> Nothing

  where
    findMatch x xs = case xs of
      []           -> Nothing
      ((y,n) : xr) -> if x == y then Just n
                     else findMatch x xr

-----------------------------------------------------------------------------

-- | Inserts a new string-int pair to the given mapping. If the mapping
-- already containts the given string, then the corresponding value is
-- updated.

insert
  :: String -> Int -> StringMap -> StringMap

insert s i = \case
  Empty       -> Leaf (s, i)
  Leaf (e, v) -> case e of
    [] -> case s of
      []   -> Leaf (s, i)
      y:yr -> Node (Just v, [(y, Leaf (yr, i))])
    x:xr
      | e == s    -> Leaf (s, i)
      | otherwise -> case s of
          []               -> Node (Just i, [(x, Leaf (xr, v))])
          y:yr | x == y    -> Node (Nothing, [(x, insert yr i (Leaf (xr,v)))])
               | otherwise -> Node (Nothing, [(x, Leaf (xr,v)),(y, Leaf (yr,i))])
  Node (v,xs) -> case s of
    []     -> Node (Just i,xs)
    (x:xr) -> Node (v, add x xr i xs)

  where
    add x xr j = \case
      []            -> [(x, Leaf (xr,j))]
      ((c, n) : yr)
        | x == c    -> (c, insert xr j n) : yr
        | otherwise -> (c, n) : add x xr j yr

-----------------------------------------------------------------------------
