-----------------------------------------------------------------------------
-- |
-- Module      :  Reader.Sugar
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Removes syntactic sugar elements from the specification.
--
-----------------------------------------------------------------------------

module Reader.Sugar
  ( replaceSugar
  ) where

-----------------------------------------------------------------------------

import Reader.Error
  ( Error
  )

import Data.Binding
  ( Binding
  , BindExpr(..)
  )

import Reader.Data
  ( Specification(..)
  )

import Data.Expression
  ( Expr(..)
  , Expr'(..)
  )

import Data.Either
  ( lefts
  )

import Data.List.NonEmpty
  ( NonEmpty(..)
  , toList
  )

-----------------------------------------------------------------------------

-- | Replaces syntactic sugar elements in the given specification by their
-- corresponding standard elements.

replaceSugar
  :: Specification -> Either Error Specification

replaceSugar s = do
  vs <- mapM replaceBinding $ definitions s
  return s { definitions = vs }

-----------------------------------------------------------------------------

replaceBinding
  :: Binding -> Either Error Binding

replaceBinding b =
  case bVal b of
    _ :| [] -> return b
    xs      -> return b { bVal = replaceExpr xs }

-----------------------------------------------------------------------------

replaceExpr
  :: NonEmpty (Expr Int) -> NonEmpty (Expr Int)

replaceExpr xs =
  let
    ys = isOtherwise
      <$> if any ischeck $ toList xs
          then addcheck <$> xs
          else xs

    ncond p = Expr (BlnNot (orlist p $ map cond $ lefts $ toList ys)) p
  in
    either id id . fmap (replace ncond) <$> ys

  where
    ischeck e = case expr e of
      Colon {} -> True
      _        -> False

    cond e = case expr e of
      Colon x _ -> x
      _         -> Expr BaseTrue (srcPos e)

    isOtherwise e = case expr e of
      Colon x _ -> case expr x of
        BaseOtherwise -> Right e
        _             -> Left e
      _        -> Left e

    addcheck e = case expr e of
      Colon {} -> e
      _        -> Expr (Colon (cond e) e) $ srcPos e

    replace f e = case expr e of
      Colon _ y -> Expr (Colon (f (srcPos e)) y) $ srcPos e
      _         -> e

    orlist p = foldl (fldor p) (Expr BaseFalse p)

    fldor p e1 e2 = Expr (BlnOr e1 e2) p

-----------------------------------------------------------------------------
