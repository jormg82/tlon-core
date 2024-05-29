-----------------------------------------
-- Pretty.hs
-- Utilidades para pretty printing
-----------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Util.Pretty
  ( brackets,
    parens,
    bracketListF,
    pairMapF,
    module Fmt,
  )
where

import Data.Foldable(toList)
import Data.List(intersperse)
import Fmt

brackets :: Builder -> Builder
brackets b = "{" <> b <> "}"

parens :: Builder -> Builder
parens b = "(" <> b <> ")"

bracketListF' :: Foldable f => (a -> Builder) -> f a -> Builder
bracketListF' fbuild xs = mconcat $
  "{" : intersperse ", " (map fbuild $ toList xs) ++ ["}"]

bracketListF :: (Foldable f, Buildable a) => f a -> Builder
bracketListF = bracketListF' build

pairMapF :: (Buildable a, Buildable b) => (a, b) -> Builder
pairMapF (x, y) =  "" +| x |+ ": " +| y |+ ""
