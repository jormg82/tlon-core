------------------------------------------------------------
-- Node.hs
------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Machine.Node
  ( Val (..),
    Node (..),
    isPtr,
    prettyNode,
  )
where

import qualified Machine.Code as C
import Machine.Heap (Addr)
import qualified Util.Pretty as P
import Util.Pretty((+|), (|+))

data Val
  = Num Int
  | Ch Char
  | Fail
  | Constr String Int [Val]
  | Ptr Addr
  deriving (Show)

data Node
  = Def Int [Val] C.Code
  | Seq [Val]
  deriving (Show)

-- Utilities
isPtr :: Val -> Bool
isPtr (Ptr _) = True
isPtr _ = False


-----------
---- Pretty

instance P.Buildable Val where
  build (Num n) = "num " +| n |+ ""
  build (Ch c) = "char '" +| c |+ "'"
  build Fail = "fail"
  build (Constr s n vs) =
    "<" +| s |+ " " +| n |+ " "
    +| P.unwordsF (map (P.parens . P.build) vs) |+ ">"
  build (Ptr a) = "ptr " +| a |+ ""

instance P.Buildable Node where
  build (Def n vs cs) =
    "def " +| n |+ " " +| P.listF vs |+ " " +| P.listF cs |+ ""
  build (Seq vs) = "seq" +| P.bracketListF vs |+ ""

prettyNode :: Node -> String
prettyNode = P.fmt . P.build

