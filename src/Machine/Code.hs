------------------------------------------------------------
-- Code.hs
-- Instrucciones de la maquina virtual
------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Machine.Code
  ( Instruction (..),
    Code,
    CodeInfo,
    CodeDic,
    prettyCode,
    prettyCodeDic,
  )
where

import qualified Data.Map as M
import qualified Util.Pretty as P
import Util.Pretty((+|), (|+), (+||), (||+))


type Code = [Instruction]
type CodeInfo = (Int, Code)
type CodeDic = M.Map String CodeInfo

data Instruction
  = Pushnum Int
  | Pushch Char
  | Pushfail
  | Pushglobal String
  | Pushdef Int [(Int, Int)] Code
  | Pushval Int Int
  | Slide Int Int
  | Alloc Int
  | Enter Int
  | Return
  | Eval
  | Pack String Int Int -- Name Tag NumArgs
  | Casejump [(Int, Code)]
  | Casefail Code Code
  | Error
  | Add | Sqrt
  deriving (Show, Read)


-----------
---- Pretty

instance P.Buildable Instruction where
  build (Pushnum n) = "pushnum " <> P.build n
  build (Pushch c) = "pushch '" +| c |+ "'"
  build Pushfail = "pushfail"
  build (Pushglobal s) = "pushglobal " +| s |+ ""
  build (Pushdef n cis cs) =
    "pushdef " +| n |+ " " +|| cis ||+ " {" +| cs |+ "}"
  build (Pushval x y) = "pushval " +| x |+ " " +| y |+ ""
  build (Slide m n) = "slide " +| m |+ " " +| n |+ ""
  build (Alloc n) = "alloc " +| n |+ ""
  build (Enter n) = "enter " +| n |+ ""
  build Return = "return"
  build Eval = "eval"
  build (Pack s n m) = "pack " +| s |+ " " +| n |+ " " +| m |+ ""
  build (Casejump alts) = "casejump " +| alts |+ ""
  build (Casefail cs cs') = "casefail " +| cs |+ " " +| cs' |+ ""
  build Error = "error"
  build Add = "add"
  build Sqrt = "sqrt"

instance P.Buildable Code where
  build = P.listF

instance P.Buildable CodeInfo where
  build = P.tupleF

instance P.Buildable (String, CodeInfo) where
  build = P.tupleF

prettyCode :: Code -> String
prettyCode = P.fmt . P.build

prettyCodeDic :: CodeDic -> String
prettyCodeDic =
  P.fmt . ("[+] Code Dic:\n" <>) . P.unlinesF . map P.pairMapF . M.toList

