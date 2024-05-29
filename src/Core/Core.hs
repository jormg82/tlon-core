--------------------------------------------------
-- Core.hs
-- Core language
--------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Core.Core
  ( IsRec,
    recursive,
    nonRecursive,
    Lit (..),
    Exp (..),
    Def (..),
    Alt (..),
    Prog (..),
    unwindExp,
    freeVars,
    unzipDefs,
    prettyProg,
  )
where

import qualified Data.Set as S
import Util.Pretty ((+|), (|+))
import qualified Util.Pretty as P

type IsRec = Bool

recursive, nonRecursive :: IsRec
recursive = True
nonRecursive = False


data Lit
  = LInt Int
  | LChar Char
  deriving (Show)

data Exp
  = ELit Lit
  | EVar String
  | EAp Exp Exp
  | ELet IsRec [Def] Exp
  | EFBar Exp Exp
  | ELam [String] Exp
  | ECase Exp [Alt]
  deriving (Show)

data Def = Def {defVar :: String, defExp :: Exp}
  deriving (Show)

data Alt = Alt {altTag :: Int,
                altConstr :: String,
                altVars :: [String],
                altExp :: Exp}
           deriving (Show)

data Prog = Prog {progDefs :: [Def]}
  deriving (Show)


-- Utilidades
unwindExp :: Exp -> [Exp]
unwindExp (EAp e1 e2) = unwindExp e1 ++ [e2]
unwindExp e = [e]

unzipDefs :: [Def] -> ([String], [Exp])
unzipDefs ds = unzip [(v, e) | Def v e <- ds]

-- Variables libres de una expresion Core
freeVars :: Exp -> S.Set String
freeVars (ELit _) = S.empty
freeVars (EVar v) = S.singleton v
freeVars (EAp e1 e2) = freeVars e1 `S.union` freeVars e2
freeVars (ELet _ ds e) =
  (freeVars e `S.union` S.unions frees) S.\\ S.fromList vs
  where
    (vs, frees) = unzip [(v, freeVars e') | Def v e' <- ds]
freeVars (EFBar e1 e2) = freeVars e1 `S.union` freeVars e2
freeVars (ELam us e) = freeVars e S.\\ S.fromList us
freeVars (ECase e as) = freeVars e `S.union` fralts
  where
    fralts = S.unions [freeVars e' S.\\ S.fromList us | Alt _ _ us e' <- as]


-- Pretty
instance P.Buildable Lit where
  build (LInt n) = P.build n
  build (LChar c) = "'" +| c |+ "'"

instance P.Buildable Def where
  build (Def x e) = "" +| x |+ " = " <> buildExp False e

instance P.Buildable Alt where
  build (Alt n constr vs e) =
    "<" +| n |+ "> " +| constr |+ " " <> P.unwordsF vs <>
    " -> " <> buildExp False e

instance P.Buildable Prog where
  build (Prog ds) = P.unlinesF ds

buildExp :: Bool -> Exp -> P.Builder
buildExp _ (ELit lit) = P.build lit
buildExp _ (EVar x) = P.build x
buildExp isatom (EAp e1 e2) =
  if isatom then P.parens app else app
  where
    app = buildExp False e1 <> " " <> buildExp True e2
buildExp isatom (ELet isrec ds e) =
  if isatom then P.parens elet else elet
  where
    leth = if isrec then "letrec " else "let "
    elet = leth <> "\n" <> (P.indentF 2 $ P.unlinesF ds) <>
           "in\n" <> (P.indentF 2 $ buildExp False e)
buildExp isatom (EFBar e1 e2) =
  if isatom then P.parens app else app
  where
    app = buildExp False e1 <> " [||] " <> buildExp False e2
buildExp _ (ELam xs e) =
  P.brackets $ P.unwordsF xs <> " -> " <> buildExp False e
buildExp _ (ECase e as) =
  "case " <> buildExp False e <> " of\n" <> (P.indentF 2 $ P.unlinesF as)

prettyProg :: Prog -> String
prettyProg = P.fmt . ("[+] Program:\n" <>) . P.build

