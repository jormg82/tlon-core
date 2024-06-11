
--------------------------------------------------
-- CoreToCode.hs
--
-- Translation from Core to a MVT program
--------------------------------------------------

module Compiler.CoreToCode(coreToCode) where

import Core.Core
import Machine.Code

import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)


-- Posicion de un valor relativo a la pila: as[x][y]
type Pos = (Int, Int)

-- Diccionario de offsets de las variables que se
-- van definiendo en las expresiones
type PosDic = M.Map String Pos

-------- Utilidades para el manejo del diccionario
emptyDic :: PosDic
emptyDic = M.empty

pushLevelDic :: PosDic -> PosDic
pushLevelDic = M.map (\(x, y) -> (x+1, y))

lookupDic :: String -> PosDic -> Maybe Pos
lookupDic = M.lookup

-- Incrementa offsets en el primer nivel
updateNDic :: Int -> PosDic -> PosDic
updateNDic n = M.map (\p@(x, y) -> if x==0 then (x,y+n) else p)

update1Dic :: PosDic -> PosDic
update1Dic = updateNDic 1

-- Hace hueco en el primer nivel para una lista de simbolos
-- los nuevos simbolos tienen prioridad
addDic :: [String] -> PosDic -> PosDic
addDic us dic = M.fromList ps `M.union` updateNDic (length us) dic
  where
    ps = [(u, (0, n)) | (u, n) <- zip us [0..]]


---- Tlon Prelude
compiledCode :: CodeDic
compiledCode =
  M.fromList
    [
      ("id", (1, [])),
      ("k", (2, [Slide 1 1])),
      ("k1", (2, [Pushval 0 1, Slide 1 2])),
      ("s", (3, [Alloc 1, Enter 0, Pushval 1 3, Pushval 1 2, Return,
                 Pushval 0 3, Pushval 0 2, Slide 3 3])),
      ("compose", (3, [Alloc 1, Enter 0, Pushval 1 3, Pushval 1 2, Return,
                       Pushval 0 1, Slide 2 3])),
      ("twice", (1, [Pushval 0 0, Pushval 0 1, Pushglobal "compose",
                     Slide 2 1])),
      ("add", (2, [Pushval 0 1, Eval, Pushval 0 1, Eval, Add, Slide 1 2])),
      ("sqrt", (1, [Eval, Sqrt])),
      ("_error", (0, [Error])),
      ("False", (0, [Pack "False" 1 0])), -- Tag is 1-based
      ("True", (0, [Pack "True" 2 0])),
      ("Nil", (0, [Pack "Nil" 1 0])),
      ("Cons", (2, [Pack "Cons" 2 2]))
    ]


primitives :: M.Map String (Instruction, Int)
primitives =
  M.fromList
    [
      ("add", (Add, 2)),
      ("sqrt", (Sqrt, 1))
    ]


-------- Compilacion

-- Funcion principal
coreToCode :: Prog -> CodeDic
coreToCode (Prog ds) = compiledCode `M.union` M.fromList (map compile ds)


compile :: Def -> (String, CodeInfo)
compile (Def v (ELam us e)) = (v, (nvar, compileE dic nvar e))
  where
    nvar = length us
    dic = addDic us emptyDic
compile (Def v e) = (v, (0, compileE emptyDic 0 e))


slide :: Int -> Int -> Code
slide m n = if n>0 then [Slide m n] else []


-- Compilador entorno estricto, no Eval al final
compileE :: PosDic -> Int -> Exp -> Code
compileE dic n e@(ELit _) = compileC dic n e
compileE dic n e@(EVar _) = compileC dic n e
compileE dic n e@(EAp _ _)
  | isPrim && les == opargs+1 = foldCompile stepEval dic es' ++ op:slide 1 n
  | isPrim && les > opargs+1 = error "Excess args for the primitive"
  | otherwise = foldCompile stepNoEval dic es ++ slide les n
  where
    es = unwindExp e
    (e1, es') = (head es, tail es)
    les = length es
    prinfo = opPrimitive e1
    isPrim = isJust prinfo
    (op, opargs) = fromJust prinfo
    stepEval se (d, cs) = (update1Dic d, cs ++ compileAE d se)
    stepNoEval se (d, cs) = (update1Dic d, cs ++ compileAC d se)
compileE dic n (ELet isrec ds e)
  | isrec = [Alloc ndefs] ++ cs' ++ compileE dic' (n+ndefs) e
  | otherwise = foldCompile step dic es ++ compileE dic' (n+ndefs) e
  where
    ndefs = length ds
    (vs, es) = unzipDefs ds
    dic' = addDic vs dic
    step se (d, cs) = (d, cs ++ compileAC d se)
    dic'' = pushLevelDic dic'
    cs' = concat $ map (compileRecExp dic'') (reverse $ zip [0..] es)
compileE dic n (EFBar e1 e2) =
  compileAC dic e2 ++ compileAE (update1Dic dic) e1 ++
  [Casefail [Pushval 0 1, Slide 1 (n+2)] [Slide 1 (n+1)]]
compileE dic n e@(ELam _ _) = compileC dic n e
compileE dic n (ECase e as) =
  compileAE dic e ++ [Casejump $ map (compileAlt dic n) as]


-- Compilador entorno no estricto
compileC :: PosDic -> Int -> Exp -> Code
compileC _ n (ELit (LInt i)) = Pushnum i:slide 1 n
compileC _ n (ELit (LChar c)) = Pushch c:slide 1 n
compileC dic n (EVar v)
  | v == "_fail" = Pushfail:slide 1 n
  | otherwise = case lookupDic v dic of
      Just (x, y) -> Pushval x y:slide 1 n
      Nothing -> Pushglobal v:slide 1 n
compileC dic n e@(EAp _ _) = foldCompile step dic es ++ slide len n
  where
    es = unwindExp e
    len = length es
    step se (d, cs) = (update1Dic d, cs ++ compileAC d se)
compileC dic n (ELet isrec ds e)
  | isrec = [Alloc ndefs] ++ cs' ++ compileC dic' (n+ndefs) e
  | otherwise = foldCompile step dic es ++ compileC dic' (n+ndefs) e
  where
    ndefs = length ds
    (vs, es) = unzipDefs ds
    dic' = addDic vs dic
    step se (d, cs) = (d, cs ++ compileAC d se)
    dic'' = pushLevelDic dic'
    cs' = concat $ map (compileRecExp dic'') (reverse $ zip [0..] es)
compileC _ _ (EFBar _ _) = error "FBar in wrong context"
compileC dic n e'@(ELam vs e) = Pushdef (length vs) pos cs:slide 1 n
  where
    dicfrees = M.restrictKeys dic (freeVars e')
    (frees, pos) = unzip $ M.toList dicfrees
    dic' = addDic (frees++vs) emptyDic
    nargs = M.size dic'
    cs = compileE dic' nargs e
compileC dic n e@(ECase _ _) = compileC dic n (ELam [] e)


-- Compilador argumento entorno estricto, requiere Eval al final 
compileAE :: PosDic -> Exp -> Code
compileAE dic e@(ELit _) = compileC dic 0 e
compileAE dic (EVar v)
  | v == "_fail" = [Pushfail]
  | otherwise = case lookupDic v dic of
      Just (x, y) -> [Pushval x y, Eval]
      Nothing -> [Pushglobal v, Eval]
compileAE _ (EFBar _ _) = error "FBar in wrong context"
compileAE dic e@(ELam _ _) = compileC dic 0 e
compileAE dic e = [Alloc 1, Enter 0] ++ cs ++ [Return, Eval]
  where
    dic' = pushLevelDic $ update1Dic dic
    cs = compileE dic' 0 e


-- Compilador argumento entorno no estricto, sin Eval al final 
compileAC :: PosDic -> Exp -> Code
compileAC dic e@(ELit _) = compileC dic 0 e
compileAC dic e@(EVar _) = compileC dic 0 e
compileAC _ (EFBar _ _) = error "FBar in wrong context"
compileAC dic e@(ELam _ _) = compileC dic 0 e
compileAC dic e@(ECase _ _) = compileC dic 0 e
compileAC dic e = [Alloc 1, Enter 0] ++ cs ++ [Return]
  where
    dic' = pushLevelDic $ update1Dic dic
    cs = compileC dic' 0 e


foldCompile :: (Exp -> (PosDic, Code) -> (PosDic, Code))
            -> PosDic
            -> [Exp]
            -> Code
foldCompile step dic = snd . foldr step (dic, [])


compileRecExp :: PosDic -> (Int, Exp) -> Code
compileRecExp dic (n, re) = [Enter n] ++ compileC dic 0 re ++ [Return]


compileAlt :: PosDic -> Int -> Alt -> (Int, Code)
compileAlt dic n (Alt t _ vs e) =
  (t, compileE (addDic vs dic) (n+length vs) e)


opPrimitive :: Exp -> Maybe (Instruction, Int)
opPrimitive (EVar v) = M.lookup v primitives
opPrimitive _ = Nothing

