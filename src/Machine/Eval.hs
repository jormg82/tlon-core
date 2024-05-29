------------------------------------------------------------
-- Eval.hs
-- MCode virtual machine
------------------------------------------------------------

module Machine.Eval(eval) where

import Machine.Code
import qualified Machine.Heap as H
import Machine.Node
import Machine.State

import Control.Monad(when)
import Control.Monad.Loops(whileM_)
import qualified Data.List.NonEmpty as L
import Data.List.NonEmpty( NonEmpty( (:|) ), (<|) )
import qualified Data.Map as M
import Data.Maybe(fromJust)
 

-- Inicializa heap, stack y globals con los resultados de la compilacion
initState :: CodeDic -> MS ()
initState d = do
  let (h, g) = M.foldrWithKey allocG (H.initial, M.empty) d
      (h', a) = H.alloc h (Seq [])
  putCode [Pushglobal "main", Eval]
  putStack $ L.singleton a
  putHeap h'
  putGlobals g


allocG :: String
       -> CodeInfo
       -> (H.Heap Node, Globals)
       -> (H.Heap Node, Globals)
allocG s (i, cs) (h, g) = (h', M.insert s a g) 
  where
   (h', a) = H.alloc h (Def i [] cs)


-- Funcion principal
eval :: CodeDic -> IO ()
eval dic = do
  result <- runMS (run dic)
  case result of
    Left s   -> putStrLn $ "Evaluation error: " ++ s
    Right () -> putStrLn "Evaluation OK" 


run :: CodeDic -> MS ()
run dic = do
  initState dic
  whileM_ (traceState >> not <$> finalState) step


finalState :: MS Bool
finalState = do
  (cs, a:|as, h) <- (,,) <$> getCode <*> getStack <*> getHeap
  -- OJO revisar condiciones de estado final
  case (cs, as, H.lookup h a) of
    ([], [], Just (Seq [v])) -> return $ not $ isPtr v
    (_, _, Just (Seq _)) -> return False
    _ -> throw "Illegal state in final state check"


step :: MS ()
step = do
  incSteps
  (cod, s@(a:|_), h) <- (,,) <$> getCode <*> getStack <*> getHeap
  case (cod, H.lookup h a) of
    (c:cs, Just (Seq vs)) -> do
      putCode cs
      dispatch c cs s vs h
    _ -> throw "Illegal state in step"


dispatch :: Instruction
         -> Code
         -> Stack
         -> [Val]
         -> H.Heap Node
         -> MS ()
dispatch (Pushnum i) _ (a:|_) vs h = putHeap $ H.update h a (Seq $ Num i:vs)
dispatch (Pushch i) _ (a:|_) vs h = putHeap $ H.update h a (Seq $ Ch i:vs)
dispatch Pushfail _ (a:|_) vs h = putHeap $ H.update h a (Seq $ Fail:vs)

dispatch (Pushglobal s) _ (a:|_) vs h = do
  g <- getGlobals
  case M.lookup s g of
    Just a' -> putHeap $ H.update h a (Seq $ Ptr a':vs)
    Nothing -> throw $ "No such global: " ++ s

dispatch (Pushdef k xs cs) _ s@(a:|_) vs h = do
  vs' <- traverse (peekCoord s h) xs
  let (h', a') = H.alloc h (Def k vs' cs)
  putHeap $ H.update h' a (Seq $ Ptr a':vs)

dispatch (Pusharg n m ) _ s@(a:|_) vs h = do
  v <- peekCoord s h (n, m)
  putHeap $ H.update h a $ Seq $ v:vs

dispatch (Slide n m ) _ (a:|_) vs h = do
  let (s1, s2) = splitAt n vs
  putHeap $ H.update h a (Seq $ s1++drop m s2)

dispatch (Alloc n) _ (a:|_) vs h = do
  let (h', as') = H.allocn n h $ Seq []
  putHeap $ H.update h' a $ Seq $ map Ptr as'++vs

dispatch (Enter n) _ s vs _ = do
  when (n >= length vs) (throw "Illegal position in enter instruction")
  case vs!!n of
    Ptr a' -> putStack $ a'<|s
    _ -> throw "Illegal value in enter instruction"

dispatch Return _ (_:|as) _ _ = do
  if null as then
    throw "Empty stack in return"
  else
    putStack $ L.fromList as

dispatch Eval cs s@(a:|_) _ h = deployAddr h a >>= dispatchEval cs s h
dispatch Unwind cs s@(a:|_) _ h = deployAddr h a >>= dispatchUnwind cs s h

dispatch (Pack s t n) _ (a:|_) vs h = do
  let (s1, s2) = splitAt n vs
  putHeap $ H.update h a $ Seq $ Constr s t s1:s2

dispatch (Casejump ts) cs (a:|_) vs h =
  case vs of
    Constr _ t vs':vs'' -> do
      let cs' = snd $ ts!!(t-1) -- Tag is 1-based
      putCode $ cs'++cs
      putHeap $ H.update h a $ Seq $ vs'++vs''
    _ -> throw "No constructor for casejump!!"      

dispatch (Casefail cs' cs'') cs _ vs _ = do
  when (null vs) (throw "Null sequence in casefail")
  case head vs of
    Fail -> putCode $ cs'++cs
    _ -> putCode $ cs''++cs

dispatch Error _ _ _ _ = throw "Panic!!"

dispatch Add _ (a:|_) vs h =
  case vs of
    Num n1:Num n2:vs' -> putHeap $ H.update h a $ Seq $ Num(n1+n2):vs'
    _ -> throw "Operation add: bad operators"
dispatch Sqrt _ (a:|_) vs h =
  case vs of
    Num n:vs' -> let sq = floor $ sqrt (fromIntegral n :: Float)
                 in putHeap $ H.update h a $ Seq $ Num sq:vs'
    _ -> throw "Operation sqrt: bad operator"


peekCoord :: Stack
          -> H.Heap Node
          -> (Int, Int)
          -> MS Val
peekCoord s h (x, y) = do
  when (x >= L.length s) (throw "Illegal state in peek coord")
  let a = s L.!! x
  case H.lookup h a of
    Just (Seq vs) -> do
      when (y >= length vs) (throw "Illegal state in peek coord")
      return $ vs!!y
    _ -> throw "Illegal state in peek coord"


dispatchEval :: Code
             -> Stack
             -> H.Heap Node
             -> [Node]
             -> MS ()
dispatchEval _ _ _ [Seq (v:_)] | not (isPtr v) = return ()
dispatchEval _ _ _ [Seq (Ptr _:_), Def k _ _] | k > 0 = return ()
dispatchEval cs s h [Seq (Ptr a':_), Def 0 vs cs'] = do
  putCode $ cs'++[Unwind]++cs
  putStack $ a'<|s
  putHeap $ H.update h a' (Seq vs)
dispatchEval cs s _ (Seq (Ptr a':_):_) = do
  putCode $ Unwind:cs 
  putStack $ a'<|s
dispatchEval _ _ _ _ = throw "Illegal state in dispatch"


dispatchUnwind :: Code
               -> Stack
               -> H.Heap Node
               -> [Node]
               -> MS ()
dispatchUnwind _ (_:|a:as) h [Seq [v]]
  | not (isPtr v) =
      case H.lookup h a of
        Just (Seq (_:vs)) -> do
          putStack (a:|as)
          putHeap $ H.update h a (Seq $ v:vs)
        _ -> throw "Illegal value in unwind instruction"
dispatchUnwind cs s h [Seq (Ptr a':_), Def 0 vs' cs'] = do
  putCode $ cs'++[Unwind, Unwind]++cs
  putStack $ a'<|s
  putHeap $ H.update h a' (Seq vs')
dispatchUnwind cs (a:|_) h [Seq (Ptr _:vs), Def k vs' cs']
  | 0 < k && k <= length vs = do
      putCode $ cs'++[Unwind]++cs
      putHeap $ H.update h a $ (Seq $ vs'++vs)
dispatchUnwind _ (_:|a:as) h [Seq (Ptr a'':vs'), Def k _ _]
  | k > length vs' =
      case H.lookup h a of
        Just (Seq (_:vs)) -> do
          putStack $ a:|as
          putHeap $ H.update h a $ Seq $ Ptr a'':(vs'++vs)
        _ -> throw "Illegal value in unwind instruction"
dispatchUnwind cs s _ (Seq (Ptr a':_):_) = do
  putCode $ Unwind:Unwind:cs
  putStack $ a'<|s
dispatchUnwind _ _ _ _ = throw "Illegal state in unwind"


deployAddr :: H.Heap Node -> H.Addr -> MS [Node]
deployAddr h a =
  case H.lookup h a of
    Just n@(Seq (v:_)) ->
      case v of
        Ptr a' -> return [n, fromJust $ H.lookup h a']
        _      -> return [n]
    _ -> throw "Deploying address failure"

