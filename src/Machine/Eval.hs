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
  putCodeStack $ L.singleton [Pushglobal "main"]
  putAddrStack $ L.singleton a
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
  (css, a:|as, h) <- (,,) <$> getCodeStack <*> getAddrStack <*> getHeap
  case (css, as, H.lookup h a) of
    ([]:|[], [], Just (Seq [v])) -> return $ not $ isPtr v
    (_, _, Just (Seq _)) -> return False
    _ -> throw "Illegal state in final state check"


step :: MS ()
step = do
  incSteps
  (css, as@(a:|_), h) <- (,,) <$> getCodeStack <*> getAddrStack <*> getHeap
  case H.lookup h a of
    (Just (Seq vs)) -> dispatch css as vs h
    _ -> throw "Illegal state in step"


dispatch :: CodeStack
         -> AddrStack
         -> [Val]
         -> H.Heap Node
         -> MS ()
dispatch ((Pushnum i:cs):|css) (a:|_) vs h = do
  putCodeStack $ cs:|css
  putHeap $ H.update h a (Seq $ Num i:vs)
  
dispatch ((Pushch c:cs):|css) (a:|_) vs h = do
  putCodeStack $ cs:|css
  putHeap $ H.update h a (Seq $ Ch c:vs)

dispatch ((Pushfail:cs):|css) (a:|_) vs h = do
  putCodeStack $ cs:|css
  putHeap $ H.update h a (Seq $ Fail:vs)

dispatch ((Pushglobal s:cs):|css) (a:|_) vs h = do
  putCodeStack $ cs:|css
  g <- getGlobals
  case M.lookup s g of
    Just a' -> putHeap $ H.update h a (Seq $ Ptr a':vs)
    Nothing -> throw $ "No such global: " ++ s

dispatch ((Pushdef k xs cs':cs):|css) s@(a:|_) vs h = do
  putCodeStack $ cs:|css
  vs' <- traverse (peekCoord s h) xs
  let (h', a') = H.alloc h (Def k vs' cs')
  putHeap $ H.update h' a (Seq $ Ptr a':vs)

dispatch ((Pushval n m:cs):|css) as@(a:|_) vs h = do
  putCodeStack $ cs:|css
  v <- peekCoord as h (n, m)
  putHeap $ H.update h a $ Seq $ v:vs

dispatch ((Slide n m:cs):|css) (a:|_) vs h = do
  putCodeStack $ cs:|css
  let (s1, s2) = splitAt n vs
  putHeap $ H.update h a (Seq $ s1++drop m s2)

dispatch ((Alloc n:cs):|css) (a:|_) vs h = do
  putCodeStack $ cs:|css
  let (h', as') = H.allocn n h $ Seq []
  putHeap $ H.update h' a $ Seq $ map Ptr as'++vs

dispatch ((Enter n:cs):|css) as vs _ = do
  putCodeStack $ cs:|css
  when (n >= length vs) (throw "Illegal position in enter instruction")
  case vs!!n of
    Ptr a' -> putAddrStack $ a'<|as
    _ -> throw "Illegal value in enter instruction"

dispatch ((Return:cs):|css) (_:|as) _ _ = do
  putCodeStack $ cs:|css
  if null as then
    throw "Empty stack in return"
  else
    putAddrStack $ L.fromList as

dispatch ((Pack s t n:cs):|css) (a:|_) vs h = do
  putCodeStack $ cs:|css
  let (s1, s2) = splitAt n vs
  putHeap $ H.update h a $ Seq $ Constr s t s1:s2

dispatch ((Casejump ts:cs):|css) (a:|_) vs h =
  case vs of
    Constr _ t vs':vs'' -> do
      let cs' = snd $ ts!!(t-1) -- Tag is 1-based
      putCodeStack $ (cs'++cs):|css
      putHeap $ H.update h a $ Seq $ vs'++vs''
    _ -> throw "No constructor for casejump!!"      

dispatch ((Casefail cs' cs'':cs):|css) _ vs _ = do
  when (null vs) (throw "Null sequence in casefail")
  case head vs of
    Fail -> putCodeStack $ (cs'++cs):|css
    _ -> putCodeStack $ (cs''++cs):|css

dispatch ((Error:_):|_) _ _ _ = throw "Panic!!"

dispatch ((Add:cs):|css) (a:|_) vs h = do
  putCodeStack $ cs:|css
  case vs of
    Num n1:Num n2:vs' -> putHeap $ H.update h a $ Seq $ Num(n1+n2):vs'
    _ -> throw "Operation add: bad operators"

dispatch ((Sqrt:cs):|css) (a:|_) vs h = do
  putCodeStack $ cs:|css
  case vs of
    Num n:vs' -> let sq = floor $ sqrt (fromIntegral n :: Float)
                 in putHeap $ H.update h a $ Seq $ Num sq:vs'
    _ -> throw "Operation sqrt: bad operator"

dispatch cs@((Eval:_):|_) as@(a:|_) _ h = do
  deployed <- deployAddr h a
  dispatchEval cs as h deployed

dispatch cs@([]:|_) as@(a:|_) _ h = do
  deployed <- deployAddr h a
  dispatchUnwind cs as h deployed


 
dispatchEval :: CodeStack
             -> AddrStack
             -> H.Heap Node
             -> [Node]
             -> MS ()
dispatchEval ((_:cs):|css) _ _ [Seq (v:_)]
  | not (isPtr v) = putCodeStack $ cs:|css

dispatchEval ((_:cs):|css) _ _ [Seq (Ptr _:_), Def k _ _]
  | k > 0 = putCodeStack $ cs:|css

dispatchEval ((_:cs):|css) as h [Seq (Ptr a':_), Def 0 vs cs'] = do
  putCodeStack $ cs':|(cs:css)
  putAddrStack $ a'<|as
  putHeap $ H.update h a' (Seq vs)

dispatchEval ((_:cs):|css) as _ (Seq (Ptr a':_):_) = do
  putCodeStack $ []:|(cs:css)
  putAddrStack $ a'<|as

dispatchEval _ _ _ _ = throw "Illegal state in dispatch"



dispatchUnwind :: CodeStack
               -> AddrStack
               -> H.Heap Node
               -> [Node]
               -> MS ()
dispatchUnwind ([]:|(cs:css)) (_:|a:as) h [Seq [v]]
  | not (isPtr v) = do
      putCodeStack $ cs:|css
      case H.lookup h a of
        Just (Seq (_:vs)) -> do
          putAddrStack $ a:|as
          putHeap $ H.update h a (Seq $ v:vs)
        _ -> throw "Illegal value in unwind instruction"

dispatchUnwind ([]:|css) as h [Seq (Ptr a':_), Def 0 vs' cs'] = do
  putCodeStack $ cs':|([]:css)
  putAddrStack $ a'<|as
  putHeap $ H.update h a' (Seq vs')

dispatchUnwind ([]:|css) (a:|_) h [Seq (Ptr _:vs), Def k vs' cs']
  | 0 < k && k <= length vs = do
      putCodeStack $ cs':|css
      putHeap $ H.update h a $ (Seq $ vs'++vs)

dispatchUnwind ([]:|(cs:css)) (_:|a:as) h [Seq (Ptr a'':vs'), Def k _ _]
  | k > length vs' =
      case H.lookup h a of
        Just (Seq (_:vs)) -> do
          putCodeStack $ (cs:|css)
          putAddrStack $ a:|as
          putHeap $ H.update h a $ Seq $ Ptr a'':(vs'++vs)
        _ -> throw "Illegal value in unwind instruction"

dispatchUnwind css as _ (Seq (Ptr a':_):_) = do
  putCodeStack $ []<|css
  putAddrStack $ a'<|as

dispatchUnwind _ _ _ _ = throw "Illegal state in unwind"



peekCoord :: AddrStack
          -> H.Heap Node
          -> (Int, Int)
          -> MS Val
peekCoord as h (x, y) = do
  when (x >= L.length as) (throw "Illegal state in peek coord")
  let a = as L.!! x
  case H.lookup h a of
    Just (Seq vs) -> do
      when (y >= length vs) (throw "Illegal state in peek coord")
      return $ vs!!y
    _ -> throw "Illegal state in peek coord"



deployAddr :: H.Heap Node -> H.Addr -> MS [Node]
deployAddr h a =
  case H.lookup h a of
    Just n@(Seq (v:_)) ->
      case v of
        Ptr a' -> return [n, fromJust $ H.lookup h a']
        _      -> return [n]
    _ -> throw "Deploying address failure"
