------------------------------------------------------------
-- State.hs
-- Estado de la maquina interprete
------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Machine.State
  ( Stack,
    Globals,
    State(..),
    MS,
    runMS,
    throw,
    try,
    traceIO,
    getCode,
    getStack,
    getHeap,
    getGlobals,
    getSteps,
    putCode,
    putStack,
    putHeap,
    putGlobals,
    incSteps,
    prettyState,
    traceState,
  )
  where

import Control.Monad.IO.Class(liftIO)
import Control.Monad.Trans.Class(lift)
import Control.Monad.Trans.Except(runExceptT, ExceptT, throwE, catchE)
import Control.Monad.Trans.State (StateT, evalStateT, get, put)
import qualified Data.List.NonEmpty as L
import qualified Data.Map as M
import Data.Maybe (fromJust)

import Machine.Code
import qualified Machine.Heap as H
import Machine.Node
import qualified Util.Pretty as P

type Stack = L.NonEmpty H.Addr
type Globals = M.Map String H.Addr

-- State
data State = State
  { code :: Code,
    stack :: Stack,
    heap :: H.Heap Node,
    globals :: Globals,
    steps :: Int
  }

emptyState :: State
emptyState = State
  { code=undefined,
    stack=undefined,
    heap=undefined,
    globals=undefined,
    steps=0
  }

type MS = ExceptT String (StateT State IO)

runMS :: MS () -> IO (Either String ())
runMS r = evalStateT (runExceptT r) emptyState

-- errors
throw :: String -> MS a
throw = throwE

try :: MS a -> (String -> MS a) -> MS a
try = catchE

-- logs
traceIO :: String -> MS ()
traceIO = liftIO . putStrLn


-- accessing the state
getCode :: MS Code
getCode = code <$> lift get

getStack :: MS Stack
getStack = stack <$> lift get

getHeap :: MS (H.Heap Node)
getHeap = heap <$> lift get

getGlobals :: MS Globals
getGlobals = globals <$> lift get

getSteps :: MS Int
getSteps = steps <$> lift get

-- modifying the state
putCode :: Code -> MS ()
putCode c = lift get >>= \s -> lift (put s{code = c})

putStack :: Stack -> MS ()
putStack st = lift get >>= \s -> lift (put s{stack = st})

putHeap :: H.Heap Node -> MS ()
putHeap h = lift get >>= \s -> lift (put s{heap = h})

putGlobals :: Globals -> MS ()
putGlobals g = lift get >>= \s -> lift (put s{globals = g})

incSteps :: MS ()
incSteps = lift get >>= \s -> lift (put s{steps = steps s+1})


-------------
-- Pretty

instance P.Buildable State where
  build State{code=cs, stack=as, heap=h, steps=s} =
    P.nameF "<<state>>"
      $ P.nameF "code" (P.listF cs)
      <> P.nameF "stack" (P.listF $ map P.pairMapF nodes)
      <> P.nameF "heap size" (P.build $ H.size h)
      <> P.nameF "steps" (P.build s)
    where
      nodes = [(a, fromJust $ H.lookup h a) | a <- L.toList as]

prettyState :: State -> String
prettyState = P.fmt . P.build

traceState :: MS ()
traceState = lift get >>= traceIO . prettyState
