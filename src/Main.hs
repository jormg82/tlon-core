-----------------------------------------
-- Main.hs
-----------------------------------------

module Main where

import Compiler.CoreToCode
import Core.Core
import Core.Parser
import Core.Scanner
import Machine.Code(prettyCodeDic)
import Machine.Eval
import Params.Params (Params (..), cmdLineParser)


main :: IO ()
main = do
  params <- cmdLineParser
  text <- readFile $ file params

  let tokens = alexScanTokens text
  putStrLn "\n[+] Tokens:"
  print tokens

  let prog = parseFile tokens
  putStrLn $ "\n" ++ prettyProg prog

  let dic = coreToCode prog
  putStrLn $ prettyCodeDic dic

  putStrLn "\n[+] Evaluation:"
  eval dic
