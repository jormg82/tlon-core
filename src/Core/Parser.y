--------------------------------------
-- Parser.y
-- Syntactic analyzer for Core
--------------------------------------

{

module Core.Parser(parseFile) where

import Data.Maybe (fromMaybe, maybeToList)

import Core.Core
import qualified Core.Scanner as S

}

%name       parseFile
%tokentype  {S.Token}
%error      {parseFileError}


%token
  ';'       {S.Semicolon}
  '('       {S.LParen}
  ')'       {S.RParen}
  '{'       {S.LBrace}
  '}'       {S.RBrace}
  '='       {S.Equal}
  '->'      {S.Arrow}
  '[|]'     {S.FBar}
  '<'       {S.LThan}
  '>'       {S.GThan}
  'let'     {S.Let}
  'letrec'  {S.Letrec}
  'in'      {S.In}
  'case'    {S.Case}
  'of'      {S.Of}
  ID        {S.Var $$}
  INT       {S.Num $$}
  CHAR      {S.Ch $$}



%%

-- expression

program :: {Prog}
  : def_semis {Prog $1}

def_semis :: {[Def]}
  : def_semis ';' def {$1 ++ maybeToList $3}
  | def               {maybeToList $1}

def :: {Maybe Def}
  : ID '=' exp  {Just $ Def $1 $3}
  |             {Nothing}

exp :: {Exp}
  : exp_let    {$1}
  | exp_letrec {$1}
  | exp_fbar   {$1}
  | exp_case   {$1}
  | exp_ap     {$1}


exp_let :: {Exp}
  : 'let' def_semis 'in' exp
      {case $2 of
         [] -> $4
         ds@(_:_) -> ELet nonRecursive ds $4}

exp_letrec :: {Exp}
  : 'letrec' def_semis 'in' exp
      {case $2 of
         [] -> $4
         ds@(_:_) -> ELet recursive ds $4}

exp_fbar :: {Exp}
  : exp '[|]' exp {EFBar $1 $3}

exp_case  :: {Exp}
  : 'case' exp 'of' '{' alt_semis '}' {ECase $2 $5}

exp_ap :: {Exp}
  : exp_atom        {$1}
  | exp_ap exp_atom {EAp $1 $2}

exp_atom :: {Exp}
  : INT                   {ELit (LInt $1)}
  | CHAR                  {ELit (LChar $1)}
  | ID                    {EVar $1}
  | '{' ids '->' exp '}'  {ELam (reverse $2) $4}
  | '(' exp ')'           {$2}

ids :: {[String]}
  : ID {[$1]}
  | ids ID {$2:$1}

alt :: {Maybe Alt}
  : '<' INT '>' ids '->' exp {let is = reverse $4
                              in Just $ Alt $2 (head is) (tail is) $6}
  |                          {Nothing}

alt_semis :: {[Alt]}
  : alt_semis ';' alt {$1 ++ maybeToList $3}
  | alt               {maybeToList $1}


{

parseFileError :: [S.Token] -> a
parseFileError _ = error "Parse error"

}
