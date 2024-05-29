----------------------------
-- Scanner.x
-- Core lexical analizer
----------------------------

{

module Core.Scanner
  ( Token(..),
    alexScanTokens
  )
where

}


%wrapper "basic"


$digit = [0-9]    -- digitos
$alpha = [a-zA-Z] -- caracteres alfabeticos

$char = [' '-'~'] -- caracteres para tipo char

@number = $digit+
-- Los identificadores de Core pueden llevar '.' y '$' en su
-- interior y empezar con _
@id = ($alpha | _) ($alpha | $digit | _ | ' | \$ | \.)*
@char = ' $char '


tokens :-

  $white+   ;
  "//".*    ;

  ";"       {\_ -> Semicolon}
  "("       {\_ -> LParen}
  ")"       {\_ -> RParen}
  "{"       {\_ -> LBrace}
  "}"       {\_ -> RBrace}

  "="       {\_ -> Equal}
  "->"      {\_ -> Arrow}
  "[|]"     {\_ -> FBar}
  "<"       {\_ -> LThan}
  ">"       {\_ -> GThan}

  let       {\_ -> Let}
  letrec    {\_ -> Letrec}
  in        {\_ -> In}
  case      {\_ -> Case}
  of        {\_ -> Of}

  @id       {Var}
  @number   {Num . read}
  @char     {Ch . (!!1)}


{

data Token
  = Semicolon
  | LParen
  | RParen
  | LBrace
  | RBrace
  | Equal
  | Arrow
  | FBar
  | LThan
  | GThan
  | Let
  | Letrec
  | In
  | Case
  | Of
  | Var String
  | Num Int
  | Ch Char
  deriving (Eq,Show)

}
