(* elpi: embedded lambda prolog interpreter                                  *)
(* license: GNU Lesser General Public License Version 2.1 or later           *)
(* ------------------------------------------------------------------------- *)

(* extensible syntax *)
type fixity = Infixl | Infixr | Infix | Prefix | Postfix

let pp_fixity fmt = function
 | Infixl  -> Format.fprintf fmt "Infix left associative"
 | Infixr  -> Format.fprintf fmt "Infix right associative"
 | Infix   -> Format.fprintf fmt "Infix not associative"
 | Prefix  -> Format.fprintf fmt "Prefix not associative"
 | Postfix -> Format.fprintf fmt "Postfix not associative"

let fixity_of_string = function
 | "infixl" -> Infixl 
 | "infixr" -> Infixr 
 | "infix" -> Infix 
 | "prefix" -> Prefix 
 | "prefixr" -> Prefix
 | "postfix" -> Postfix 
 | "postfixl" -> Postfix
 | _ -> assert false

(* family of tokens *)

type extensible = {
  start : string;
  mk_token : string -> Tokens.token;
  token : string;
  non_enclosed : bool;
  at_least_one_char : bool;
  fixed : string list;
}

type fixed = {
  token : string;
  the_token : string;
  mk_token : Tokens.token;
}

type mixfix_kind = Fixed of fixed | Extensible of extensible

type mixfix = {
  tokens : mixfix_kind list;
  fixity : fixity;
  legacy_level : int;
}

let mkFix token the_token mk_token = Fixed { token; the_token; mk_token }

let mkExt token start ?(non_enclosed=false) ?(at_least_one_char=false) ?(fixed=[]) mk_token =
  Extensible { start; mk_token; token; non_enclosed; at_least_one_char; fixed }

let mixfix_symbols : mixfix list = [
  { tokens = [ mkFix "VDASH" ":-" VDASH;
               mkFix "QDASH" "?-" QDASH];
    fixity = Infix; legacy_level = 1  };
  { tokens = [ mkFix "OR" ";" OR];
    fixity = Infixr; legacy_level = 100  };
  { tokens = [ mkFix "CONJ" "," CONJ];
    fixity = Infixr; legacy_level = 110  };
  { tokens = [ mkFix "ARROW" "->" ARROW];
    fixity = Infixr; legacy_level = 116  };
  { tokens = [ mkFix "DARROW" "=>" DARROW];
    fixity = Infixr; legacy_level = 129  };
  { tokens = [ mkFix "EQ" "=" EQ;
               mkFix "EQ2" "==" EQ2;
               mkExt "SYMB_LT" "<" ~fixed:["=<"; "r<"; "i<"; "s<"; "r=<"; "i=<"; "s=<"] (fun x -> SYMB_LT x);
               mkExt "SYMB_GT" ">" ~fixed:["r>"; "i>"; "s>"; "r>="; "i>="; "s>="] (fun x -> SYMB_GT x);
               mkFix "IS" "is" IS;
               ];
    fixity = Infix; legacy_level = 130 };
  { tokens = [ mkFix "CONS" "::" CONS];
    fixity = Infixr; legacy_level = 140 };
  { tokens = [ mkExt "SYMB_TICK" "'" ~non_enclosed:true (fun x -> SYMB_TICK x) ];
    fixity = Infix; legacy_level = 0 };
  { tokens = [ mkExt "SYMB_EXP" "^" (fun x -> SYMB_EXP x);
               mkExt "SYMB_PLUS" "+" ~fixed:["r+";"i+";"s+"] (fun x -> SYMB_PLUS x);
               mkFix "MINUS" "-" MINUS;
               mkFix "MINUSr" "r-" MINUSr;
               mkFix "MINUSi" "i-" MINUSi;
               mkFix "MINUSs" "s-" MINUSs; ];
    fixity = Infixl; legacy_level = 150 };
  { tokens = [ mkExt "SYMB_TIMES" "*" ~fixed:["r*";"i*";"s*"] (fun x -> SYMB_TIMES x);
              mkExt "SYMB_SLASH" "/" (fun x -> SYMB_SLASH x);
              mkFix "DIV" "div" DIV;
              mkFix "MOD" "mod" MOD; ];
    fixity = Infixl; legacy_level = 160 };
  { tokens = [ mkExt "SYMB_MINUS" "--" (fun x -> SYMB_MINUS x) ];
    fixity = Infixr; legacy_level = 0 };
  { tokens = [ mkExt "SYMB_BTICK" "`" ~non_enclosed:true (fun x -> SYMB_BTICK x) ];
    fixity = Infix; legacy_level = 141 };
  { tokens = [ mkExt "SYMB_EQ" "==" ~at_least_one_char:true (fun x -> SYMB_EQ x) ];
    fixity = Infixr; legacy_level = 0 };
  { tokens = [ mkExt "SYMB_OR" "||" (fun x -> SYMB_OR x) ];
    fixity = Infixr; legacy_level = 0 };
  { tokens = [ mkExt "SYMB_AND" "&" (fun x -> SYMB_AND x) ];
    fixity = Infixr; legacy_level = 128 };
  { tokens = [ mkExt "SYMB_SHARP" "#" (fun x -> SYMB_SHARP x) ];
    fixity = Infixl; legacy_level = 200 };
  { tokens = [ mkExt "SYMB_TILDE" "~" ~fixed:["r~"; "i~"] (fun x -> SYMB_TILDE x) ];
    fixity = Prefix; legacy_level = 256 };
  { tokens = [ mkExt "SYMB_QMARK" "?" (fun x -> SYMB_QMARK x) ];
    fixity = Postfix; legacy_level = 300 };
]
;;

