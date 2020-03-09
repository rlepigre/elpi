(* elpi: embedded lambda prolog interpreter                                  *)
(* license: GNU Lesser General Public License Version 2.1 or later           *)
(* ------------------------------------------------------------------------- *)

open Elpi_util
open Elpi_lexer_config.Lexer_config

exception ParseError of Util.Loc.t * string

(* this is the input of the parser functor in grammar.mly, it ties the knot:
   accumulate requires to call the same parser on another file, but file/module
   resolution is not a parser business *)

module type ParseFile = sig
  val parse_file : ?cwd:string -> string -> (Digest.t * Ast.Program.decl list) list
end

let rec substrings i len_s s =
  if len_s - i >= 0 then
    String.sub s 0 i :: substrings (i+1) len_s s
  else []
let substrings s = substrings 1 (String.length s) s

let find_sub tab s =
  let rec aux = function
    | [] -> raise Not_found
    | x :: xs ->
        try Hashtbl.find tab x
        with Not_found -> aux xs
  in
    aux (substrings s)

let precedence_of, umax_precedence, appl_precedence, inf_precedence =
  let tab = Hashtbl.create 21 in
  List.iteri (fun legacy_level { tokens; fixity } ->
    List.iter (function
      | Extensible { start; _ } -> Hashtbl.add tab start (fixity,legacy_level)
      | Fixed { the_token; _ } -> Hashtbl.add tab the_token (fixity,legacy_level)
      ) tokens;
    ) mixfix_symbols;
  let umax_precedence = List.length mixfix_symbols in
  let appl_precedence = umax_precedence + 1 in
  let inf_precedence = appl_precedence + 1 in (* greater than any used precedence*)
  (fun s ->
    try find_sub tab s
    with Not_found -> Prefix,appl_precedence),
  umax_precedence, appl_precedence, inf_precedence

let comma_precedence = 1 + (snd @@ precedence_of ",")
let min_precedence = -1  (* minimal precedence in use *)
let lam_precedence = -1  (* precedence of lambda abstraction *)
let umin_precedence = 0   (* minimal user defined precedence *)

let pp_fixed fmt l =
  l |> List.iter (fun x -> Format.fprintf fmt "%s @ " x)

let pp_tok_list fmt l =
  Format.pp_open_hbox fmt ();
  List.iter (function
    | Extensible { start; fixed; _ } -> Format.fprintf fmt "%a%s.. @ " pp_fixed fixed start
    | Fixed { the_token; _ } -> Format.fprintf fmt "%s @ " the_token)
    l;
  Format.pp_close_box fmt ()

let legacy_parser_compat_error =
  let open Format in
  let b = Buffer.create 80 in
  let fmt = formatter_of_buffer b in
  fprintf fmt "@[<v>";
  fprintf fmt "%s@;" "Mixfix directives are not supported by this parser.";
  fprintf fmt "%s@;" "";
  fprintf fmt "%s@;" "The parser is based on token families.";
  fprintf fmt "%s@;" "A family is identified by some starting characters, for example";
  fprintf fmt "%s@;" "a token '+-->' belongs to the family of '+'. There is no need";
  fprintf fmt "%s@;" "to declare it.";
  fprintf fmt "%s@;" "";
  fprintf fmt "%s@;" "All the tokens of a family are parsed with the same precedence and";
  fprintf fmt "%s@;" "associativity, for example 'x +--> y *--> z' is parsed as";
  fprintf fmt "%s@;" "'x +--> (y *--> z)' since the family of '*' has higher precedence";
  fprintf fmt "%s@;" "than the family of '+'.";
  fprintf fmt "%s@;" "";
  fprintf fmt "%s@;" "Here the table of tokens and token families.";
  fprintf fmt "%s@;" "Token families are represented by the start symbols followed by '..'.";
  fprintf fmt "%s@;" "The listing is ordered by increasing precedence.";
  pp_open_tbox fmt ();
  pp_set_tab fmt ();
  fprintf fmt "%-25s  " "fixity";
  pp_set_tab fmt ();
  fprintf fmt "| %s" "tokens / token families";
  pp_print_tab fmt ();
  fprintf fmt "--------------------------";
  pp_print_tab fmt ();
  fprintf fmt "+ -----------------------------------";
  pp_print_tab fmt ();
  List.iter (fun { tokens; fixity; _ } ->
    fprintf fmt "%a" pp_fixity fixity;
    pp_print_tab fmt ();
    fprintf fmt "| @[<hov 2>%a@]" pp_tok_list tokens;
    pp_print_tab fmt ();
    ) mixfix_symbols;
  pp_close_tbox fmt ();
  fprintf fmt "@]";
  pp_print_flush fmt ();
  Buffer.contents b
;;

let error_mixfix loc =
  raise (ParseError(loc,legacy_parser_compat_error))
