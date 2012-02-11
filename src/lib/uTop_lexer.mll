(*
 * uTop_lexer.mll
 * --------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of utop.
 *)

(* Lexer for the OCaml language. *)

{
  open UTop_token
}

let uchar = ['\x00' - '\x7f'] | _ [ '\x80' - '\xbf' ]*

let blank = [' ' '\009' '\012']
let lowercase = ['a'-'z' '_']
let uppercase = ['A'-'Z']
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']
let lident = lowercase identchar*
let uident = uppercase identchar*
let ident = (lowercase|uppercase) identchar*

let hexa_char = ['0'-'9' 'A'-'F' 'a'-'f']
let decimal_literal =
  ['0'-'9'] ['0'-'9' '_']*
let hex_literal =
  '0' ['x' 'X'] hexa_char ['0'-'9' 'A'-'F' 'a'-'f' '_']*
let oct_literal =
  '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
let bin_literal =
  '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*
let int_literal =
  decimal_literal | hex_literal | oct_literal | bin_literal
let float_literal =
  ['0'-'9'] ['0'-'9' '_']*
  ('.' ['0'-'9' '_']* )?
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)?

let symbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']

rule token fallback = parse
  | ('\n' | blank)+
      { Blanks }
  | "true"
      { Constant }
  | "false"
      { Constant }
  | lident
      { Lident }
  | uident
      { Uident }
  | int_literal "l"
      { Constant }
  | int_literal "L"
      { Constant }
  | int_literal "n"
      { Constant }
  | int_literal
      { Constant }
  | float_literal
      { Constant }
  | '"'
      { String (string lexbuf) }
  | "'" [^'\'' '\\'] "'"
      { Char }
  | "'\\" ['\\' '"' 'n' 't' 'b' 'r' ' ' '\'' 'x' '0'-'9'] eof
      { Char }
  | "'\\" ['\\' '"' 'n' 't' 'b' 'r' ' ' '\''] "'"
      { Char }
  | "'\\" (['0'-'9'] ['0'-'9'] | 'x' hexa_char) eof
      { Char }
  | "'\\" (['0'-'9'] ['0'-'9'] ['0'-'9'] | 'x' hexa_char hexa_char) eof
      { Char }
  | "'\\" (['0'-'9'] ['0'-'9'] ['0'-'9'] | 'x' hexa_char hexa_char) "'"
      { Char }
  | "'\\" uchar
      { Error }
  | "(**"
      { Doc (comment 0 lexbuf) }
  | "(*"
      { Comment (comment 0 lexbuf) }
  | ""
      { fallback lexbuf }

and token_fallback = parse
  | "(" | ")"
  | "[" | "]"
  | "{" | "}"
  | "`"
  | "#"
  | ","
  | ";" | ";;"
  | symbolchar+
      { Symbol }
  | uchar
      { Error }
  | eof
      { raise End_of_file }

and token_fallback_camlp4 = parse
  | '<' (':' ident)? ('@' lident)? '<'
      { Quotation (quotation lexbuf) }
  | ""
      { token_fallback lexbuf }

and comment depth = parse
  | "(*"
      { comment (depth + 1) lexbuf }
  | "*)"
      { if depth > 0 then comment (depth - 1) lexbuf else true }
  | '"'
      { string lexbuf && comment depth lexbuf }
  | uchar
      { comment depth lexbuf }
  | eof
      { false }

and string = parse
  | '"'
      { true }
  | "\\\""
      { string lexbuf }
  | uchar
      { string lexbuf }
  | eof
      { false }

and quotation = parse
  | ">>"
      { true }
  | '$'
      { antiquotation lexbuf }
  | uchar
      { quotation lexbuf }
  | eof
      { false }

and antiquotation = parse
  | '$'
      { quotation lexbuf }
  | eof
      { false }
  | ""
      { ignore (token token_fallback_camlp4 lexbuf); antiquotation lexbuf }

{
  let lex_string ?(camlp4=false) str =
    let fallback = if camlp4 then token_fallback_camlp4 else token_fallback in
    let lexbuf = Lexing.from_string str in
    let rec loop idx ofs_a =
      match try Some (token fallback lexbuf) with End_of_file -> None with
        | Some token ->
            let ofs_b = Lexing.lexeme_end lexbuf in
            let src = String.sub str ofs_a (ofs_b - ofs_a) in
            let idx' = idx + Zed_utf8.length src in
            (token, idx, idx', src) :: loop idx' ofs_b
        | None ->
            []
    in
    loop 0 0
}
