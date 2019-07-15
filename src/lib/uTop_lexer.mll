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
  open Lexing
  open UTop_token

  let mkloc idx1 idx2 ofs1 ofs2 = {
    idx1 = idx1;
    idx2 = idx2;
    ofs1 = ofs1;
    ofs2 = ofs2;
  }

  (* Only for ascii-only lexemes. *)
  let lexeme_loc idx lexbuf =
    let ofs1 = lexeme_start lexbuf and ofs2 = lexeme_end lexbuf in
    {
      idx1 = idx;
      idx2 = idx + (ofs2 - ofs1);
      ofs1 = ofs1;
      ofs2 = ofs2;
    }

  let _merge_loc l1 l2 = {
    idx1 = l1.idx1;
    idx2 = l2.idx2;
    ofs1 = l1.ofs1;
    ofs2 = l2.ofs2;
  }

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

rule tokens idx acc = parse
  | eof
      { (idx, None, List.rev acc) }
  | ('\n' | blank)+
      { let loc = lexeme_loc idx lexbuf in
        tokens loc.idx2 ((Blanks, loc) :: acc) lexbuf }
  | lident
      { let src = lexeme lexbuf in
        let loc = lexeme_loc idx lexbuf in
        let tok =
          match src with
            | ("true" | "false") ->
                Constant src
            | _ ->
                Lident src
        in
        tokens loc.idx2 ((tok, loc) :: acc) lexbuf }
  | uident
      { let src = lexeme lexbuf in
        let loc = lexeme_loc idx lexbuf in
        let tok = Uident src in
        tokens loc.idx2 ((tok, loc) :: acc) lexbuf }
  | int_literal "l"
  | int_literal "L"
  | int_literal "n"
  | int_literal
  | float_literal
      { let loc = lexeme_loc idx lexbuf in
        let tok = Constant (lexeme lexbuf) in
        tokens loc.idx2 ((tok, loc) :: acc) lexbuf }
  | '"'
      { let ofs = lexeme_start lexbuf in
        let item, idx2= cm_string (idx + 1) lexbuf in
        let loc = mkloc idx idx2 ofs (lexeme_end lexbuf) in
        tokens idx2 ((item, loc) :: acc) lexbuf }
  | '{' (lowercase* as tag) '|'
      { let ofs = lexeme_start lexbuf in
        let delim_len = String.length tag + 2 in
        let idx2, terminated = quoted_string (idx + delim_len) tag false lexbuf in
        let loc = mkloc idx idx2 ofs (lexeme_end lexbuf) in
        tokens idx2 ((String (delim_len, terminated), loc) :: acc) lexbuf }
  | "'" [^'\'' '\\'] "'"
  | "'\\" ['\\' '"' 'n' 't' 'b' 'r' ' ' '\'' 'x' '0'-'9'] eof
  | "'\\" ['\\' '"' 'n' 't' 'b' 'r' ' ' '\''] "'"
  | "'\\" (['0'-'9'] ['0'-'9'] | 'x' hexa_char) eof
  | "'\\" (['0'-'9'] ['0'-'9'] ['0'-'9'] | 'x' hexa_char hexa_char) eof
  | "'\\" (['0'-'9'] ['0'-'9'] ['0'-'9'] | 'x' hexa_char hexa_char) "'"
      { let loc = lexeme_loc idx lexbuf in
        tokens loc.idx2 ((Char, loc) :: acc) lexbuf }
  | "'\\" uchar
      { let loc = mkloc idx (idx + 3) (lexeme_start lexbuf) (lexeme_end lexbuf) in
        tokens loc.idx2 ((Error, loc) :: acc) lexbuf }
  | "(*)"
      { let loc = lexeme_loc idx lexbuf in
        tokens loc.idx2 ((Comment (Comment_reg, true), loc) :: acc) lexbuf }
  | "(**)"
      { let loc = lexeme_loc idx lexbuf in
        tokens loc.idx2 ((Comment (Comment_doc, true), loc) :: acc) lexbuf }
  | "(**"
      { let ofs = lexeme_start lexbuf in
        let idx2, terminated = comment (idx + 3) 0 false lexbuf in
        let loc = mkloc idx idx2 ofs (lexeme_end lexbuf) in
        tokens idx2 ((Comment (Comment_doc, terminated), loc) :: acc) lexbuf }
  | "(*"
      { let ofs = lexeme_start lexbuf in
        let idx2, terminated = comment (idx + 2) 0 false lexbuf in
        let loc = mkloc idx idx2 ofs (lexeme_end lexbuf) in
        tokens idx2 ((Comment (Comment_reg, terminated), loc) :: acc) lexbuf }
  | ""
      { symbol idx acc lexbuf }

and symbol idx acc = parse
  | "(" | ")"
  | "[" | "]"
  | "{" | "}"
  | "`"
  | "#"
  | ","
  | ";" | ";;"
  | symbolchar+
      { let loc = lexeme_loc idx lexbuf in
        let tok = Symbol (lexeme lexbuf) in
        tokens loc.idx2 ((tok, loc) :: acc) lexbuf }
  | uchar as uchar
      { let uChar= Zed_utf8.unsafe_extract uchar 0 in
        if Zed_char.is_combining_mark uChar then
          let tok, loc= List.hd acc
          and tl= List.tl acc in
          let tok= match tok with
            | Symbol str-> Symbol (str ^ (lexeme lexbuf))
            | Lident str-> Lident (str ^ (lexeme lexbuf))
            | Uident str-> Uident (str ^ (lexeme lexbuf))
            | Constant str-> Constant (str ^ (lexeme lexbuf))
            | _-> tok
          in
          let loc= { loc with ofs2= lexeme_end lexbuf } in
          tokens loc.idx2 ((tok, loc) :: tl) lexbuf
        else
          let loc = mkloc idx (idx + 1) (lexeme_start lexbuf) (lexeme_end lexbuf) in
          tokens loc.idx2 ((Error, loc) :: acc) lexbuf
      }

and cm_string idx= parse
  | '"'
      { (String (1, true), idx+1) }
  | "\\\""
      { let idx2, terminated= string (idx + 2) false lexbuf in
        (String (1, terminated), idx2)
      }
  | uchar as uchar
      {
        let uChar= Zed_utf8.unsafe_extract uchar 0 in
        if Zed_char.is_combining_mark uChar then
          cm_string idx lexbuf
        else
          let idx2, terminated= string (idx + 1) true lexbuf in
          (String (1, terminated), idx2)
      }
  | eof
      { (String (1, false), idx) }

and comment idx depth combining= parse
  | "(*"
      { comment (idx + 2) (depth + 1) false lexbuf }
  | "*)"
      { if depth = 0 then
          (idx + 2, true)
        else
          comment (idx + 2) (depth - 1) false lexbuf }
  | '"'
      { let idx, terminated = string (idx + 1) false lexbuf in
        if terminated then
          comment idx depth false lexbuf
        else
          (idx, false) }
  | uchar as uchar
      { let uChar= Zed_utf8.unsafe_extract uchar 0 in
        if not combining then
          if Zed_char.is_combining_mark uChar then
            comment (idx + 1) depth false lexbuf
          else
            comment (idx + 1) depth true lexbuf
        else
          if Zed_char.is_combining_mark uChar then
            comment idx depth true lexbuf
          else
            comment (idx + 1) depth true lexbuf
      }
  | eof
      { (idx, false) }

and string idx combining= parse
  | '"'
      { (idx + 1, true) }
  | "\\\""
      { string (idx + 2) false lexbuf }
  | uchar as uchar
      { let uChar= Zed_utf8.unsafe_extract uchar 0 in
        if not combining then
          if Zed_char.is_combining_mark uChar then
            string (idx + 1) false lexbuf
          else
            string (idx + 1) true lexbuf
        else
          if Zed_char.is_combining_mark uChar then
            string idx true lexbuf
          else
            string (idx + 1) true lexbuf
      }
  | eof
      { (idx, false) }

and quoted_string idx tag combining= parse
    | '|' (lowercase* as tag2) '}'
        { let idx = idx + 2 + String.length tag2 in
          if tag = tag2 then
            (idx, true)
          else
            quoted_string idx tag false lexbuf }
    | eof
        { (idx, false) }
    | uchar as uchar
        { let uChar= Zed_utf8.unsafe_extract uchar 0 in
          if not combining then
            if Zed_char.is_combining_mark uChar then
              quoted_string (idx + 1) tag false lexbuf
            else
              quoted_string (idx + 1) tag true lexbuf
          else
            if Zed_char.is_combining_mark uChar then
              quoted_string idx tag true lexbuf
            else
              quoted_string (idx + 1) tag true lexbuf
        }

{
  let lex_string str =
    let _, _, items = tokens 0 [] (Lexing.from_string str) in
    items
}
