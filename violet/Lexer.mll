{
open Parser

exception Lexing_error of string

let resolve_keyword =
  let keywords = Hashtbl.create 17 in
  List.iter (fun (s, l) -> Hashtbl.add keywords s l)
        (
        [
          ("fn", FN);
          ("let", LET);
          ("return", RETURN);
          ("if", IF);
          ("else", ELSE);
          ("while", WHILE);
          ("loop", LOOP);
          ("break", BREAK);
          ("continue", CONTINUE);
          ("goto", GOTO);
        ]
        );
  fun s ->
    try Hashtbl.find keywords s
    with Not_found -> IDENT s
}

let eol = '\n' | '\r' '\n' | '\r'
let digit = ['0'-'9']
let lower = ['a'-'z'] | '_'
let upper = ['A'-'Z']
let dec_integer = '0' | ['1'-'9'] digit*
let bin_integer = "0b" ('0' | '1')+
let hex_integer = "0x" (['0'-'9' 'a'-'f' 'A'-'F' ])+
let integer = dec_integer | bin_integer | hex_integer
let identifier = (upper | lower) (lower | upper | digit)*

rule next_token = parse
  | eol { Lexing.new_line lexbuf; next_token lexbuf }
  | ' ' | '\t' { next_token lexbuf }
  | eof { EOF }
  | "//" { line_comment lexbuf }
  | "/*" { block_comment lexbuf }
  | ',' { COMMA }
  | ';' { SEMI }
  | ':' { COLON }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { STAR }
  | '/' { SLASH }
  | '%' { PERCENT }
  | '(' { LPAR }
  | ')' { RPAR}
  | '{' { LBRACKET }
  | '}' { RBRACKET }
  | integer as i { INTEGER (Nativeint.of_string i) }
  | identifier as i { resolve_keyword i }

  | ['\x20'-'\x7E'] as c
    {
      let msg = Format.sprintf "Illegal character '%c'in code." c in
      raise (Lexing_error msg)
    }

  | _ as c
    {
      let msg = Format.sprintf "Illegal character '\\x%x'in code." (Char.code c) in
      raise (Lexing_error msg)
    }

and line_comment = parse
  | eol { Lexing.new_line lexbuf; next_token lexbuf }
  | eof { EOF }
  | _ { line_comment lexbuf }

and block_comment = parse
  | eol { Lexing.new_line lexbuf; block_comment lexbuf }
  | eof { raise (Lexing_error "Unterminated block comment.") }
  | "*/" { next_token lexbuf }
  | _ { block_comment lexbuf }
