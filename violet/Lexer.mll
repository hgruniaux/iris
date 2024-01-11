{
open Parser

exception Lexing_error of string

let string_buffer = Buffer.create 1024

let resolve_keyword =
  let keywords = Hashtbl.create 17 in
  List.iter (fun (s, l) -> Hashtbl.add keywords s l)
        (
        [
          ("true", TRUE);
          ("false", FALSE);
          ("new", NEW);
          ("delete", DELETE);
          ("print", PRINT);
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
  | '&' { AMP }
  | "&&" { AMP_AMP }
  | '^' { CARET }
  | '|' { PIPE }
  | "||" { PIPE_PIPE }
  | '<' { LESS }
  | "<=" { LESS_EQ }
  | '>' { GREATER }
  | ">=" { GREATER_EQ }
  | "=" { EQ }
  | "==" { EQ_EQ }
  | "!=" { EXCLAIM_EQ }
  | "<<" { LESS_LESS }
  | ">>" { GREATER_GREATER }
  | '(' { LPAR }
  | ')' { RPAR}
  | '{' { LBRACKET }
  | '}' { RBRACKET }
  | integer as i { INTEGER (Nativeint.of_string i) }
  | identifier as i { resolve_keyword i }

  | '"'
    {
      (* Calling (string lexbuf) overwrites the start position saved in lexbuf.
         Sadly, string is called many times when lexing a string literal.
         Therefore, the start position of the token is incorrect by default.
         To avoid this, we save manually the start position of the constant
         and then restore it after lexing it. *)
      let start_pos = lexbuf.lex_start_pos in
      let start_p = lexbuf.lex_start_p in
      let lexed_str = string lexbuf in
      lexbuf.lex_start_pos <- start_pos;
      lexbuf.lex_start_p <- start_p;
      STRING (lexed_str)
    }

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

and string = parse
  | eol | eof
    { raise (Lexing_error ("Unterminated string.")) }

  | '"'
    { let s = Buffer.contents string_buffer in
      Buffer.reset string_buffer;
      s }

  | "\\n"
    { Buffer.add_char string_buffer '\n';
      string lexbuf }

  | "\\\\"
    { Buffer.add_char string_buffer '\\';
      string lexbuf }

  | "\\\""
    { Buffer.add_char string_buffer '"';
      string lexbuf }

  | '\\' _ as c
    { raise (Lexing_error ("Invalid escape sequence '\\" ^ c ^ "' in string.")) }

  | _ as c
    { Buffer.add_char string_buffer c;
      string lexbuf }
