{
open Parser
open Context

exception Error of string
let error msg = raise (Error msg)

let string_buffer = Buffer.create 1024

let resolve_keyword =
  let keywords = Hashtbl.create 17 in
  List.iter (fun (s, l) -> Hashtbl.add keywords s l)
    ([
      ("alignas", ALIGNAS);
      ("alignof", ALIGNOF);
      ("auto", AUTO);
      ("bool", BOOL);
      ("break", BREAK);
      ("_Alignas", ALIGNAS);
      ("_Alignof", ALIGNOF);
      ("_Bool", BOOL);
      ("case", CASE);
      ("char", CHAR);
      ("const", CONST);
      ("continue", CONTINUE);
      ("default", DEFAULT);
      ("do", DO);
      ("double", DOUBLE);
      ("else", ELSE);
      ("enum", ENUM);
      ("extern", EXTERN);
      ("false", FALSE);
      ("float", FLOAT);
      ("for", FOR);
      ("goto", GOTO);
      ("if", IF);
      ("inline", INLINE);
      ("int", INT);
      ("long", LONG);
      ("nullptr", NULLPTR);
      ("register", REGISTER);
      ("restrict", RESTRICT);
      ("return", RETURN);
      ("short", SHORT);
      ("signed", SIGNED);
      ("sizeof", SIZEOF);
      ("static", STATIC);
      ("static_assert", STATIC_ASSERT);
      ("struct", STRUCT);
      ("switch", SWITCH);
      ("thread_local", THREAD_LOCAL);
      ("true", TRUE);
      ("typedef", TYPEDEF);
      ("typeof", TYPEOF);
      ("typeof_unqual", TYPEOF_UNQUAL);
      ("union", UNION);
      ("unsigned", UNSIGNED);
      ("void", VOID);
      ("volatile", VOLATILE);
      ("while", WHILE);
      ("_Noreturn", NORETURN);
      ("_Bool", BOOL);
      ("_Alignas", ALIGNAS);
      ("_Alignof", ALIGNOF);
      ("_Static_assert", STATIC_ASSERT);
      ("_Thread_local", THREAD_LOCAL);

      ("__extension__", GNU_EXTENSION);
      ("__attribute", GNU_ATTRIBUTE);
      ("__attribute__", GNU_ATTRIBUTE);

      ("__alignof", ALIGNOF);
      ("__alignof__", ALIGNOF);
      ("__auto_type", AUTO);
      ("__bool", BOOL);
      ("__null", NULLPTR);
      ("__restrict", RESTRICT);
      ("__restrict__", RESTRICT);
      ("__volatile", VOLATILE);
      ("__volatile__", VOLATILE);
      ("__inline", INLINE);
      ("__inline__", INLINE);
      ("__typeof", TYPEOF);
      ("__typeof__", TYPEOF);
      ("__thread", THREAD_LOCAL);
      ("__signed", SIGNED);
      ("__signed__", SIGNED);
    ]);
  fun s ->
    try Hashtbl.find keywords s
    with Not_found -> IDENT s

  let parse_integer s =
    let without_sep = String.concat "" (String.split_on_char '\'' s) in
    let length = String.length without_sep in
    let is_digit c = c >= '0' && c <= '9' in
    let is_octal = (length > 0 && String.get without_sep 0 = '0') && (length == 1 || is_digit (String.get without_sep 1)) in
    if is_octal then
      Z.of_string ("0o" ^ without_sep)
    else
      Z.of_string without_sep

  let parse_int_suffix = function
  | None -> Ast.Pint_suffix_none
  | Some s -> (
      match String.lowercase_ascii s with
      | "u" -> Ast.Pint_suffix_unsigned
      | "l" -> Ast.Pint_suffix_long
      | "ul" | "lu" -> Ast.Pint_suffix_unsigned_long
      | "ll" -> Ast.Pint_suffix_long_long
      | "ull" | "llu" -> Ast.Pint_suffix_unsigned_long_long
      | _ -> assert false)

  let parse_float s =
    let without_sep = String.concat "" (String.split_on_char '\'' s) in
    float_of_string without_sep

  let parse_float_suffix = function
  | None -> Ast.Pfloat_suffix_none
  | Some s -> (
      match s with
      | 'f' | 'F' -> Ast.Pfloat_suffix_float
      | 'l' | 'L' -> Ast.Pfloat_suffix_long_double
      | _ -> assert false)

  let illegal_character c =
    if Char.code c >= 32 && Char.code c <= 126 then
      let msg = Format.sprintf "Illegal character '%c' in code." c in
      error msg
    else
      let msg = Format.sprintf "Illegal character '\\x%x' in code." (Char.code c) in
      error msg
}

let eol = '\n' | '\r' '\n' | '\r'
let whitespace = ' ' | '\t'

let digit_sep = '\''

(* C23 6.4.4.2 Integer constants *)
let bin_digit = '0' | '1'
let oct_digit = ['0'-'7']
let dec_digit = ['0'-'9']
let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']
let dec_integer = ['1'-'9'] (digit_sep? dec_digit)*
let oct_integer = '0' (digit_sep? oct_digit)*
let bin_integer = '0' ('b' | 'B') bin_digit (digit_sep? bin_digit)*
let hex_integer = '0' ('x' | 'X') hex_digit (digit_sep? hex_digit)*
let long_suffix = 'l' | 'L'
let long_long_suffix = "ll" | "LL"
let unsigned_suffix = 'u' | 'U'
let integer_suffix =
    unsigned_suffix long_suffix?
    | unsigned_suffix long_long_suffix
    | long_suffix unsigned_suffix?
    | long_long_suffix unsigned_suffix?
let integer = bin_integer | oct_integer | dec_integer | hex_integer

(* C23 6.4.4.3 Floating constants *)
let dec_digit_seq = dec_digit (digit_sep? dec_digit)*
let hex_digit_seq = hex_digit (digit_sep? hex_digit)*
let fractional_constant = dec_digit_seq? '.' dec_digit_seq | dec_digit_seq '.'
let exponent_part = ('e' | 'E') ('+' | '-')? dec_digit_seq
let hex_fractional_constant = hex_digit_seq? '.' hex_digit_seq | hex_digit_seq '.'
let binary_exponent_part = ('p' | 'P') ('+' | '-')? dec_digit_seq
let dec_float = (dec_digit_seq exponent_part) | (fractional_constant exponent_part?)
let hex_float = '0' ('x' | 'X') (hex_fractional_constant | hex_digit_seq) binary_exponent_part
let float = dec_float | hex_float
let float_suffix = 'f' | 'F' | 'l' | 'L'

let identifier_start = ['a'-'z' 'A'-'Z' '$' '_']
let identifier_cont = identifier_start | ['0'-'9']
let identifier = identifier_start identifier_cont*

rule next_token = parse
  | eof { EOF }
  | eol { Lexing.new_line lexbuf; next_token lexbuf }
  | whitespace+ { next_token lexbuf }
  | "//" { line_comment lexbuf }
  | "/*" { block_comment lexbuf }
  | '.' { DOT }
  | "..." { ELLIPSIS }
  | "->" { ARROW }
  | ',' { COMMA }
  | ';' { SEMI }
  | ':' { COLON }
  | '?' { QUESTION }
  | '+' { PLUS }
  | "+=" { PLUS_EQ }
  | "++" { PLUS_PLUS }
  | '-' { MINUS }
  | "-=" { MINUS_EQ }
  | "--" { MINUS_MINUS }
  | '*' { STAR }
  | "*=" { STAR_EQ }
  | '/' { SLASH }
  | "/=" { SLASH_EQ }
  | '%' { PERCENT }
  | "%=" { PERCENT_EQ }
  | '!' { EXCLAIM }
  | '~' { TILDE }
  | '&' { AMP }
  | "&=" { AMP_EQ }
  | "&&" { AMP_AMP }
  | '^' { CARET }
  | "^=" { CARET_EQ }
  | '|' { PIPE }
  | "|=" { PIPE_EQ }
  | "||" { PIPE_PIPE }
  | '<' { LESS }
  | "<=" { LESS_EQ }
  | '>' { GREATER }
  | ">=" { GREATER_EQ }
  | "=" { EQ }
  | "==" { EQ_EQ }
  | "!=" { EXCLAIM_EQ }
  | "<<" { LESS_LESS }
  | "<<=" { LESS_LESS_EQ }
  | ">>" { GREATER_GREATER }
  | ">>=" { GREATER_GREATER_EQ }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '{' { LBRACKET }
  | '}' { RBRACKET }
  | '[' { LSQUARE }
  | ']' { RSQUARE }
  | identifier as i { resolve_keyword i }

  | (integer as i) (integer_suffix as s)?
    {
      let suffix = parse_int_suffix s in
      INTEGER_LITERAL (parse_integer i, suffix)
    }

  | (float as f) (float_suffix as s)?
    {
      let suffix = parse_float_suffix s in
      FLOAT_LITERAL (parse_float f, suffix)
    }

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
      STRING_LITERAL (lexed_str)
    }

  | _ as c { illegal_character c }

and line_comment = parse
  | eol { Lexing.new_line lexbuf; next_token lexbuf }
  | eof { EOF }
  | _ { line_comment lexbuf }

and block_comment = parse
  | eol { Lexing.new_line lexbuf; block_comment lexbuf }
  | eof { raise (Error "Unterminated block comment.") }
  | "*/" { next_token lexbuf }
  | _ { block_comment lexbuf }

and string = parse
  | eol | eof
    { raise (Error ("Unterminated string.")) }

  | '"'
    { let s = Buffer.contents string_buffer in
      Buffer.reset string_buffer;
      s }

  | "\\e" { Buffer.add_char string_buffer '\x1b'; string lexbuf }
  | "\\n" { Buffer.add_char string_buffer '\n'; string lexbuf }
  | "\\r" { Buffer.add_char string_buffer '\r'; string lexbuf }
  | "\\t" { Buffer.add_char string_buffer '\x09'; string lexbuf }
  | "\\v" { Buffer.add_char string_buffer '\x0b'; string lexbuf }
  | "\\b" { Buffer.add_char string_buffer '\x08'; string lexbuf }
  | "\\f" { Buffer.add_char string_buffer '\x0c'; string lexbuf }
  | "\\\\" { Buffer.add_char string_buffer '\\'; string lexbuf }
  | "\\\"" { Buffer.add_char string_buffer '"'; string lexbuf }

  | "\\x" ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F']
    as hex
    {
      let code = int_of_string ("0x" ^ (String.sub hex 2 2)) in
      Buffer.add_char string_buffer (Char.chr code);
      string lexbuf
    }

  | '\\' _ as c
    { raise (Error ("Invalid escape sequence '\\" ^ c ^ "' in string.")) }

  | _ as c
    { Buffer.add_char string_buffer c;
      string lexbuf }

{
  (* In the following, we define a new lexer, which wraps [lexer], and applies
     the following two transformations to the token stream:

     - A [IDENT] token is replaced with a sequence of either [IDENT VARIABLE] or
       [IDENT TYPE]. The decision is made via a call to [Context.is_typedefname].
       The call takes place only when the second element of the sequence is
       demanded. *)

  (* This second lexer is implemented using a 2-state state machine, whose
     states are as follows. *)

  type lexer_state =
    | SRegular          (* Nothing to recall from the previous tokens. *)
    | SIdent of string  (* We have seen an identifier: we have just
                           emitted a [NAME] token. The next token will be
                           either [VARIABLE] or [TYPE], depending on
                           what kind of identifier this is. *)

  let lexer =
    let st = ref SRegular in
    fun lexbuf ->
      match !st with
      | SIdent id ->
          st := SRegular;
          if is_typedefname id then TYPE else VARIABLE
      | SRegular ->
          let token = next_token lexbuf in
          match !st, token with
          | _, IDENT id ->
              st := SIdent id;
              token
          | _, _ ->
              st := SRegular;
              token
}
