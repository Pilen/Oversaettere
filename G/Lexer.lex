{
 open Lexing;

 exception LexicalError of string * (int * int) (* (message, (line, column)) *)

 val currentLine = ref 1
 val lineStartPos = ref [0]

 fun getPos lexbuf = getLineCol (getLexemeStart lexbuf)
                                (!currentLine)
                                (!lineStartPos)

 and getLineCol pos line (p1::ps) =
       if pos>=p1 then (line, pos-p1)
       else getLineCol pos (line-1) ps
   | getLineCol pos line [] = raise LexicalError ("",(0,0))

 fun lexerError lexbuf s =
     raise LexicalError (s, getPos lexbuf)

 fun keyword (s, pos) =
     case s of
         "if"           => Parser.IF pos
       | "else"         => Parser.ELSE pos
       | "while"        => Parser.WHILE pos
       | "int"          => Parser.INT pos
       | "char"         => Parser.CHAR pos
       | "return"       => Parser.RETURN pos
       | _              => Parser.ID (s, pos)

 }
rule Token = parse
    [` ` `\t` `\r`]+    { Token lexbuf } (* whitespace *)
    | "/*" ([^`*`] | `*`[^`/`])* "*/"
                        { Token lexbuf } (* comment *)
  | [`\n` `\012`]       { currentLine := !currentLine+1;
                          lineStartPos :=  getLexemeStart lexbuf
                                           :: !lineStartPos;
                          Token lexbuf } (* newlines *)
  | [`0`-`9`]+          { case Int.fromString (getLexeme lexbuf) of
                               NONE   => lexerError lexbuf "Bad integer!"
                             | SOME i => Parser.NUM (i, getPos lexbuf) }

  | `'` ([` ` `!` `#`-`&` `(`-`[` `]`-`~`]|(`\` [` `-`~`])) `'`
                        { let val cs = getLexeme lexbuf
                            in (case Char.fromCString
                                (String.substring
                                 (cs,1,(size cs)-2)) of
                                NONE   => lexerError lexbuf "Bad char!"
                                | SOME i => Parser.CHARCONST (i, getPos lexbuf))
                          end
                        }
  | `"` ([` ` `!` `#`-`&` `(`-`[` `]`-`~`]|(`\` [` `-`~`]))* `"`
                        {case String.fromCString (getLexeme lexbuf) of
                            NONE   => lexerError lexbuf "Bad string!"
                            | SOME i => Parser.STRINGCONST (String.substring
                                                            (i,1,(size i)-2),
                                                            getPos lexbuf)}

  | [`a`-`z` `A`-`Z`] [`a`-`z` `A`-`Z` `0`-`9` `_`]*
                        { keyword (getLexeme lexbuf,getPos lexbuf) }
  | `+`                 { Parser.PLUS (getPos lexbuf) }
  | `-`                 { Parser.MINUS (getPos lexbuf) }
  | `<`                 { Parser.LESS (getPos lexbuf) }
  | `*` [`a`-`z` `A`-`Z`] [`a`-`z` `A`-`Z` `0`-`9` `_`]*
                        { let val s = getLexeme lexbuf;
                          in Parser.REF (String.substring(s,1,(size s)-1),
                                         getPos lexbuf) end }
  | `=`                 { Parser.ASSIGN (getPos lexbuf) }
  | `=` `=`             { Parser.EQUAL (getPos lexbuf)}
  | `(`                 { Parser.LPAR (getPos lexbuf) }
  | `)`                 { Parser.RPAR (getPos lexbuf) }
  | `{`                 { Parser.LBRACE (getPos lexbuf)}
  | `}`                 { Parser.RBRACE (getPos lexbuf)}
  | `[`                 { Parser.LBRACK (getPos lexbuf)}
  | `]`                 { Parser.RBRACK (getPos lexbuf)}
  | `,`                 { Parser.COMMA (getPos lexbuf) }
  | `;`                 { Parser.SEMICOLON (getPos lexbuf) }
  | `r``e``t``u``r``n`  { Parser.RETURN (getPos lexbuf) }
  | eof                 { Parser.EOF (getPos lexbuf) }
  | _                   { lexerError lexbuf "Illegal symbol in input" }

;
