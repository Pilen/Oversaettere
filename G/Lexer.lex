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
       | "then"         => Parser.THEN pos
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

  | `'` [` ` `!` `#`-`&` `(`-`[` `]`-`~` (`\` ` `-`~`)] `'` 
                        { case Char.fromCString (getLexeme lexbuf) of
                               NONE   => lexerError lexbuf "Bad char!"
                             | SOME i => Parser.CHARCONST (i, getPos lexbuf) }
							 
  | `"` [` ` `!` `#`-`&` `(`-`[` `]`-`~` (`\` ` `-`~`)]* `"` 
                        {case String.fromCString (getLexeme lexbuf) of
                               NONE   => lexerError lexbuf "Bad string!"
                             | SOME i => Parser.STRINGCONST (i, getPos lexbuf)}
                       	 					 
  | [`a`-`z` `A`-`Z`] [`a`-`z` `A`-`Z` `0`-`9` `_`]*
                        { keyword (getLexeme lexbuf,getPos lexbuf) }
  | `+`                 { Parser.PLUS (getPos lexbuf) }
  | `-`                 { Parser.MINUS (getPos lexbuf) }
  | `<`                 { Parser.LESS (getPos lexbuf) }
  | `*` [`a`-`z` `A`-`Z`] [`a`-`z` `A`-`Z` `0`-`9` `_`]*
                        { Parser.REF (getPos lexbuf}
  | `=`                 { Parser.ASSIGN (getPos lexbuf) }
  | `==`                { Parser.EQUAL (getpos lexbuf)}  
  | `(`                 { Parser.LPAR (getPos lexbuf) }
  | `)`                 { Parser.RPAR (getPos lexbuf) }
  | `{`                 { Parser.LBRACE (getpos lexbuf)}
  | `}`                 { Parser.RBRACE (getpos lexbuf)}
  | `[`                 { Parser.LBRACK (getpos lexbuf)}
  | `]`                 { Parser.RBRACK (getpos lexbuf)}
  | `,`                 { Parser.COMMA (getPos lexbuf) }
  | `;`                 { Parser.SEMICOLON (getPos lexbuf) }
  | eof                 { Parser.EOF (getPos lexbuf) }
  | _                   { lexerError lexbuf "Illegal symbol in input" }

;
