local open Obj Lexing in


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

 
fun action_23 lexbuf = (
 lexerError lexbuf "Illegal symbol in input" )
and action_22 lexbuf = (
 Parser.EOF (getPos lexbuf) )
and action_21 lexbuf = (
 Parser.RETURN (getPos lexbuf) )
and action_20 lexbuf = (
 Parser.SEMICOLON (getPos lexbuf) )
and action_19 lexbuf = (
 Parser.COMMA (getPos lexbuf) )
and action_18 lexbuf = (
 Parser.RBRACK (getPos lexbuf))
and action_17 lexbuf = (
 Parser.LBRACK (getPos lexbuf))
and action_16 lexbuf = (
 Parser.RBRACE (getPos lexbuf))
and action_15 lexbuf = (
 Parser.LBRACE (getPos lexbuf))
and action_14 lexbuf = (
 Parser.RPAR (getPos lexbuf) )
and action_13 lexbuf = (
 Parser.LPAR (getPos lexbuf) )
and action_12 lexbuf = (
 Parser.EQUAL (getPos lexbuf))
and action_11 lexbuf = (
 Parser.ASSIGN (getPos lexbuf) )
and action_10 lexbuf = (
 let val s = getLexeme lexbuf;
                          in Parser.REF (String.substring(s,1,(size s)-1),
                                         getPos lexbuf) end )
and action_9 lexbuf = (
 Parser.LESS (getPos lexbuf) )
and action_8 lexbuf = (
 Parser.MINUS (getPos lexbuf) )
and action_7 lexbuf = (
 Parser.PLUS (getPos lexbuf) )
and action_6 lexbuf = (
 keyword (getLexeme lexbuf,getPos lexbuf) )
and action_5 lexbuf = (
case String.fromCString (getLexeme lexbuf) of
                               NONE   => lexerError lexbuf "Bad string!"
                            | SOME i => Parser.STRINGCONST (String.substring
                                                            (i,1,(size i)-2),
                                                            getPos lexbuf))
and action_4 lexbuf = (
 let val cs = getLexeme lexbuf
                          in (case Char.fromCString (String.substring
                                                     (cs,1,(size cs)-2)) of
                                NONE   => lexerError lexbuf "Bad char!"
                                | SOME i => Parser.CHARCONST (i, getPos lexbuf))
                          end
                        )
and action_3 lexbuf = (
 case Int.fromString (getLexeme lexbuf) of
                               NONE   => lexerError lexbuf "Bad integer!"
                             | SOME i => Parser.NUM (i, getPos lexbuf) )
and action_2 lexbuf = (
 currentLine := !currentLine+1;
                          lineStartPos :=  getLexemeStart lexbuf
                                           :: !lineStartPos;
                          Token lexbuf )
and action_1 lexbuf = (
 Token lexbuf )
and action_0 lexbuf = (
 Token lexbuf )
and state_0 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_18 lexbuf
 else if currChar >= #"a" andalso currChar <= #"q" then  state_18 lexbuf
 else if currChar >= #"s" andalso currChar <= #"z" then  state_18 lexbuf
 else if currChar >= #"0" andalso currChar <= #"9" then  state_14 lexbuf
 else case currChar of
    #"\t" => state_3 lexbuf
 |  #"\r" => state_3 lexbuf
 |  #" " => state_3 lexbuf
 |  #"\n" => action_2 lexbuf
 |  #"\f" => action_2 lexbuf
 |  #"}" => action_16 lexbuf
 |  #"{" => action_15 lexbuf
 |  #"r" => state_22 lexbuf
 |  #"]" => action_18 lexbuf
 |  #"\\" => state_20 lexbuf
 |  #"[" => action_17 lexbuf
 |  #"=" => state_17 lexbuf
 |  #"<" => action_9 lexbuf
 |  #";" => action_20 lexbuf
 |  #"/" => state_13 lexbuf
 |  #"-" => action_8 lexbuf
 |  #"," => action_19 lexbuf
 |  #"+" => action_7 lexbuf
 |  #"*" => state_9 lexbuf
 |  #")" => action_14 lexbuf
 |  #"(" => action_13 lexbuf
 |  #"'" => state_6 lexbuf
 |  #"\"" => state_5 lexbuf
 |  #"\^@" => action_22 lexbuf
 |  _ => action_23 lexbuf
 end)
and state_3 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_0);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\t" => state_42 lexbuf
 |  #"\r" => state_42 lexbuf
 |  #" " => state_42 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_5 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_5);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"(" andalso currChar <= #"[" then  action_5 lexbuf
 else if currChar >= #"]" andalso currChar <= #"~" then  action_5 lexbuf
 else case currChar of
    #"!" => action_5 lexbuf
 |  #" " => action_5 lexbuf
 |  #"&" => action_5 lexbuf
 |  #"%" => action_5 lexbuf
 |  #"$" => action_5 lexbuf
 |  #"#" => action_5 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_6 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_23);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"(" andalso currChar <= #"[" then  action_4 lexbuf
 else if currChar >= #"]" andalso currChar <= #"~" then  action_4 lexbuf
 else case currChar of
    #"!" => action_4 lexbuf
 |  #" " => action_4 lexbuf
 |  #"&" => action_4 lexbuf
 |  #"%" => action_4 lexbuf
 |  #"$" => action_4 lexbuf
 |  #"#" => action_4 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_9 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_23);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_41 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_41 lexbuf
 else backtrack lexbuf
 end)
and state_13 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_23);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"*" => state_38 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_14 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_3);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_37 lexbuf
 else backtrack lexbuf
 end)
and state_17 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_11);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"=" => action_12 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_18 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_6);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_25 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_25 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_25 lexbuf
 else case currChar of
    #"_" => state_25 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_20 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_23);
 let val currChar = getNextChar lexbuf in
 if currChar >= #" " andalso currChar <= #"~" then  state_31 lexbuf
 else backtrack lexbuf
 end)
and state_22 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_6);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_25 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_25 lexbuf
 else if currChar >= #"f" andalso currChar <= #"z" then  state_25 lexbuf
 else case currChar of
    #"_" => state_25 lexbuf
 |  #"d" => state_25 lexbuf
 |  #"c" => state_25 lexbuf
 |  #"b" => state_25 lexbuf
 |  #"a" => state_25 lexbuf
 |  #"e" => state_26 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_25 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_6);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_25 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_25 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_25 lexbuf
 else case currChar of
    #"_" => state_25 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_26 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_6);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_25 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_25 lexbuf
 else if currChar >= #"a" andalso currChar <= #"s" then  state_25 lexbuf
 else if currChar >= #"u" andalso currChar <= #"z" then  state_25 lexbuf
 else case currChar of
    #"_" => state_25 lexbuf
 |  #"t" => state_27 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_27 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_6);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_25 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_25 lexbuf
 else if currChar >= #"a" andalso currChar <= #"t" then  state_25 lexbuf
 else case currChar of
    #"_" => state_25 lexbuf
 |  #"z" => state_25 lexbuf
 |  #"y" => state_25 lexbuf
 |  #"x" => state_25 lexbuf
 |  #"w" => state_25 lexbuf
 |  #"v" => state_25 lexbuf
 |  #"u" => state_28 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_28 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_6);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_25 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_25 lexbuf
 else if currChar >= #"a" andalso currChar <= #"q" then  state_25 lexbuf
 else if currChar >= #"s" andalso currChar <= #"z" then  state_25 lexbuf
 else case currChar of
    #"_" => state_25 lexbuf
 |  #"r" => state_29 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_29 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_6);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_25 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_25 lexbuf
 else if currChar >= #"a" andalso currChar <= #"m" then  state_25 lexbuf
 else if currChar >= #"o" andalso currChar <= #"z" then  state_25 lexbuf
 else case currChar of
    #"_" => state_25 lexbuf
 |  #"n" => state_30 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_30 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_6);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_25 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_25 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_25 lexbuf
 else case currChar of
    #"_" => state_25 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_31 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\\" => state_34 lexbuf
 |  #"'" => action_4 lexbuf
 |  #"\"" => action_5 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_34 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #" " andalso currChar <= #"~" then  state_35 lexbuf
 else backtrack lexbuf
 end)
and state_35 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\\" => state_34 lexbuf
 |  #"\"" => action_5 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_37 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_3);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_37 lexbuf
 else backtrack lexbuf
 end)
and state_38 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"*" => state_39 lexbuf
 |  #"\^@" => backtrack lexbuf
 |  _ => state_38 lexbuf
 end)
and state_39 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"/" => action_1 lexbuf
 |  #"\^@" => backtrack lexbuf
 |  _ => state_38 lexbuf
 end)
and state_41 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_10);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_41 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_41 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_41 lexbuf
 else case currChar of
    #"_" => state_41 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_42 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_0);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\t" => state_42 lexbuf
 |  #"\r" => state_42 lexbuf
 |  #" " => state_42 lexbuf
 |  _ => backtrack lexbuf
 end)
and Token lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_0 lexbuf)

(* The following checks type consistency of actions *)
val _ = fn _ => [action_23, action_22, action_21, action_20, action_19, action_18, action_17, action_16, action_15, action_14, action_13, action_12, action_11, action_10, action_9, action_8, action_7, action_6, action_5, action_4, action_3, action_2, action_1, action_0];

end
