mosmlc -c Mips.sml
mosmlc -c RegAlloc.sig RegAlloc.sml
mosmlc -c S100.sml
mosmlyac -v Parser.grm
mosmlc -liberal -c Parser.sig Parser.sml 
mosmllex Lexer.lex
mosmlc -c Lexer.sml
mosmlc -c Type.sig Type.sml
mosmlc -c Compiler.sig Compiler.sml
mosmlc -o C100 C100.sml


./C100 error01
./C100 error02
./C100 error03
./C100 error04
./C100 error05
./C100 error06
./C100 error07
./C100 error08
./C100 error09
./C100 error10
./C100 error11
./C100 error12
./C100 error13
./C100 error14
./C100 error15

echo -e "\nour errors"
./C100 ourerror01
./C100 ourerror02


echo -e "\nprograms"
./C100 charref
#./C100 copy
#./C100 dfa
#./C100 fib
#./C100 sort
#./C100 sort2
#./C100 ssort
#./C100 ssort2