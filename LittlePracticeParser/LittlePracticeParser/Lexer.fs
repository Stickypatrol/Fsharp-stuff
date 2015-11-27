module Lexer

open ParserMonad
open ResultMonad

//this function makes a parser instance and then applies it to the state that you inputted
//let func1 : Parser<'char, 'a> =
  (*
  lets say I want to parse the following line:
  while(x > 5){print(x); x-1}
  I need to remove the whitespace because it's not necessary
  I need to read the first character each time, 
  *)
  









//let lexer() = 
//here add the functions that get composed together to form the check for a valid input
//or an EOF, if both fail then they must return a failed result