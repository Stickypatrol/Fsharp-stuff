module program

open ParserMonad
open Lexer


//read all the characters
//that's now your beginstate
//we break this beginstate down char by char into tokens
//the tokens are now your ('a or contents)
  //we read a char
  //check if it's a 'processable' character
  //if its part of a longer string of characters then we repeat this procedure UNTIL we get a usable token
  //then we return this token and put it on top of the list of eventual characters that we're going to use to print.

//we then turn this list of tokens into blocks, which we can then process further...

let file  = System.IO.File.ReadAllText "main.txt"