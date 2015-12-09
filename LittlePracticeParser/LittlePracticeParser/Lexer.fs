module Lexer

open ParserMonad
open ResultMonad

//this function makes a parser instance and then applies it to the state that you inputted
//let func1 : Parser<'char, 'a> =
  (*
  lets say I want to parse the following line:
  1234590 uawdin 9 ww7772 bd 8
  
  I have to check for all characters and EOF and return a token
  and compile a list of these tokens
  eventually returning this

  *)

// this function makes a function that
// tests the input for all possibilities
// returns a token for the first input
// and then recursively checks the rest of the input the same way

let getHead : Parser<'char, 'char>=
  fun s ->
    match s with
    | h::t -> res{ return (h,t)}
    | [] -> res{return! Error(["we couldn't get any head"])}

let digit : Parser<'char, 'a> =
  prs{
    let! intval = getHead

  }

let Integer : Parser<'char, 'a> =
  prs{
    let! integerval = repeatAtLeastOnce digit
    return integerval
  }

let lexer () =
  fun s ->
    

  









//let lexer() = 
//here add the functions that get composed together to form the check for a valid input
//or an EOF, if both fail then they must return a failed result