module Lexer

open Common
open ParserMonad

type Token = 
  | Literal of Literal
  | Id of Id
  | Keyword of Keyword
  | Open of Bracket
  | Closed of Bracket



let digit : Parser<char,char> =
  prs{
    let! x = getFirstSymbol
    if x >= '0' && x <= '9' then
      return x
    else
      return! fail ["error: expected digit"]
  }

let alphabetic : Parser<char,char> =
  prs{
    let! x = getFirstSymbol
    if (x >= 'A' && x <= 'Z') || (x >= 'a' && x <= 'z') then
      return x
    else
      return! fail ["error: expected alphabetic"]
  }

let integer : Parser<char, Token> =
  prs{
    let! ds = repeatAtLeastOnce digit
    return Literal(Int(System.Int32.Parse(new System.String(Seq.toArray(ds)))))
  }

let identifier =
  prs{
    let! ds = repeatAtLeastOnce alphabetic
    return Id(new System.String(Seq.toArray(ds)))
  }

let rec word (s:List<char>) : Parser<char,Unit> =
  match s with
  | [] -> prs{ return ()}
  | x::xs ->
    prs{
      let! d = getFirstSymbol
      if d = x then
        return! word xs
      else
        return! fail ["error: unexpected character"]
    }

let wordToKeyword s k =
  prs{
    do! word (List.ofSeq s)
    return Keyword k
  }

let openBracket s b =
  prs{
    do! word (List.ofSeq s)
    return Open b
  }

let closedBracket s b =
  prs{
    do! word (List.ofSeq s)
    return Closed b
  }

let whitespace : Parser<char, Unit> =
  prs{
    let! c = getFirstSymbol
    if c = '\t' || c = ' ' || c = '\n' || c = '\r' then
      return ()
    else
      return! fail ["error: expected whitespace"]
  }

let skipWhitespace =
    prs{
      let! x = getFirstSymbol
      if x = ' ' || x = '\t' || x = '\n' || x = '\r' then
        return ()
      else
        return! fail ["error: expected whitespace"]
    }

let skipSpaces =
  prs{
    let! _ = repeat whitespace
    return ()
  }

let rec lexer() : Parser<char, List<Token>> =
  prs{
    do! skipSpaces
    let! first =
      integer 
      .|| wordToKeyword "while" Keyword.While
      .|| wordToKeyword "if" Keyword.If
      .|| wordToKeyword "=" Keyword.Equals
      .|| wordToKeyword "/" Keyword.DividedBy
      .|| wordToKeyword "+" Keyword.Plus
      .|| wordToKeyword "-" Keyword.Minus
      .|| wordToKeyword ">" Keyword.GreaterThan
      .|| wordToKeyword ";" Keyword.Semicolon
      .|| wordToKeyword "print" Keyword.Print
      .|| openBracket "(" Bracket.Round
      .|| closedBracket ")" Bracket.Round
      .|| openBracket "{" Bracket.Curly
      .|| closedBracket "}" Bracket.Curly
      .|| identifier
    do! skipSpaces
    let! rest = lexer()
    return first :: rest
  } .||
  prs{
    do! eof
    return []
  }