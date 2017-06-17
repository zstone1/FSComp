module Parser
open FParsec
open FParsec.CharParsers

type Literal = 
 | IntLit of int
 | StringLit of string

type Expression =
 | Atom of Literal
 | Func of string * (Expression list)
 
 
type ParserData = unit
 
let allowabeNameChar = letter
let parseName = manyChars letter
let tok s = pstring s >>. spaces

let rec parseExpression (): Parser<Expression,ParserData> =
  let parseInt = pint32 .>> spaces |>> IntLit
  
  let parseString = 
    let normal = satisfy (fun c -> c <>'\\' && c <> '"')
    let unescapeChar = function
        | 'n' -> '\n' 
        | 'r' -> '\r'
        | 't' -> '\t'
        |  c  ->  c
    let escaped = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescapeChar)
    between (pstring "\"") (tok "\"") (manyChars (normal <|> escaped)) |>> StringLit
 

  let parseAtom = parseInt <|> parseString |>> Atom
  let parseFunc = parseName .>>. between (tok "(") (tok ")") (sepBy (parseExpression ()) (tok ",")) |>> Func
  parseAtom <|> parseFunc <?> "Failed to parse expression"













