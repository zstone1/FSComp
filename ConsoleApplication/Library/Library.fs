module Parser
open FParsec
open FParsec.CharParsers

type Literal = 
 | IntLit of int
 | StringLit of string

type Expression =
 | Atom of Literal
 | Func of string * (Expression list)

type Statement = 
 | ReturnStat of Expression
 | IfStat of Expression * Statement list
 | Execution of Expression
 | Declaration of string * string
 | Assignment of string * Expression
 
type ParserData = unit
 
let allowabeNameChar = letter
let parseName = manyChars letter
let tok s = pstring s .>> spaces >>% ()

let rec parseExpression' (): Parser<Expression,ParserData> =
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
  let parseFunc = parse {
      let! name = parseName 
      do! (tok "(") 
      do! (tok ")") 
      let! args = sepBy (parseExpression' ()) (tok ",")
      return Func (name, args)
  }
  parseAtom <|> parseFunc <?> "Failed to parse expression"

let parseExpression = parseExpression'()

let rec parseStatement' () : Parser<Statement, ParserData> = 
  let parseBody =  between (tok "{") (tok "}") (sepBy (parseStatement'()) (tok ";"))
  let parseReturn = tok "return " >>. parseExpression |>> ReturnStat
  let parseIfStat = parse {
    let! guard = between (tok "(") (tok ")") (parseExpression)
    let! body = parseBody
    return IfStat (guard, body)
  }
  let parseExecution = parseExpression |>> Execution
  let parseDeclaration = parse {
    let! ty = parseName
    let! name = parseName
    return Declaration (ty,name) 
  }
  let parseAssignment = parse{
    let! name = parseName
    do! tok "="
    let! value = parseExpression
    return Assignment (name, value)
  }
  
  parseReturn <|> 
  parseIfStat <|> 
  parseExecution <|> 
  parseDeclaration <|>
  parseAssignment













