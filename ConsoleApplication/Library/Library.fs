module Parser
open FParsec
open FParsec.CharParsers
open System

exception CompilerError of string

type Expression =
 | IntLit of int
 | StringLit of string 
 | Variable of string
 | Func of string * (Expression list)

type Statement = 
 | ReturnStat of Expression
 | IfStat of Expression * Statement list
 | Execution of Expression
 | Declaration of string * string
 | Assignment of string * Expression
 
type FuncSignature = {
  access : string
  isStatic : bool
  returnTy : string
  name : string
  args : (string * string) list
}

type Function = {
  signature : FuncSignature
  body : Statement list
}
 
type ParserData = unit
 
let allowabeNameChar = letter
let parseName = many1Chars letter .>> spaces
let tok s = pstring s .>> spaces >>% ()
let tok2 s = spaces >>. tok s

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
    let strChar = (manyChars (normal <|> escaped)) 
    between (pstring "\"") (tok "\"") strChar |>> StringLit
  let parseVariable = parseName |>> Variable
  let parseFunc = parse {
      let! name = parseName
      do! (tok "(") 
      let! args = sepBy (parseExpression' ()) (tok ",")
      do! (tok ")") 
      return Func (name, args)
  }
  
  attempt parseInt <|> 
  attempt parseString <|> 
  attempt parseFunc <|> 
  attempt parseVariable <?> 
  "Failed to parse expression"

let parseExpression = parseExpression'()

let rec parseStatement' () : Parser<Statement, ParserData> = 
  let parseReturn = tok "return " >>. parseExpression |>> ReturnStat
  let parseIfStat = tok "if"
                >>. between (tok "(") (tok ")") (parseExpression) 
               .>>. parseBody
                |>> IfStat 
  let parseExecution = parseExpression |>> Execution
  let parseDeclaration = parseName 
                    .>>. parseName 
                     |>> Declaration 
  let parseAssignment = parseName
                    .>> tok "="
                   .>>. parseExpression
                    |>> Assignment
  
  attempt parseReturn      <|> 
  attempt parseIfStat      <|> 
  attempt parseAssignment  <|>
  attempt parseDeclaration <|>
  attempt parseExecution   <?> 
  "Failed to parse statement"
and parseBody =  between (tok "{") (tok "}") (sepEndBy (parseStatement'()) (tok ";"))

let parseStatement = parseStatement'()

let parseSignature : Parser<FuncSignature, ParserData> = parse {
    let! access = parseName
    let! isStatic = attempt (tok "static") >>% true
                <|> preturn false 
    let! returnTy = parseName 
    let! name = parseName
    let! args = sepBy (parseName .>>. parseName) (tok ",")
    return {
      access = access
      isStatic = isStatic
      returnTy = returnTy
      args = args
      name = name
    }
  }
  
let parseFunction : Parser<Function, ParserData> = parse {
  let! signature = parseSignature
  let! body = parseBody
  return {signature = signature; body = body}
}

let parseModule = many parseFunction

let public parseProgram s : Function list =
  match run parseModule s with
  | Success(result,_,_) -> result
  | Failure(err,_,_) -> raise (CompilerError ("parser failed: " + err))
  










