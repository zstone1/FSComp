module Parser
open FParsec
open FParsec.CharParsers

exception CompilerError of string

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
      let! args = sepBy (parseExpression' ()) (tok ",")
      do! (tok ")") 
      return Func (name, args)
  }
  parseAtom <|> parseFunc <?> "Failed to parse expression"

let parseExpression = parseExpression'()

let rec parseStatement' () : Parser<Statement, ParserData> = 
  let parseReturn = tok "return " >>. parseExpression |>> ReturnStat
  let parseIfStat = between (tok "(") (tok ")") (parseExpression) 
                    .>>. parseBody
                    |>> IfStat 
  let parseExecution = parseExpression |>> Execution
  let parseDeclaration = parseName .>>. parseName |>> Declaration 
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
and parseBody =  between (tok "{") (tok "}") (sepBy (parseStatement'()) (tok ";"))

let parseStatement = parseStatement'()

let parseSignature : Parser<FuncSignature, ParserData> = 
  let parseSignature isStatic = parse{
    let! access = parseName
    do! if isStatic then tok "static" else preturn ()
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
  parseSignature true <|> parseSignature false
  
let parseFunction : Parser<Function, ParserData> = parse {
  let! signature = parseSignature
  let! body = parseBody
  return {signature = signature; body = body}
}

let parseModule = many parseFunction

let public parseProgram s : Function list =
  match run parseModule s with
  | Success(result,_,_) -> result
  | Failure(err,_,_) -> raise (CompilerError ("parser failed with " + err))
  










