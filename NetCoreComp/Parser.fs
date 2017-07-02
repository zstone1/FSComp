module Parser
open FParsec
open FParsec.CharParsers
open System

type Expression =
 | IntLit of int
 | StringLit of string 
 | Variable of string
 | Func of string * (Expression list)

type PStatement = 
 | ReturnStat of Expression
 | IfStat of Expression * PStatement list
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

type ParserFunction = {
  signature : FuncSignature
  body : PStatement list
}
 
type ParserData = unit


let allowabeNameChar = letter
let parseName = many1Chars letter .>> spaces
let tok s = pstring s .>> spaces >>% ()
let tok2 s = spaces >>. tok s
let betweenParens a = between (tok "(") (tok ")") a
let betweenCurlys a = between (tok "{") (tok "}") a

let opp = OperatorPrecedenceParser<_,_,ParserData>()
let parseExpression = opp.ExpressionParser;

let arithHelper s= InfixOperator(s, spaces, 1, Associativity.Left, fun x y -> Func ("_" + s,[x;y]))
opp.AddOperator(arithHelper "-")
opp.AddOperator(arithHelper "+")
opp.AddOperator(arithHelper "*")

let expr = 
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
     let! args = sepBy parseExpression (tok ",") |> betweenParens 
     return Func (name,args)}
   let parsensExpr = betweenParens parseExpression
              
   attempt parsensExpr   <|>
   attempt parseInt      <|> 
   attempt parseString   <|> 
   attempt parseFunc     <|> 
   attempt parseVariable <?> 
   "Failed to parse expression"
opp.TermParser <- expr

let parseStatement, parseStatementRef = createParserForwardedToRef()

do parseStatementRef :=
   let parseReturn = tok "return " >>. parseExpression |>> ReturnStat
   let parseBody =  sepEndBy parseStatement (tok ";") |> betweenCurlys
   let parseIfStat = tok "if"
                 >>. (parseExpression |> betweenParens)
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

let parseSignature = parse {
    let! access = parseName
    let! isStatic = attempt (tok "static") >>% true
                <|> preturn false 
    let! returnTy = parseName 
    let! name = parseName
    let! args = sepBy (parseName .>>. parseName) (tok ",") 
             |> betweenParens
    return {
      access = access
      isStatic = isStatic
      returnTy = returnTy
      args = args
      name = name
    } }
  
let parseFunction = parse {
  let! signature = parseSignature
  let! body = sepEndBy parseStatement (tok ";") |> betweenCurlys
  return {signature = signature; body = body} }

let parseModule = spaces >>. many parseFunction .>> spaces .>> eof

let public parseProgram s : ParserFunction list =
  match run parseModule s with
  | Success(result,_,_) -> result
  | Failure(err,_,_) -> raise (CompilerErrorException ("parser failed: " + err))









