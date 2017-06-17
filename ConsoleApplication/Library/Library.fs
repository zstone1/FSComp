module Parser
open FParsec
open FParsec.CharParsers

type Literal = 
 | IntLit of int
 | StringLit of string

type Expression =
 | Atom of Literal
 | Func of string * (Expression list)
 

let ParseInt = pint32 .>> spaces |>> IntLit
 
 
let Parse = run ParseInt

