module Unifty
open Flatten
open ASTBuilder

type Next<'t> = 
  | Return
  | Step of 't
  | Branch of 't*'t

let (|AsList|) = function 
  | Return -> []
  | Step t -> [t]
  | Branch (t1,t2) -> [t1;t2]

type CompNode = {
  instruction : Instruct
  next : Next<CompNode>
}

let fold f acc {next = (AsList l)} = List.fold f acc l

let getVarAtom = function 
  | IntLitAtom _ -> []
  | DataRefAtom _ -> []
  | VarAtom v -> [v]

let mapInstruct p q r s = function
  | CmpI (a,b) | AssignI (a,b) | AddI (a,b) 
  | SubI (a,b) | IMulI (a,b) -> p a b
  | CallI (a, l, bs) -> q a l bs
  | ReturnI a -> r a
  | JnzI l | JmpI l | LabelI l -> s l
    

