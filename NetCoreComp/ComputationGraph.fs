module ComputationGraph
open Hekate
open ASTBuilder

type CompNode = {
    algorithm : ASTStatement list
    variables : ASTVariable list
}