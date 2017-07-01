namespace FSharpx
#nowarn "40"

open System
open System.Collections
open System.Collections.Generic
open System
open System.Globalization
open System.Diagnostics
open System.Runtime.ExceptionServices

[<AutoOpen>]
module Prelude =

    /// Transforms a function by flipping the order of its arguments.
    let inline flip f a b = f b a

    /// Transforms a function by flipping the order of its arguments.
    let inline flip3 f a b c = f c a b

    /// Transforms a function by flipping the order of its arguments.
    let inline flip4 f a b c d = f d a b c
    
    /// Transforms an uncurried function to a curried function.
    let inline curry f a b = f(a,b)

    /// Transforms an uncurried function to a curried function.
    let inline uncurry f (a,b) = f a b

    /// Transforms an uncurried function to a triple-curried function.
    let inline  curry3 f a b c = f (a, b, c)

    /// Transforms an uncurried function to a triple-curried function.
    let inline uncurry3 f (a,b,c) = f a b c

    /// Swap the elements of a pair.
    let inline swap (a,b) = (b,a)

    /// Given a value, creates a function with one ignored argument which returns the value.
    let inline konst a _ = a

    /// Given a value, creates a function with two ignored arguments which returns the value.
    let inline konst2 a _ _ = a

    /// Creates a pair
    let inline tuple2 a b = a,b
    
    /// Creates a 3-tuple
    let inline tuple3 a b c = a,b,c
    
    /// Creates a 4-tuple
    let inline tuple4 a b c d = a,b,c,d
    
    /// Creates a 5-tuple
    let inline tuple5 a b c d e = a,b,c,d,e
    
    /// Creates a 6-tuple
    let inline tuple6 a b c d e f = a,b,c,d,e,f

    /// Fixed point combinator.
    let rec fix f x = f (fix f) x

    /// Fixed point combinator.
    let rec fix2 f x y = f (fix2 f) x y

    /// Fixed point combinator.
    let rec fix3 f x y z = f (fix3 f) x y z

    let public trd (_,_,c) = c


    /// Sequencing operator like Haskell's ($). Has better precedence than (<|) due to the
    /// first character used in the symbol.
    let (^) = (<|)

    /// Bottom value
    let undefined<'T> : 'T = raise (NotImplementedException("result was implemented as undefined"))

    /// Given a value, apply a function to it, ignore the result, then return the original value.
    let inline tee fn x = fn x |> ignore; x

    /// Custom operator for `tee`: Given a value, apply a function to it, ignore the result, then return the original value.
    let inline (|>!) x fn = tee fn x

    /// Rethrows an exception. This can be used even outside of try-with block. The exception object (stacktrace, etc) is not changed.
    let reraise' (e:exn) : 'T = ExceptionDispatchInfo.Capture(e).Throw() ; undefined
    // http://thorarin.net/blog/post/2013/02/21/Preserving-Stack-Trace.aspx
    // https://stackoverflow.com/questions/7168801/how-to-use-reraise-in-async-workflows-in-f

    /// Rethrows an exception, but bebore that applies a function on it. This can be used even outside of try-with block. The exception object (stacktrace, etc) is not changed.
    let reraiseWith (f : exn -> unit) (e:exn) : 'T = f e ; reraise' e

    let inline toOption x = match x with
                            | true, v -> Some v
                            | _       -> None

    let inline tryWith f x = f x |> toOption
/// Generic monadic operators    
module Operators =

    /// Inject a value into the monadic type
    let inline returnM builder x = (^M: (member Return: 'b -> 'c) (builder, x))
    let inline bindM builder m f = (^M: (member Bind: 'd -> ('e -> 'c) -> 'c) (builder, m, f))
    let inline liftM builder f m =
        let inline ret x = returnM builder (f x)
        bindM builder m ret

    /// Sequential application
    let inline applyM (builder1:^M1) (builder2:^M2) f m =
        bindM builder1 f <| fun f' ->
            bindM builder2 m <| fun m' ->
                returnM builder2 (f' m') 
module ZipList = 
    let returnM v = Seq.initInfinite (fun _ -> v)
    /// Sequential application
    let (<*>) f a = Seq.zip f a |> Seq.map (fun (k,v) -> k v)
    /// Sequential application
    let inline ap m f = f <*> m
    /// Promote a function to a monad/applicative, scanning the monadic/applicative arguments from left to right.
    let inline lift2 f a b = returnM f <*> a <*> b
    /// Sequence actions, discarding the value of the first argument.
    let inline ( *>) x y = lift2 (fun _ z -> z) x y
    /// Sequence actions, discarding the value of the second argument.
    let inline ( <*) x y = lift2 (fun z _ -> z) x y

module State =

    type State<'T, 'State> = State of ('State -> 'T * 'State)
    
    let getState = State (fun s -> (s,s))
    let putState s = State (fun _ -> ((),s))
    let eval (State m) s = m s |> fst
    let exec (State m) s = m s |> snd
    let run (State m) s = m s 
    let empty = State (fun s -> ((), s))
    let bind (k: 'a -> State<'b,'c>) (State m) = State (fun s -> 
      let (a, s') = m s 
      let (State k') = k a
      k' s')
    
    /// The state monad.
    /// The algorithm is adjusted from my original work off of Brian Beckman's http://channel9.msdn.com/shows/Going+Deep/Brian-Beckman-The-Zen-of-Expressing-State-The-State-Monad/.
    /// The approach was adjusted from Matthew Podwysocki's http://codebetter.com/blogs/matthew.podwysocki/archive/2009/12/30/much-ado-about-monads-state-edition.aspx and mirrors his final result.
    type StateBuilder() =
        member this.Return(a) : State<'T,'State> = State(fun s -> (a,s))
        member this.ReturnFrom(m:State<'T,'State>) = m
        member this.Bind(m:State<'T,'State>, k:'T -> State<'U,'State>) : State<'U,'State> = bind k m
        member this.Zero() = this.Return ()
        member this.Combine(r1, r2) = this.Bind(r1, fun () -> r2)
        member this.TryWith((State m):State<'T,'State>, h:exn -> State<'T,'State>) : State<'T,'State> =
            State (fun env -> try m env
                              with e -> let (State  h') = (h e) in h' env)
        member this.TryFinally((State m):State<'T,'State>, compensation) : State<'T,'State> =
            State ( fun env -> try m env
                               finally compensation())
        member this.Using(res:#IDisposable, body) =
            this.TryFinally(body res, (fun () -> match res with null -> () | disp -> disp.Dispose()))
        member this.Delay(f) = this.Bind(this.Return (), f)
        member this.While(guard, m) =
            if not(guard()) then this.Zero() else
                this.Bind(m, (fun () -> this.While(guard, m)))
        member this.For(sequence:seq<_>, body) =
            this.Using(sequence.GetEnumerator(),
                (fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current))))
    let state = StateBuilder()
    
    open Operators

    /// Inject a value into the State type
    let inline returnM x = returnM state x
    /// Sequentially compose two actions, passing any value produced by the first as an argument to the second.
    let inline (>>=) m f = bindM state m f
    /// Flipped >>=
    let inline (=<<) f m = bindM state m f
    /// Sequential application
    let inline (<*>) f m = applyM state state f m
    /// Sequential application
    let inline ap m f = f <*> m
    /// Transforms a State value by using a specified mapping function.
    let inline map f m = liftM state f m
    /// Infix map
    let inline (<!>) f m = map f m
    /// Promote a function to a monad/applicative, scanning the monadic/applicative arguments from left to right.
    let inline lift2 f a b = returnM f <*> a <*> b
    /// Sequence actions, discarding the value of the first argument.
    let inline ( *>) x y = lift2 (fun _ z -> z) x y
    /// Sequence actions, discarding the value of the second argument.
    let inline ( <*) x y = lift2 (fun z _ -> z) x y
    /// Sequentially compose two state actions, discarding any value produced by the first
    let inline (>>.) m f = bindM state m (fun _ -> f)
    /// Left-to-right Kleisli composition
    let inline (>=>) f g = fun x -> f x >>= g
    /// Right-to-left Kleisli composition
    let inline (<=<) x = flip (>=>) x
    
    let inline public (|>>) m f = f <!> m

    let public updateState (f:'State -> 'State) : State<'State,'State> = state{
      let! prev = getState
      let next = f prev
      do! putState next
      return next
    }
    
    let public updateState' f = updateState f |>> ignore

    let foldM f s = 
        Seq.fold (fun acc t -> acc >>= (flip f) t) (returnM s)

    let inline sequence s =
        let inline cons a b = lift2 (fun x y -> x :: y) a b
        List.foldBack cons s (returnM [])

    let inline mapM f x = sequence (List.map f x)

    let inline mapMUnit (f:'a -> State<unit,'c>) x = sequence (List.map f x) |>> ignore
        


