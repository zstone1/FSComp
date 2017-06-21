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

    type Boolean with
        static member parse =
            tryWith bool.TryParse

    type Byte with
        static member parseWithOptions style provider x =
            Byte.TryParse(x, style, provider) |> toOption

        static member parse x =
            Byte.parseWithOptions NumberStyles.Integer CultureInfo.InvariantCulture x

    type SByte with
        static member parseWithOptions style provider x =
            SByte.TryParse(x, style, provider) |> toOption

        static member parse x =
            SByte.parseWithOptions NumberStyles.Integer CultureInfo.InvariantCulture x

    type UInt16 with
        static member parseWithOptions style provider x =
            UInt16.TryParse(x, style, provider) |> toOption

        static member parse x =
            UInt16.parseWithOptions NumberStyles.Integer CultureInfo.InvariantCulture x

    type Int16 with
        static member parseWithOptions style provider x =
            Int16.TryParse(x, style, provider) |> toOption

        static member parse x =
            Int16.parseWithOptions NumberStyles.Integer CultureInfo.InvariantCulture x

    type UInt32 with
        static member parseWithOptions style provider x =
            UInt32.TryParse(x, style, provider) |> toOption

        static member parse x =
            UInt32.parseWithOptions NumberStyles.Integer CultureInfo.InvariantCulture x

    type Int32 with
        static member parseWithOptions style provider x =
            Int32.TryParse(x, style, provider) |> toOption

        static member parse x =
            Int32.parseWithOptions NumberStyles.Integer CultureInfo.InvariantCulture x

    type UInt64 with
        static member parseWithOptions style provider x =
            UInt64.TryParse(x, style, provider) |> toOption

        static member parse x =
            UInt64.parseWithOptions NumberStyles.Integer CultureInfo.InvariantCulture x

    type Int64 with
        static member parseWithOptions style provider x =
            Int64.TryParse(x, style, provider) |> toOption

        static member parse x =
            Int64.parseWithOptions NumberStyles.Integer CultureInfo.InvariantCulture x

    type Decimal with
        static member parseWithOptions style provider x =
            Decimal.TryParse(x, style, provider) |> toOption

        static member parse x =
            Decimal.parseWithOptions NumberStyles.Currency CultureInfo.InvariantCulture x

    type Single with
        static member parseWithOptions style provider x =
            Single.TryParse(x, style, provider) |> toOption

        static member parse x =
            Single.parseWithOptions NumberStyles.Float CultureInfo.InvariantCulture x

    type Double with
        static member parseWithOptions style provider x =
            Double.TryParse(x, style, provider) |> toOption

        static member parse x =
            Double.parseWithOptions NumberStyles.Float CultureInfo.InvariantCulture x

    type DateTime with
        static member parseWithOptions style provider x =
            DateTime.TryParse(x, provider, style) |> toOption

        static member parse x =
            DateTime.parseWithOptions DateTimeStyles.None CultureInfo.InvariantCulture x

        static member parseExactWithOptions style provider (formats: string[]) x =
            DateTime.TryParseExact(x, formats, provider, style) |> toOption

        static member parseExact formats x =
            DateTime.parseExactWithOptions DateTimeStyles.None CultureInfo.InvariantCulture formats x

    type DateTimeOffset with
        static member parseWithOptions style provider x =
            DateTimeOffset.TryParse(x, provider, style) |> toOption

        static member parse x =
            DateTimeOffset.parseWithOptions DateTimeStyles.None CultureInfo.InvariantCulture x

        static member parseExactWithOptions style provider (formats: string[]) x =
            DateTimeOffset.TryParseExact(x, formats, provider, style) |> toOption

        static member parseExact formats x =
            DateTimeOffset.parseExactWithOptions DateTimeStyles.None CultureInfo.InvariantCulture formats x

    // Active patterns
    let (|Boolean       |_|) = Boolean.parse
    let (|Byte          |_|) = Byte.parse
    let (|SByte         |_|) = SByte.parse
    let (|UInt16        |_|) = UInt16.parse
    let (|Int16         |_|) = Int16.parse
    let (|UInt32        |_|) = UInt32.parse
    let (|Int32         |_|) = Int32.parse
    let (|UInt64        |_|) = UInt64.parse
    let (|Int64         |_|) = Int64.parse
    let (|Decimal       |_|) = Decimal.parse
    let (|Single        |_|) = Single.parse
    let (|Double        |_|) = Double.parse
    let (|DateTime      |_|) = DateTime.parse
    let (|DateTimeOffset|_|) = DateTimeOffset.parse
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

module Async =
    open Operators
    
    /// Sequentially compose two actions, passing any value produced by the second as an argument to the first.
    let inline bind f m = async.Bind(m,f)
    /// Inject a value into the async type
    let inline returnM x = returnM async x
    /// Sequentially compose two actions, passing any value produced by the first as an argument to the second.
    let inline (>>=) m f = bindM async m f
    /// Flipped >>=
    let inline (=<<) f m = bindM async m f
    /// Sequential application
    let inline (<*>) f m = applyM async async f m
    /// Sequential application
    let inline ap m f = f <*> m
    /// Flipped map
    let inline pipe m f = liftM async f m
    let inline pipe2 x y f = returnM f <*> x <*> y
    let inline pipe3 x y z f = returnM f <*> x <*> y <*> z
    /// Transforms an async value by using a specified mapping function.
    let inline map f m = pipe m f
    /// Promote a function to a monad/applicative, scanning the monadic/applicative arguments from left to right.
    let inline lift2 f x y = returnM f <*> x <*> y
    /// Infix map
    let inline (<!>) f m = pipe m f
    /// Sequence actions, discarding the value of the first argument.
    let inline ( *>) x y = pipe2 x y (fun _ z -> z)
    /// Sequence actions, discarding the value of the second argument.
    let inline ( <*) x y = pipe2 x y (fun z _ -> z)

    /// Sequentially compose two async actions, discarding any value produced by the first
    let inline (>>.) m f = bindM async m (fun _ -> f)
    /// Left-to-right Kleisli composition
    let inline (>=>) f g = fun x -> f x >>= g
    /// Right-to-left Kleisli composition
    let inline (<=<) x = flip (>=>) x

    let foldM f s = 
        Seq.fold (fun acc t -> acc >>= (flip f) t) (returnM s)

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

module Option =

    /// The maybe monad.
    /// This monad is my own and uses an 'T option. Others generally make their own Maybe<'T> type from Option<'T>.
    /// The builder approach is from Matthew Podwysocki's excellent Creating Extended Builders series http://codebetter.com/blogs/matthew.podwysocki/archive/2010/01/18/much-ado-about-monads-creating-extended-builders.aspx.
    type MaybeBuilder() =
        member this.Return(x) = Some x

        member this.ReturnFrom(m: 'T option) = m

        member this.Bind(m, f) = Option.bind f m

        member this.Zero() = None

        member this.Combine(m, f) = Option.bind f m

        member this.Delay(f: unit -> _) = f

        member this.Run(f) = f()

        member this.TryWith(m, h) =
            try this.ReturnFrom(m)
            with e -> h e

        member this.TryFinally(m, compensation) =
            try this.ReturnFrom(m)
            finally compensation()

        member this.Using(res:#IDisposable, body) =
            this.TryFinally(body res, fun () -> match res with null -> () | disp -> disp.Dispose())

        member this.While(guard, f) =
            if not (guard()) then Some () else
            do f() |> ignore
            this.While(guard, f)

        member this.For(sequence:seq<_>, body) =
            this.Using(sequence.GetEnumerator(),
                                 fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current)))
    let maybe = MaybeBuilder()

    /// Option wrapper monoid
    
    open Operators
    
    /// Inject a value into the option type
    let inline returnM x = returnM maybe x

    /// Sequentially compose two actions, passing any value produced by the first as an argument to the second.
    let inline (>>=) m f = bindM maybe m f

    /// Flipped >>=
    let inline (=<<) f m = bindM maybe m f

    /// Sequential application
    let inline (<*>) f m = applyM maybe maybe f m

    /// Sequential application
    let inline ap m f = f <*> m

    /// Infix map
    let inline (<!>) f m = Option.map f m

    /// Promote a function to a monad/applicative, scanning the monadic/applicative arguments from left to right.
    let inline lift2 f a b = returnM f <*> a <*> b

    /// Sequence actions, discarding the value of the first argument.
    let inline ( *>) x y = lift2 (fun _ z -> z) x y

    /// Sequence actions, discarding the value of the second argument.
    let inline ( <*) x y = lift2 (fun z _ -> z) x y

    /// Sequentially compose two maybe actions, discarding any value produced by the first
    let inline (>>.) m f = bindM maybe m (fun _ -> f)

    /// Left-to-right Kleisli composition
    let inline (>=>) f g = fun x -> f x >>= g

    /// Right-to-left Kleisli composition
    let inline (<=<) x = flip (>=>) x

    /// Maps a Nullable to Option
    let ofNullable (n: _ Nullable) = 
        if n.HasValue
            then Some n.Value
            else None

    /// Maps an Option to Nullable
    let toNullable =
        function
        | None -> Nullable()
        | Some x -> Nullable(x)

    /// True -> Some(), False -> None
    let inline ofBool b = if b then Some() else None

    /// Converts a function returning bool,value to a function returning value option.
    /// Useful to process TryXX style functions.
    let inline tryParseWith func = func >> function
       | true, value -> Some value
       | false, _ -> None
    
    /// If true,value then returns Some value. Otherwise returns None.
    /// Useful to process TryXX style functions.
    let inline ofBoolAndValue b = 
        match b with
        | true,v -> Some v
        | _ -> None

    /// Maps Choice 1Of2 to Some value, otherwise None.
    let ofChoice =
        function
        | Choice1Of2 a -> Some a
        | _ -> None

    /// Gets the value associated with the option or the supplied default value.
    let inline getOrElse v =
        function
        | Some x -> x
        | None -> v

    /// Gets the value associated with the option or the supplied default value.
    let inline getOrElseLazy (v: _ Lazy) =
        function
        | Some x -> x
        | None -> v.Value

    /// Gets the value associated with the option or the supplied default value from a function.
    let inline getOrElseF v =
        function
        | Some x -> x
        | None -> v()

    /// Gets the value associated with the option or fails with the supplied message.
    let inline getOrFail m =
        function
        | Some x -> x
        | None -> failwith m

    /// Gets the value associated with the option or print to a string buffer and raise an exception with the given result. Helper printers must return strings.
    let inline getOrFailF fmt =
        function
        | Some x -> x
        | None -> failwithf fmt

    /// Gets the value associated with the option or raises the supplied exception.
    let inline getOrRaise e =
        function
        | Some x -> x
        | None -> raise e

    /// Gets the value associated with the option or reraises the supplied exception.
    let inline getOrReraise e =
        function
        | Some x -> x
        | None -> reraise' e

    /// Gets the value associated with the option or the default value for the type.
    let getOrDefault =
        function
        | Some x -> x
        | None -> Unchecked.defaultof<_>
    
    /// Gets the option if Some x, otherwise the supplied default value.
    let inline orElse v =
        function
        | Some x -> Some x
        | None -> v

    /// Applies a predicate to the option. If the predicate returns true, returns Some x, otherwise None.
    let inline filter pred =
        function
        | Some x when pred x -> Some x
        | _ -> None

    /// Attempts to cast an object. Returns None if unsuccessful.
    [<CompiledName("Cast")>]
    let inline cast (o: obj) =
        try
            Some (unbox o)
        with _ -> None

    let foldM f s = 
        Seq.fold (fun acc t -> acc >>= (flip f) t) (returnM s)
 
    let inline getOrElseWith v f =
        function
        | Some x -> f x
        | None -> v

    // Additional Option-Module extensions

    /// Haskell-style maybe operator
    let option (defaultValue : 'U) (map : 'T -> 'U) = function
        | None   -> defaultValue
        | Some a -> map a

    /// transforms a function in the Try...(input, out output) style
    /// into a function of type: input -> output Option
    /// Example: fromTryPattern(System.Double.TryParse)
    /// See Examples.Option
    let fromTryPattern (tryFun : ('input -> (bool * 'output))) =
        fun input ->
            match tryFun input with
            | (true,  output) -> Some output
            | (false,      _) -> None

    /// Concatenates an option of option.
    let inline concat x = 
        x >>= id

module Nullable =
    let (|Null|Value|) (x: _ Nullable) =
        if x.HasValue then Value x.Value else Null

    let create x = Nullable x
    /// Gets the value associated with the nullable or the supplied default value.
    let getOrDefault n v = match n with Value x -> x | _ -> v
    /// Gets the value associated with the nullable or the supplied default value.
    let getOrElse (n: Nullable<'T>) (v: Lazy<'T>) = match n with Value x -> x | _ -> v.Force()
    /// Gets the value associated with the Nullable.
    /// If no value, throws.
    let get (x: Nullable<_>) = x.Value
    /// Converts option to nullable
    let ofOption = Option.toNullable
    /// Converts nullable to option
    let toOption = Option.ofNullable
    /// Monadic bind
    let bind f x =
        match x with
        | Null -> Nullable()
        | Value v -> f v
    /// True if Nullable has value
    let hasValue (x: _ Nullable) = x.HasValue
    /// True if Nullable does not have value
    let isNull (x: _ Nullable) = not x.HasValue
    /// Returns 1 if Nullable has value, otherwise 0
    let count (x: _ Nullable) = if x.HasValue then 1 else 0
    /// Evaluates the equivalent of List.fold for a nullable.
    let fold f state x =
        match x with
        | Null -> state
        | Value v -> f state v
    /// Performs the equivalent of the List.foldBack operation on a nullable.
    let foldBack f x state =
        match x with
        | Null -> state
        | Value v -> f x state
    /// Evaluates the equivalent of List.exists for a nullable.
    let exists p x =
        match x with
        | Null -> false
        | Value v -> p x
    /// Evaluates the equivalent of List.forall for a nullable.
    let forall p x = 
        match x with
        | Null -> true
        | Value v -> p x
    /// Executes a function for a nullable value.
    let iter f x =
        match x with
        | Null -> ()
        | Value v -> f v
    /// Transforms a Nullable value by using a specified mapping function.
    let map f x =
        match x with
        | Null -> Nullable()
        | Value v -> Nullable(f v)
    /// Convert the nullable to an array of length 0 or 1.
    let toArray x = 
        match x with
        | Null -> [||]
        | Value v -> [| v |]
    /// Convert the nullable to a list of length 0 or 1.
    let toList x =
        match x with
        | Null -> []
        | Value v -> [v]
        
    /// Promote a function to a monad/applicative, scanning the monadic/applicative arguments from left to right.
    let lift2 f (a: _ Nullable) (b: _ Nullable) =
        if a.HasValue && b.HasValue
            then Nullable(f a.Value b.Value)
            else Nullable()

    let mapBool op a b =
        match a,b with
        | Value x, Value y -> op x y
        | _ -> false

    let inline (+?) a b = (lift2 (+)) a b
    let inline (-?) a b = (lift2 (-)) a b
    let inline ( *?) a b = (lift2 ( *)) a b
    let inline (/?) a b = (lift2 (/)) a b
    let inline (>?) a b = (mapBool (>)) a b
    let inline (>=?) a b = a >? b || a = b
    let inline (<?) a b = (mapBool (<)) a b
    let inline (<=?) a b = a <? b || a = b
    let inline notn (a: bool Nullable) = 
        if a.HasValue 
            then Nullable(not a.Value) 
            else Nullable()
    let inline (&?) a b = 
        let rec and' a b = 
            match a,b with
            | Null, Value y when not y -> Nullable(false)
            | Null, Value y when y -> Nullable()
            | Null, Null -> Nullable()
            | Value x, Value y -> Nullable(x && y)
            | _ -> and' b a
        and' a b

    let inline (|?) a b = notn ((notn a) &? (notn b))

    type Int32 with
        member x.n = Nullable x

    type Double with
        member x.n = Nullable x

    type Single with
        member x.n = Nullable x

    type Byte with
        member x.n = Nullable x

    type Int64 with
        member x.n = Nullable x

    type Decimal with
        member x.n = Nullable x

module State =

    type State<'T, 'State> = 'State -> 'T * 'State
    
    let getState = fun s -> (s,s)
    let putState s = fun _ -> ((),s)
    let eval m s = m s |> fst
    let exec m s = m s |> snd
    let empty = fun s -> ((), s)
    let bind k m = fun s -> let (a, s') = m s in (k a) s'
    
    /// The state monad.
    /// The algorithm is adjusted from my original work off of Brian Beckman's http://channel9.msdn.com/shows/Going+Deep/Brian-Beckman-The-Zen-of-Expressing-State-The-State-Monad/.
    /// The approach was adjusted from Matthew Podwysocki's http://codebetter.com/blogs/matthew.podwysocki/archive/2009/12/30/much-ado-about-monads-state-edition.aspx and mirrors his final result.
    type StateBuilder() =
        member this.Return(a) : State<'T,'State> = fun s -> (a,s)
        member this.ReturnFrom(m:State<'T,'State>) = m
        member this.Bind(m:State<'T,'State>, k:'T -> State<'U,'State>) : State<'U,'State> = bind k m
        member this.Zero() = this.Return ()
        member this.Combine(r1, r2) = this.Bind(r1, fun () -> r2)
        member this.TryWith(m:State<'T,'State>, h:exn -> State<'T,'State>) : State<'T,'State> =
            fun env -> try m env
                       with e -> (h e) env
        member this.TryFinally(m:State<'T,'State>, compensation) : State<'T,'State> =
            fun env -> try m env
                       finally compensation()
        member this.Using(res:#IDisposable, body) =
            this.TryFinally(body res, (fun () -> match res with null -> () | disp -> disp.Dispose()))
        member this.Delay(f) = this.Bind(this.Return (), f)
        member this.While(guard, m) =
            if not(guard()) then this.Zero() else
                this.Bind(m, (fun () -> this.While(guard, m)))
        member this.For(sequence:seq<_>, body) =
            this.Using(sequence.GetEnumerator(),
                (fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current))))
    let state = new StateBuilder()
    
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

    let foldM f s = 
        Seq.fold (fun acc t -> acc >>= (flip f) t) (returnM s)


    let inline sequence s =
        let inline cons a b = lift2 (fun x y -> x :: y) a b
        List.foldBack cons s (returnM [])

    let inline mapM f x = sequence (List.map f x)
        


module Reader =

    type Reader<'R,'T> = 'R -> 'T

    let bind k m = fun r -> (k (m r)) r
    
    /// The reader monad.
    /// This monad comes from Matthew Podwysocki's http://codebetter.com/blogs/matthew.podwysocki/archive/2010/01/07/much-ado-about-monads-reader-edition.aspx.
    type ReaderBuilder() =
        member this.Return(a) : Reader<'R,'T> = fun _ -> a
        member this.ReturnFrom(a:Reader<'R,'T>) = a
        member this.Bind(m:Reader<'R,'T>, k:'T -> Reader<'R,'U>) : Reader<'R,'U> = bind k m
        member this.Zero() = this.Return ()
        member this.Combine(r1, r2) = this.Bind(r1, fun () -> r2)
        member this.TryWith(m:Reader<'R,'T>, h:exn -> Reader<'R,'T>) : Reader<'R,'T> =
            fun env -> try m env
                       with e -> (h e) env
        member this.TryFinally(m:Reader<'R,'T>, compensation) : Reader<'R,'T> =
            fun env -> try m env
                       finally compensation()
        member this.Using(res:#IDisposable, body) =
            this.TryFinally(body res, (fun () -> match res with null -> () | disp -> disp.Dispose()))
        member this.Delay(f) = this.Bind(this.Return (), f)
        member this.While(guard, m) =
            if not(guard()) then this.Zero() else
                this.Bind(m, (fun () -> this.While(guard, m)))
        member this.For(sequence:seq<_>, body) =
            this.Using(sequence.GetEnumerator(),
                (fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current))))
    let reader = new ReaderBuilder()
    
    let ask : Reader<'R,'R> = id

    let asks f = reader {
        let! r = ask
        return (f r) }

    let local (f:'r1 -> 'r2) (m:Reader<'r2,'T>) : Reader<'r1, 'T> = f >> m
    
    open Operators
    
    /// Inject a value into the Reader type
    let inline returnM x = returnM reader x
    /// Sequentially compose two actions, passing any value produced by the first as an argument to the second.
    let inline (>>=) m f = bindM reader m f
    /// Flipped >>=
    let inline (=<<) f m = bindM reader m f
    /// Sequential application
    let inline (<*>) f m = applyM reader reader f m
    /// Sequential application
    let inline ap m f = f <*> m
    /// Transforms a Reader value by using a specified mapping function.
    let inline map f m = liftM reader f m
    /// Infix map
    let inline (<!>) f m = map f m
    /// Promote a function to a monad/applicative, scanning the monadic/applicative arguments from left to right.
    let inline lift2 f a b = returnM f <*> a <*> b
    /// Sequence actions, discarding the value of the first argument.
    let inline ( *>) x y = lift2 (fun _ z -> z) x y
    /// Sequence actions, discarding the value of the second argument.
    let inline ( <*) x y = lift2 (fun z _ -> z) x y
    /// Sequentially compose two reader actions, discarding any value produced by the first
    let inline (>>.) m f = bindM reader m (fun _ -> f)
    /// Left-to-right Kleisli composition
    let inline (>=>) f g = fun x -> f x >>= g
    /// Right-to-left Kleisli composition
    let inline (<=<) x = flip (>=>) x

    let foldM f s = 
        Seq.fold (fun acc t -> acc >>= (flip f) t) (returnM s)

module Undo =
    // UndoMonad on top of StateMonad
    open State
    
    let undoable = state
    
    type History<'T> = { 
        Current: 'T
        Undos : 'T list
        Redos : 'T list }
    
    let newHistory x = { Current = x; Undos = [x]; Redos = [] }
    let current history = history.Current
    
    let getHistory = getState
    
    let putToHistory x = undoable {
        let! history = getState
        do! putState  { Current = x; 
                        Undos = history.Current :: history.Undos
                        Redos = [] } }

    let exec m s = m s |> snd |> current
    
    let getCurrent<'T> = undoable {
        let! (history:'T History) = getState
        return current history}

    let combineWithCurrent f x = undoable {
        let! currentVal = getCurrent
        do! putToHistory (f currentVal x) }
    
    let undo<'T> = undoable {
        let! (history:'T History) = getState
        match history.Undos with
        | [] -> return false
        | (x::rest) -> 
            do! putState { Current = x;
                           Undos = rest;
                           Redos = history.Current :: history.Redos }
            return true}
    
    let redo<'T> = undoable {
        let! (history:'T History) = getState
        match history.Redos with
        | [] -> return false
        | (x::rest) -> 
            do! putState { Current = x;
                           Undos = history.Current :: history.Undos;
                           Redos = rest }
            return true }
