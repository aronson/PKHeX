// Intent: Validate parser model at runtime without missing anything that went wrong
module internal ResultLogic
// Unify the 
let resultFolder (state:Result<'a seq,string>) (item:Result<'a,string>) =
    match state, item with
    | Ok x,    Ok y -> Ok (seq {yield! x;yield y })
    | Error x, Ok y -> Error x
    | Ok _,    Error y -> Error y
    | Error x, Error y -> Error (sprintf "%s\r\n%s" x y)

// Standard boilerplate
let fold list =                
    (Ok Seq.empty, list) ||> Seq.fold resultFolder

// Lift Option to Result to make interfaces pretty later
let ofOption error = 
    // Make me some curry
    function 
    | Some s -> Ok s 
    // Require error datum when lifting an option to an erroneous result
    | None -> Error error

// Computation expression builder for the Result monad
type ResultBuilder() =
    // TODO: Comment all these members once I understand what the heck I was thinking seven months ago
    member __.Return(x) = Ok x

    member __.ReturnFrom(m: Result<_, _>) = m

    member __.Bind(m, f) = Result.bind f m
    member __.Bind((m, error): (Option<'T> * 'E), f) = m |> ofOption error |> Result.bind f

    member __.Zero() = None

    member __.Combine(m, f) = Result.bind f m

    member __.Delay(f: unit -> _) = f

    member __.Run(f) = f()

    member __.TryWith(m, h) =
        try __.ReturnFrom(m)
        with e -> h e

    member __.TryFinally(m, compensation) =
        try __.ReturnFrom(m)
        finally compensation()
