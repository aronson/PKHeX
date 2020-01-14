module internal ResultLogic

let resultFolder (state:Result<'a seq,string>) (item:Result<'a,string>) =
    match state,item with
    |Ok x,Ok y -> Ok (seq {yield! x;yield y })
    |Error x, Ok y -> Error x
    |Ok _, Error y -> Error y
    |Error x,Error y -> Error (sprintf "%s\r\n%s" x y)   
let fold list =                
    (Ok Seq.empty,list) ||> Seq.fold resultFolder
let ofOption error = function Some s -> Ok s | None -> Error error
type ResultBuilder() =
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
