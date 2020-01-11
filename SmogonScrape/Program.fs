// Learn more about F# at https://fsharp.org
// See the 'F# Tutorial' project for more help.
open LocalDb
[<EntryPoint>]
let main argv =
    printfn "%A" argv
    let foo = LocalDb.legalPokemon
    0 // return an integer exit code
