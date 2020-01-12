// Learn more about F# at https://fsharp.org
// See the 'F# Tutorial' project for more help.
open LocalDb
open SmogonMoveset
[<EntryPoint>]
let main argv =
    printfn "%A" argv
    let foo = LocalDb.legalPokemon
    let clefable = SmogonMoveset.buildMeta SmogonMoveset.clefableMeta
    0 // return an integer exit code
