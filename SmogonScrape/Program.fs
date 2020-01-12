// Learn more about F# at https://fsharp.org
// See the 'F# Tutorial' project for more help.
open LocalDb
open SmogonMoveset
[<EntryPoint>]
let main argv =
    printfn "%A" argv
    let metaMons =
      [ SmogonMoveset.clefableLifeOrb, "C:\Users\i\Desktop\PKHeX\Life Orb.pk8";
        SmogonMoveset.clefableUtility, "C:\Users\i\Desktop\PKHeX\Utility.pk8" ]
    let writeMeta (metaText, dest) =
        match SmogonMoveset.buildMeta metaText with
        | Success meta ->
            let (LegalPokemon firstClefable) =
                LocalDb.legalPokemon
                |> Seq.find ( fun mon ->
                    let (LegalPokemon poke) = mon
                    poke.Species = meta.Species )
            SmogonMoveset.assignMeta meta firstClefable
            DataScrubber.adoptAndScrub firstClefable
            let analysis = new PKHeX.Core.LegalityAnalysis(firstClefable)
            let invalidResults = 
                analysis.Results |> Seq.where(fun r -> not r.Valid ) |> Seq.toList
            if invalidResults.Length > 1 then
                failwith "Not legal"
            System.IO.File.WriteAllBytes(dest, firstClefable.DecryptedPartyData)
        | _ -> failwith "uh oh"
    List.iter writeMeta metaMons
    0
