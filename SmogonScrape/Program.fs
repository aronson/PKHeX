// Learn more about F# at https://fsharp.org
// See the 'F# Tutorial' project for more help.
open System.IO
open LocalDb
open SmogonMoveset
[<EntryPoint>]
let main _ =
    let inputMons = 
        let readFileString (file:FileInfo) =
            use foo = file.OpenText()
            foo.ReadToEnd()
        // Read all raw text files (assume format is correct for each file
        let dir = Config.ConfigFile.CurrentMeta.LocalMetaFilesDir.ToString()
        (new DirectoryInfo(dir)).
            EnumerateFiles()
        |> Seq.map readFileString |> Seq.toList
    let destDirectory = 
        new DirectoryInfo(Config.ConfigFile.CurrentMeta.LocalMetaOutDir.ToString())
    let writeMeta metaText =
        match SmogonMoveset.buildMeta metaText with
        | Success meta ->
            let (LegalPokemon pokemon) =
                LocalDb.legalPokemon
                |> Seq.find ( fun mon ->
                    let (LegalPokemon poke) = mon
                    poke.Species = meta.Species && poke.EVTotal >= 508 )
            SmogonMoveset.assignMeta meta pokemon
            DataScrubber.adoptAndScrub pokemon
            let analysis = new PKHeX.Core.LegalityAnalysis(pokemon)
            let invalidResults = 
                analysis.Results |> Seq.where(fun r -> not r.Valid ) |> Seq.toList
            if invalidResults.Length > 1 then
                failwith "Not legal"
            let destDir = destDirectory.FullName.ToString()
            let speciesName = PKHeX.Core.SpeciesName.GetSpeciesName(meta.Species, 2)
            let destFile = 
                new FileInfo(
                    Path.Combine(destDir,
                        sprintf "%s - %d.pk8" speciesName pokemon.PID))
            use writer = new BinaryWriter(File.Create(destFile.FullName))
            writer.Write(pokemon.DecryptedPartyData)
            writer.Flush()
        | _ -> failwith "uh oh"
    List.iter writeMeta inputMons
    0
