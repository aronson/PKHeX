open System.IO
open System.Linq 
open LocalDb
open SmogonMoveset
open MovesetParser
[<EntryPoint>]
let main _ =
    // All raw smogon meta build strings
    let inputMons = 
        let readFileString (file:FileInfo) =
            use foo = file.OpenText()
            (file.Name.Replace(".pk8", ""), foo.ReadToEnd())
        // Read all raw text files (assume format is correct for each file
        let dir = Config.ConfigFile.CurrentMeta.LocalMetaFilesDir.ToString()
        (new DirectoryInfo(dir)).
            EnumerateFiles()
        |> Seq.map readFileString |> Seq.toList
    // Where to write modded pokemon files after they are built
    let destDirectory = 
        new DirectoryInfo(Config.ConfigFile.CurrentMeta.LocalMetaOutDir.ToString())
    // For quick lookup in a batch edit scenario
    let localLookup = LocalDb.legalPokemon.ToLookup(fun (LegalPokemon mon) -> mon.Species)
    // Create a legal .pk8 file given Smogon build input
    let writeMeta (monName, metaText) =
        // Parse build text into valid Meta record type
        match MovesetParser.parseMeta metaText with
        | Ok meta ->
            // Find first pokemon matching desired species from local legit collection
            let (Species spId) = meta.Species
            let (LegalPokemon pokemon) = localLookup.[spId].Last()
            // Apply Smogon meta build to previously-legal pokemon copy
            SmogonMoveset.assignMoveset meta pokemon
            // Reroll PokemonId, EncryptionConstant, and apply personal Trainer Info
            DataScrubber.adoptAndScrub pokemon
            // Check if pokemon is still legal in PKHeX
            let analysis = new PKHeX.Core.LegalityAnalysis(pokemon)
            let invalidResults = 
                analysis.Results |> Seq.where(fun r -> not r.Valid ) |> Seq.toList
            // Not legal!! 
            if invalidResults.Length <> 0 then
                failwith "Not legal"
            // Create destination .pk8 file and name 
            let destFile = 
                new FileInfo(
                    Path.Combine(destDirectory.FullName,
                        sprintf "%s - %d.pk8" monName pokemon.PID))
            // Write updated Pokemon file to import to PKHeX
            use writer = new BinaryWriter(File.Create(destFile.FullName))
            writer.Write(pokemon.DecryptedPartyData)
            writer.Flush()
        // If we got here, the meta format is likely broken
        | _ -> failwith "uhhh ohh st-"
    // Iterate over all Smogon moveset files in input dir and write to out dir
    List.iter writeMeta inputMons
    0
