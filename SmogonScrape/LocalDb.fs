// Intent: map pokemon within local PKHeX database into object model, then wrap into monadic model
module LocalDb
open System.IO
open Config
// Functional type name for PKHeX data class
type PKM = PKHeX.Core.PK8
// Desktop PKHeX pkdb folder
let localDirectory = new DirectoryInfo(ConfigFile.Main.DatabaseDirectory.ToString())
// FileInfos for each .pkm8 file
let localFileInfos = localDirectory.EnumerateFiles()

// Lift set of pokemon files to Seq
let pokemonFileSequence : FileInfo seq =
    localFileInfos
    |> Seq.cast<FileInfo>

// They're definitely not legit pokemon at runtime yet; I got them from my uncle nintendo
type DubiousPokemon = DubiousPokemon of PKM

// A pokemon that seems plausibly legit, enough for tournament play
type LegalPokemon = LegalPokemon of PKM

// Parse raw file data into PKMN classes at runtime, then add functional wrapper type
let liftPokemonD (file:FileInfo) =
    // TODO: IO monad when?
    new PKM(File.ReadAllBytes(file.FullName))
    |> DubiousPokemon

// Express sequence of all local, "legit" pokemon in the local pkdb at runtime
let localPokemon : DubiousPokemon seq =
    pokemonFileSequence
    // HACK: Gotta load 'em all!
    |> Seq.map liftPokemonD

// Lift a dubious pokemon to a legal pokemon if and only if it passes the PKHeX validator
let liftPokemonL dubiousPokemon : LegalPokemon option =
    // Right now this dubious pokemon is just a plain old pokemon object
    let (DubiousPokemon pokemon) = dubiousPokemon
    // Test for a legal pokemon
    let analysis = new PKHeX.Core.LegalityAnalysis(pokemon)
    // If it passes the analysis it's a legal pokemon for now
    if analysis.Valid then Some (LegalPokemon pokemon)
    // Otherwise, dubious for a reason
    else None

// Given all local pokemon at runtime, return all legal/legit pokemon at runtime
let liftPokemon : LegalPokemon seq =
    Seq.choose liftPokemonL localPokemon
