module LocalDb
open System.IO
open Config
type PKM = PKHeX.Core.PK8
// Desktop PKHeX pkdb folder

let localDirectory = new DirectoryInfo(ConfigFile.Main.DatabaseDirectory.ToString())

// FileInfos for each .pkm8 file
let localFileInfos = localDirectory.EnumerateFiles()

// All files as Seq
let pokemonFileSequence : FileInfo seq =
    localFileInfos
    |> Seq.cast<FileInfo>

// Raw legit pokemon data type
type LegitPokemon = LegitPokemon of PKM
// Function to parse raw file data into PKMN class
let getPKMN (file:FileInfo) =
    new PKM(File.ReadAllBytes(file.FullName))
    |> LegitPokemon
// Construct Seq of LegitPokemon
let localPokemon =
    pokemonFileSequence
    |> Seq.map getPKMN

// Legal Pokemon data type
type LegalPokemon = LegalPokemon of PKM
// Raise to legal pokemon if it passes the validator
let makeLegal pokemon : LegalPokemon option =
    let (LegitPokemon legitPokemon) = pokemon
    let analysis = new PKHeX.Core.LegalityAnalysis(legitPokemon)
    if analysis.Valid then Some (LegalPokemon legitPokemon)
    else None
// All local, legal, legit pokemon
let legalPokemon : LegalPokemon seq =
    Seq.choose makeLegal localPokemon
