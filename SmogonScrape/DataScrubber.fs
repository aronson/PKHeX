// Intent: Cloned legit pokemon have potentially identifiable data -- make it plausibly different
module DataScrubber
open System.IO
open Config

// TODO: Support gen formats other than Gen 8
type Pokemon = PKHeX.Core.PK8

// Get a clean PID, shiny if desired
let rec private rerollPID (pokemon:Pokemon) (wantShiny:bool) =
    // Update pokemon with another plausible PID based on state
    pokemon.PID <- PKHeX.Core.PKX.GetRandomPID(
        pokemon.Species, pokemon.Gender, pokemon.Version, pokemon.Nature, 
        pokemon.AltForm, pokemon.PID)
    // TODO: An if statement and tail recursion? Surely there's a better way...
    if pokemon.IsShiny = wantShiny then
        ()
    else 
        // ...at least it's cheap
        rerollPID pokemon wantShiny

// Reference user save file at runtime
let private savFile = new FileInfo(ConfigFile.Main.LocalTrainerSave.ToString())

// Long variable names are for people who don't have time to make them short
let private sav = 
    // TODO: this can fail so hard, IO monad when?
    new PKHeX.Core.SAV8SWSH(File.ReadAllBytes(savFile.FullName))

// Using the trainer save file from runtime, apply that OT and associated data to 
let private assignTrainerInfo (mon:Pokemon) =
    // Thank goodness someone did extensive work for us
    PKHeX.Core.Extensions.ApplyToPKM(sav, mon)
    // Current handler is owner; blank it out for correctness
    mon.CurrentHandler <- 0

// Update a pokemon's random vanity data to appear legit and unique
let adoptAndScrub mon =
    // Mon is owned by trainer in local save file
    assignTrainerInfo mon
    // Scrub EC
    PKHeX.Core.CommonEdits.SetRandomEC(mon)
    // Scrub PID
    rerollPID mon true
