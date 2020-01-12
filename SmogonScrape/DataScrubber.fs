module DataScrubber
open System.IO
open Config
type Pokemon = PKHeX.Core.PK8
// Get a clean PID, shiny if desired
let rec private rerollPID (pokemon:Pokemon) (wantShiny:bool) =
    pokemon.PID <- PKHeX.Core.PKX.GetRandomPID(
        pokemon.Species, pokemon.Gender, pokemon.Version, pokemon.Nature, 
        pokemon.AltForm, pokemon.PID)
    if pokemon.IsShiny = wantShiny then
        ()
    else 
        rerollPID pokemon wantShiny
// Get SAV 
let private savFile = new FileInfo(ConfigFile.Main.LocalTrainerSave.ToString())
let private sav = new PKHeX.Core.SAV8SWSH(File.ReadAllBytes(savFile.FullName))
// Assign trainerID
let private assignTrainerInfo (mon:Pokemon) =
    PKHeX.Core.Extensions.ApplyToPKM(sav, mon)
    // Current handler is owner; blank it out
    mon.CurrentHandler <- 0
// Update a pokemon's random vanity data to look legit and unique
let adoptAndScrub mon =
    // Mon is owned by trainer in local save file
    assignTrainerInfo mon
    // Scrub EC
    PKHeX.Core.CommonEdits.SetRandomEC(mon)
    // Scrub PID
    rerollPID mon true
