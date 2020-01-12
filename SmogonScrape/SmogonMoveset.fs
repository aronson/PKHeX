module SmogonMoveset
// Friendly type name for PKHeX data class
type private Pokemon = PKHeX.Core.PK8
// EVs have several unique sub-types
type EVType = HP | ATK | DEF | SPA | SPD | SPE
type EV = EVType * int
// Smogon Moveset Format
type Moveset = 
  { Species: int
    HeldItem: int
    Ability: int
    EVs: EV list
    Nature: int
    Move1: int
    Move2: int
    Move3: int
    Move4: int }
// Assign new EVs to pokemon
let private assignEV (effortValue: EV) (pokemon: Pokemon) =
    let (evType, value) = effortValue
    match evType with
    | HP -> pokemon.EV_HP <- value
    | ATK -> pokemon.EV_ATK <- value
    | DEF -> pokemon.EV_DEF <- value
    | SPA -> pokemon.EV_SPA <- value
    | SPD -> pokemon.EV_SPD <- value
    | SPE -> pokemon.EV_SPE <- value
// Write moveset data to a pokemon
let assignMoveset (meta: Moveset) (pokemon: Pokemon) =
    // Edit raw data with helper functions from PKHeX library
    PKHeX.Core.CommonEdits.SetAbility(pokemon, meta.Ability)
    PKHeX.Core.CommonEdits.SetNature(pokemon, meta.Nature)
    PKHeX.Core.CommonEdits.ApplyHeldItem(pokemon, meta.HeldItem, pokemon.Version)
    // Set all possible technical records as learned
    PKHeX.Core.CommonEdits.SetRecordFlags(pokemon)
    // Set all moves with max PP ups
    PKHeX.Core.CommonEdits.SetMoves(pokemon, 
        [|meta.Move1; meta.Move2; meta.Move3; meta.Move4|], true)
    // Miscellaneous and vanity data
    pokemon.Nature <- meta.Nature
    pokemon.CurrentFriendship <- 255
    // Set perfect IVs
    for ivIndex in [0..5] do
        PKHeX.Core.CommonEdits.SetIV(pokemon, ivIndex, 31)
    // TODO: Do it again just in case?
    let perfectIVs = [|31us; 31us; 31us; 31us; 31us; 31us|]
    pokemon.SetStats(perfectIVs)
    // Clear EVs
    for evIndex in [0..5] do
        PKHeX.Core.CommonEdits.SetEV(pokemon, evIndex, 0)
    // Assign meta EVs
    meta.EVs |> Seq.iter (fun ev -> assignEV ev pokemon)

