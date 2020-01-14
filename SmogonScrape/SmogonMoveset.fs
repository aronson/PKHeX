module SmogonMoveset
// Friendly type name for PKHeX data class
type private Pokemon = PKHeX.Core.PK8
// EVs have several unique sub-types
type EVType = HP | ATK | DEF | SPA | SPD | SPE
type EV = EVType * int
// Express lines of text in such an entry
type RawHeader = RawHeader of string
type RawAbility = RawAbility of string
type RawEVs = RawEVs of string 
type RawNature = RawNature of string
type RawMoves = RawMoves of string list
// Express parsed entries
type Species = Species of int
type HeldItem = HeldItem of int
type Ability = Ability of int
type EVs = EVs of EV list
type Nature = Nature of int
type Moves = Moves of int list
// Smogon Moveset Format
type Moveset = 
  { Species: Species
    HeldItem: HeldItem
    Ability: Ability
    EVs: EVs    
    Nature: Nature
    Moves: Moves }
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
    let (Ability ability) = meta.Ability
    PKHeX.Core.CommonEdits.SetAbility(pokemon, ability)
    let (Nature nature) = meta.Nature
    PKHeX.Core.CommonEdits.SetNature(pokemon, nature)
    let (HeldItem item) = meta.HeldItem
    PKHeX.Core.CommonEdits.ApplyHeldItem(pokemon, item, pokemon.Version)
    // Set all possible technical records as learned
    PKHeX.Core.CommonEdits.SetRecordFlags(pokemon)
    // Set all moves with max PP ups
    let (Moves moves) = meta.Moves
    PKHeX.Core.CommonEdits.SetMoves(pokemon, Array.ofList moves, true)
    // Miscellaneous and vanity data
    let (Nature nature) = meta.Nature
    pokemon.Nature <- nature
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
    let (EVs effortValues) = meta.EVs
    effortValues |> Seq.iter (fun ev -> assignEV ev pokemon)
