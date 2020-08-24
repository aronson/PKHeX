// Intent: lift a smogon moveset string to a functional type at runtime
module SmogonMoveset

// Friendly type name for PKHeX data class
type private Pokemon = PKHeX.Core.PK8

[<Literal>]
// Example "export moveset" datum from smogon
let private exampleInput = """Clefable @ Life Orb
Ability: Magic Guard
EVs: 196 HP / 252 SpA / 60 Spe
Modest Nature
- Moonblast
- Flamethrower
- Thunderbolt
- Moonlight"""
// ^^ TODO: account for nicknames when exporting pokemon from pokemon showdown

// Different lines in each smogon datum represent different things
type RawHeader = RawHeader of string
type RawAbility = RawAbility of string
type RawEVs = RawEVs of string 
type RawNature = RawNature of string
type RawMoves = RawMoves of string list

// Well-formed movesets can be composed of these parts at runtime
type Species = Species of int
type HeldItem = HeldItem of int
type Ability = Ability of int
type Nature = Nature of int
type Moves = Moves of int list

// EVs have several unique sub-types
type EVType = HP | ATK | DEF | SPA | SPD | SPE
// EVs at runtime as an algebraic type
type EV = EVType * int
// A tradeoff here between guaranteed uniqueness and an easier to write model
type EVs = EVs of EV list

// Express the competitive aspects of a pokemon
type Moveset = 
  { Species: Species
    HeldItem: HeldItem
    Ability: Ability
    EVs: EVs    
    Nature: Nature
    Moves: Moves }

// Assign new EVs to a pokemon
let private assignEV (effortValue: EV) (pokemon: Pokemon) =
    // Unpack EVType monad to meaningful data
    let (evType, value) = effortValue
    // Mutate the correct field of the pokemon object for this type of EV
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
    // TODO: Investigate whether or not having other PP up values is meaningful or useful
    PKHeX.Core.CommonEdits.SetMoves(pokemon, Array.ofList moves, true)

    // Set perfect IVs
    for ivIndex in [0..5] do
        PKHeX.Core.CommonEdits.SetIV(pokemon, ivIndex, 31)
    // TODO: Don't set IVs twice just in case, probably only need one of these
    let perfectIVs = [|31us; 31us; 31us; 31us; 31us; 31us|]
    pokemon.SetStats(perfectIVs)

    // Clear EVs
    for evIndex in [0..5] do
        PKHeX.Core.CommonEdits.SetEV(pokemon, evIndex, 0)
    // Assign meta EVs
    let (EVs effortValues) = meta.EVs
    effortValues |> Seq.iter (fun ev -> assignEV ev pokemon)

    // Mutate misc. vanity data unrelated to moveset
    let (Nature nature) = meta.Nature
    pokemon.Nature <- nature
    pokemon.CurrentFriendship <- 255
