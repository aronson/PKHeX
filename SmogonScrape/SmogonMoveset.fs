module SmogonMoveset
open System.Text.RegularExpressions
type Pokemon = PKHeX.Core.PK8
// Basic data types
type EVType = HP | ATK | DEF | SPA | SPD | SPE
type EV = EVType * int
// Smogon Moveset Format
type Meta = 
  { Species : int
    HeldItem : int
    Ability : int
    Nature : int
    EVs : EV list
    Move1: int
    Move2: int
    Move3: int
    Move4: int }
// Algebraic type for a pokemon with a moveset from Smogon
type SmogonMoveset = Pokemon * Meta
// Represent all EVs of pokemon
let rawEVs (pokemon:Pokemon) = 
  [ EV(HP, pokemon.EV_HP);
    EV(ATK, pokemon.EV_ATK);
    EV(DEF, pokemon.EV_DEF);
    EV(SPA, pokemon.EV_SPA);
    EV(SPD, pokemon.EV_SPD);
    EV(SPE, pokemon.EV_SPE); ]
// Create Meta record from a given pokemon
let getMeta (pokemon: Pokemon) =
  { Species = pokemon.Species
    HeldItem = pokemon.HeldItem
    Ability = pokemon.Ability
    Nature = pokemon.Nature
    EVs = rawEVs pokemon
    Move1 = pokemon.Move1
    Move2 = pokemon.Move2
    Move3 = pokemon.Move3
    Move4 = pokemon.Move4 }
// Wrap it in the SmogonMoveset type for monadic functions
let getMoveset (pokemon: Pokemon) : SmogonMoveset = 
    SmogonMoveset(pokemon, getMeta pokemon)

(* ==== Smogon Website Text Parser begins ==== *)

// Text parser result
type Result<'a> =
    | Success of 'a
    | Failure of string 
// Example export moveset data from smogon
[<Literal>]
let clefableMeta = """
Clefable @ Life Orb
Ability: Magic Guard
EVs: 196 HP / 252 SpA / 60 Spe
Modest Nature
- Moonblast
- Flamethrower
- Thunderbolt
- Moonlight"""
// Regex active pattern to extract data as string array
let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None
// Parse EV line into (number, EV string name)
let matchEvText text =
    match text with
    | Regex @"\s?(\d+)\w(.+)" [maybeEffortValue; maybeStatName] -> 
        let maxEffortValue = 252
        let minEffortValue = 0
        // Is the first match really the stat integer?
        match System.Int32.TryParse(maybeEffortValue) with
        | true, effortValue when 
            effortValue <= maxEffortValue &&
            effortValue >= minEffortValue ->
            // Is it a standard stat?
            match maybeStatName with
            | "HP" -> Some HP
            | "ATK" -> Some ATK
            | "DEF" -> Some DEF
            | "SpA" -> Some SPA
            | "SpD" -> Some SPD
            | "SPE" -> Some SPE
            | _ -> None
            |> 
            // It should be a valid effort value string
            function
            | Some ev -> EV(ev, effortValue) |> Success
            | None -> Failure "bad stat type string!"
        // Wasn't an int!!
        | _ -> Failure "bad effort value!"
    // Can't parse string per expected regex!
    | _ -> Failure "uhh oh stiiinky!! bad string no value now that's funny"
let parseEVs (evTextLine:string) (currentMeta:Meta) : Meta =
    // Basic validation
    let prefix = "EVs: "
    // Local result 
    let parsedEVs =
        // Split array into (number, EV name) strings
        evTextLine.Remove(prefix.Length).Split('/')
        // Run them through the parser
        |> Seq.map matchEvText
        // Only keep good results
        |> Seq.choose( 
            function
            | Success result -> Some result
            | Failure _ -> None)
    // Return the updated meta
    {currentMeta with EVs = Seq.toList parsedEVs }
