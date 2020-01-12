module SmogonMoveset
open System.Text.RegularExpressions
open System.Linq

type Pokemon = PKHeX.Core.PK8
// Basic data types
type EVType = HP | ATK | DEF | SPA | SPD | SPE
type EV = EVType * int
// Smogon Moveset Format
type Meta = 
  { Species : int
    HeldItem : int
    Ability : int
    EVs : EV list
    Nature : int
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
    EVs = rawEVs pokemon
    Nature = pokemon.Nature
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
let clefableMeta = """Clefable @ Life Orb
Ability: Magic Guard
EVs: 196 HP / 252 SpA / 60 Spe
Modest Nature
- Moonblast
- Flamethrower
- Thunderbolt
- Moonlight"""
// Express lines of text in such an entry
type SmogonEntry = 
  { HeaderResult: Result<int * int>
    AbilityResult: Result<int>
    EffortValueResult: Result<EV list>
    NatureResult: Result<int>
    MoveResults: Result<int list> }
// A type fully parted
type ValidMeta = ValidMeta of Meta
// Regex active pattern to extract data as string array
let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

    
// Parse title line into (Species, Item)
let matchHeaderRow textIn : Result<(int * int)> = 
    match textIn with 
    | Regex @"\s?(\w+)\s?@\s?(.*)" [maybeSpecies; maybeHeldItem] ->
        match PKHeX.Core.SpeciesName.GetSpeciesID(maybeSpecies) with
        | -1 -> None // "No species for this string"
        // Valid species data
        | id -> Some id
        |>
        function
        | None -> Failure "Bad species"
        | Some speciesId ->
            let items = PKHeX.Core.Util.GetItemsList("en").ToList()
            match items.IndexOf(maybeHeldItem) with
            | -1 -> Failure "Bad item"
            | itemId -> Success (speciesId, itemId)
    | _ -> Failure "Failed header string regex match"
// Update header entry Meta
//let parseHeaderEntry textIn (currentMeta:Meta) : Result<Meta> =
//    match matchHeaderRow textIn with
//    | Some (speciesId, itemId) -> 
//        Success { currentMeta with Species = speciesId; HeldItem = itemId }
//    | None -> Failure "Invalid header entry"
let matchAbility text =
    match text with
    | Regex @"Ability: (.*)" [maybeAbility] ->
        let natures = PKHeX.Core.Util.GetAbilitiesList("en").ToList()
        match natures.IndexOf(maybeAbility) with
        | -1 -> Failure "Bad nature"
        | abilityId -> Success abilityId
    | _ -> Failure "InvalidHeaderEntry"
// Parse EV line into (number, EV string name)
let matchEvText =
    function
    | Regex @".?(\d+) (.+)" [maybeEffortValue; maybeStatName] -> 
        let maxEffortValue = 252
        let minEffortValue = 0
        // Is the first match really the stat integer?
        match System.Int32.TryParse(maybeEffortValue) with
        | true, effortValue when 
            effortValue <= maxEffortValue &&
            effortValue >= minEffortValue ->
            // Is it a standard stat?
            match maybeStatName.ToUpper() with
            | "HP" -> Some HP | "ATK" -> Some ATK | "DEF" -> Some DEF
            | "SPA" -> Some SPA | "SPD" -> Some SPD | "SPE" -> Some SPE
            | _ -> None
            |> 
            // It should be a valid effort value string
            function
            | Some ev -> EV(ev, effortValue) |> Success
            | None -> Failure "Invalid stat type string"
        // Wasn't an int!!
        | _ -> Failure "Invalid effort value"
    // uh oh
    | _ -> Failure "EV entry text yielded invalid regex match"
// Update moveset metadata with EV row
let parseEVs (text:string) : Result<EV list> =
    // Basic validation
    let prefix = @"EVs: "
    // Local result 
    // Split array into (number, EV name) strings
    let noPrefixString = new string(text.ToCharArray().[4..text.Length-1])
    let evStrings = noPrefixString.Split([|'/'|])
    // Run them through the parser
    evStrings |> Seq.map matchEvText
    // Only keep good results
    |> Seq.choose( 
        function
        | Success result -> Some result
        | _ -> None )
    |> Seq.toList
    |>
    function
    | [] -> Failure "Ivalid EV entry"
    | evList -> Success evList
let parseNature =
    function
    | Regex @"(.*) Nature" [maybeNature] -> 
        let list = PKHeX.Core.Util.GetNaturesList("en").ToList()
        match list.IndexOf(maybeNature) with
        | -1 -> Failure "No nature found"
        | natureId -> Success natureId
    | _ -> Failure "Nature entry text invalid regex match"
let matchMove (text:string) =
    let prefix = @"- "
    let list = PKHeX.Core.Util.GetMovesList("en").ToList()
    let moveString = new string(text.ToCharArray().[2..text.Length-1])
    match list.IndexOf(moveString) with
    | -1 -> Failure "invalid move text"
    | moveId -> Success moveId
let matchMoves =
    function
    | Success moveId -> Some moveId
    | _ -> None
let buildEntry (textLines:string list) =
    let result =
      { HeaderResult = (textLines.[0] |> matchHeaderRow)
        AbilityResult = textLines.[1] |> matchAbility
        EffortValueResult = textLines.[2] |> parseEVs
        NatureResult = textLines.[3] |> parseNature
        MoveResults = 
          match textLines.[4..7] |> List.choose (matchMove >> matchMoves) with
          | [] -> Failure "No moves!"
          | moves -> Success moves }
    match result with
    | { SmogonEntry.HeaderResult = (Success (species, heldItem))
        SmogonEntry.AbilityResult = (Success ability)
        SmogonEntry.EffortValueResult = (Success evList)
        SmogonEntry.NatureResult = (Success nature)
        SmogonEntry.MoveResults = (Success moves) } ->
          { Species = species; HeldItem = heldItem; Ability = ability;
            EVs = evList; Nature = nature;
            Move1 = moves.[0]; Move2 = moves.[1]
            Move3 = moves.[2]; Move4 = moves.[3] }
          |> Success 
    | _ -> Failure "Invalid entry"

let buildMeta (metaString:string) =
    metaString.Split([|'\r';'\n'|])
    |> Seq.choose(fun line -> if line.Length = 0 then None else Some line)
    |> Seq.toList
    |> buildEntry
    
