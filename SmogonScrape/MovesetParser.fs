module MovesetParser
open System.Linq
open System.Text.RegularExpressions
open SmogonMoveset
// Text parser result
type Result<'a> =
    | Success of 'a
    | Failure of string 
// Example export moveset data from smogon
[<Literal>]
let private exampleInput = """Clefable @ Life Orb
Ability: Magic Guard
EVs: 196 HP / 252 SpA / 60 Spe
Modest Nature
- Moonblast
- Flamethrower
- Thunderbolt
- Moonlight"""
// Express lines of text in such an entry
type private SmogonEntry = 
  { HeaderResult: Result<int * int>
    AbilityResult: Result<int>
    EffortValueResult: Result<EV list>
    NatureResult: Result<int>
    MoveResults: Result<int list> }
// Regex active pattern to extract data as string array
let private (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None
// Cache all items
let private items = PKHeX.Core.Util.GetItemsList("en").ToList()
// Parse title line into (Species, Item)
let private matchHeaderRow =
    function
    | Regex @"\s?(\w+)\s?@\s?(.*)\s?" [maybeSpecies; maybeHeldItem] ->
        match PKHeX.Core.SpeciesName.GetSpeciesID(maybeSpecies) with
        | -1 -> None // "No species for this string"
        // Valid species data
        | id -> Some id
        |>
        function
        | None -> Failure "Bad species"
        | Some speciesId ->
            match items.IndexOf(maybeHeldItem) with
            | -1 -> Failure "Bad item"
            | itemId -> Success (speciesId, itemId)
    | _ -> Failure "Failed header string regex match"
// Cache all abilities
let private abilities = PKHeX.Core.Util.GetAbilitiesList("en").ToList()
// Lookup ability string to raw id
let private matchAbility =
    function
    | Regex @"Ability: (.*)" [maybeAbility] ->
        match abilities.IndexOf(maybeAbility) with
        | -1 -> Failure "Bad nature"
        | abilityId -> Success abilityId
    | _ -> Failure "InvalidHeaderEntry"
// Parse one EV line into (number, EV string name)
let private matchEvText =
    function
    | Regex @"\s?(\d+) (\w+)\s?" [maybeEffortValue; maybeStatName] -> 
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
let private parseEVs (text:string) : Result<EV list> =
    let prefix = @"EVs: "
    let strippedPrefixString = 
        new string(text.ToCharArray().[prefix.Length - 1 .. text.Length - 1])
    // Split array into (number, EV name) strings
    let evStrings = strippedPrefixString.Split([|'/'|])
    // Run them through the parser
    evStrings |> Seq.map matchEvText
    // Only keep good results
    |> Seq.choose( 
        function
        | Success result -> Some result
        | _ -> None )
    |> Seq.toList
    |> function
    | [] -> Failure "Invalid EV entry"
    | evList -> Success evList
let private parseNature =
    function
    | Regex @"(.*) Nature" [maybeNature] -> 
        let list = PKHeX.Core.Util.GetNaturesList("en").ToList()
        match list.IndexOf(maybeNature) with
        | -1 -> Failure "No nature found"
        | natureId -> Success natureId
    | _ -> Failure "Nature entry text invalid regex match"
let private matchMove (text:string) =
    let prefix = @"- "
    let list = PKHeX.Core.Util.GetMovesList("en").ToList()
    let moveString = new string(text.ToCharArray().[2..text.Length-1])
    match list.IndexOf(moveString) with
    | -1 -> Failure "invalid move text"
    | moveId -> Success moveId
let private matchMoves =
    function
    | Success moveId -> Some moveId
    | _ -> None
let private buildMoveset (textLines:string list) =
    // Run various parsers
    { HeaderResult = (textLines.[0] |> matchHeaderRow)
      AbilityResult = textLines.[1] |> matchAbility
      EffortValueResult = textLines.[2] |> parseEVs
      NatureResult = textLines.[3] |> parseNature
      MoveResults = 
        match textLines.[4..7] |> List.choose (matchMove >> matchMoves) with
        | [] -> Failure "No moves!"
        | moves -> Success moves }
    // If all parsers were successful, the MoveSet is valid
    |> function
    | { SmogonEntry.HeaderResult = (Success (species, heldItem))
        SmogonEntry.AbilityResult = (Success ability)
        SmogonEntry.EffortValueResult = (Success evList)
        SmogonEntry.NatureResult = (Success nature)
        SmogonEntry.MoveResults = (Success moves) }
        // Stitch together valid Moveset
        ->{ Species = species; HeldItem = heldItem; Ability = ability;
            EVs = evList; Nature = nature;
            Move1 = moves.[0]; Move2 = moves.[1]
            Move3 = moves.[2]; Move4 = moves.[3] }
          |> Success 
    | _ -> Failure "Invalid entry"
// Take the contents of a Smogon moveset text and build a Moveset
let parseMeta (metaText:string) =
    // Fix CRLF in Windows files
    let strippedText = metaText.Replace("\r\n", "\n")
    // Split into newlines
    strippedText.Split([|'\n'|])
    // Strip empty lines
    |> Seq.choose(fun line -> if line.Length <> 0 then Some line else None)
    |> Seq.toList
    |> buildMoveset
