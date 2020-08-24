module MovesetParser
open System.Text.RegularExpressions
open SmogonMoveset
open Stack
open ResultLogic
open DataCache

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

// Regex active pattern to extract data as string array
let private (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

// Parse title line into (Species, Item)
type HeaderData = HeaderData of Species * HeldItem
let private parseHeader =
    function
    | RawHeader(Regex @"^(.*)\s@\s(.*)" [maybeSpecies; maybeHeldItem]) ->
        match PKHeX.Core.SpeciesName.GetSpeciesID(maybeSpecies) with
        | -1 -> None // "No species for this string"
        // Valid species data
        | id -> Some id
        |>
        function
        | None -> Error "Bad species"
        | Some speciesId ->
            match itemIds.TryGetValue(maybeHeldItem) with
            | false, _ -> Error "Bad item"
            | true, itemId -> Ok (HeaderData(Species speciesId, HeldItem itemId))
    | _ -> Error "Failed header string regex match"

// Lookup ability string to raw id
let private matchAbility =
    function
    | RawAbility(Regex @"Ability: (.*)" [maybeAbility]) ->
        match abilityIds.TryGetValue(maybeAbility) with
        | false, _ -> Error "Bad nature"
        | true, abilityId -> Ok (Ability abilityId)
    | _ -> Error "Invalid Header Entry"

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
            // If we got a stat at runtime wrap it into the EVType and call it a day
            function
            | Some ev -> EV(ev, effortValue) |> Ok
            | None -> Error "Invalid stat type string"
        // Wasn't an int!?
        | _ -> Error "Invalid effort value"
    // uh oh
    | _ -> Error "EV entry text yielded invalid regex match"

// Update moveset metadata with EV row with linefeed string
let private parseEVs (RawEVs text) : Result<EVs, string> =
    let prefix = @"EVs: "
    let strippedPrefixString = 
        new string(text.ToCharArray().[prefix.Length - 1 .. text.Length - 1])
    // Split array into (number, EV name) strings
    let evStrings = strippedPrefixString.Split([|'/'|])
    // Run them through the parser
    let results = Seq.map matchEvText evStrings
    match fold results with
    | Error f -> Error f
    | Ok ev -> Ok (EVs(Seq.toList ev))

let private parseNature =
    function
    | RawNature(Regex @"(.*) Nature" [maybeNature]) -> 
        match natureIds.TryGetValue(maybeNature) with
        | false, _ -> Error "No nature found"
        | true, natureId -> Ok (Nature natureId)
    | _ -> Error "Nature entry text invalid regex match"

let private matchMove (RawMoves moveStrings) =
    Seq.map
    <| function
        | Regex @"^- (.*)$" [maybeMove] ->
            // Fix up any quotes to PKHeX quote character
            let moveString = maybeMove.Replace("'","’")
            match moveIds.TryGetValue(moveString) with
            | false, _ -> Error "invalid move text"
            | true, moveId -> Ok moveId
        | _ -> Error "error parsing move text" 
    <| moveStrings
    // Result is an excellent monad
    |> fold |> Result.map (List.ofSeq >> Moves)

// HACK: I don't understand computation expressions and this point I'm glad it works
let private parseHeaderS = state {
        let! text = pop()
        return (parseHeader (RawHeader text)) }
let private parseAbilityS = state {
        let! text = pop()
        return matchAbility (RawAbility text) }
let private parseEVsS = state {
        let! text = pop()
        return parseEVs (RawEVs text) }
let private parseNatureS = state {
        let! text = pop()
        return parseNature (RawNature text) }
let private parseMoveS = state {
    let! move1 = pop()
    let! move2 = pop()
    let! move3 = pop()
    let! move4 = pop()
    let moves = [move1; move2; move3; move4]
    return matchMove (RawMoves moves) }
let private createMoveset (HeaderData(species, heldItem)) ability effort nature moves =
    { Species = species; HeldItem = heldItem; Ability = ability;
      EVs = effort; Nature = nature; Moves = moves }
let internal result = new ResultBuilder()
let private parseMoveset = state {
    let! header = parseHeaderS
    let! abilityResult = parseAbilityS
    let! effortResult = parseEVsS
    let! natureResult = parseNatureS
    let! moveResult = parseMoveS
    return result {
        let! res1 = header
        let! res2 = abilityResult
        let! res3 = effortResult
        let! res4 = natureResult
        let! res5 = moveResult
        return createMoveset res1 res2 res3 res4 res5 } }
let private buildMoveset (textLines:string list) =
    let (result, _) = runS parseMoveset (Stack textLines)
    result
// Take the contents of a Smogon moveset text and build a Moveset
let parseMeta (metaText:string) =
    // Fix CRLF in Windows files
    let movesetLines = metaText.Replace("\r\n", "\n").Split([|'\n'|]) |> Seq.toList
    // Split into newlines
    buildMoveset movesetLines
