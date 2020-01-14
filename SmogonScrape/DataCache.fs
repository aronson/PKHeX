module internal DataCache
open System.Linq
open System.Collections.Generic

let itemIds =
    let items = PKHeX.Core.Util.GetItemsList("en").ToList()
    let item = new Dictionary<string, int>()
    let mutable index = 0
    for i in items do
        if not (item.ContainsKey(i)) then
            item.Add(i, index)
        index <- index + 1
    item
let abilityIds = 
    let abilities = 
        PKHeX.Core.Util.GetAbilitiesList("en").ToList()
    let abil = new Dictionary<string, int>()
    let mutable index = 0
    for a in abilities do
        if not (abil.ContainsKey(a)) then
            abil.Add(a, index)
        index <- index + 1
    abil
let natureIds = 
    let natures = PKHeX.Core.Util.GetNaturesList("en").ToList()
    let abil = new Dictionary<string, int>()
    let mutable index = 0
    for a in natures do
        if not (abil.ContainsKey(a)) then
            abil.Add(a, index)
        index <- index + 1
    abil
let moveIds = 
    let moves = PKHeX.Core.Util.GetMovesList("en").ToList()
    let abil = new Dictionary<string, int>()
    let mutable index = 0
    for a in moves do
        if not (abil.ContainsKey(a)) then
            abil.Add(a, index)
        index <- index + 1
    abil
