module Stack

type S<'State,'Value> =
    S of ('State -> 'Value * 'State)
let runS (S f) state = f state
let returnS x =
    let run state = x, state
    S run
let bindS f xS =
    let run state = 
        let x, newState = runS xS state
        runS (f x) newState
    S run
type StateBuilder()=
    member _.Return(x) = returnS x
    member _.ReturnFrom(xS) = xS
    member _.Bind(xS,f) = bindS f xS
let state = new StateBuilder()
type Stack<'a> = Stack of 'a list
// define pop outside of state expressions
let popStack (Stack contents) = 
    match contents with
    | [] -> failwith "Stack underflow"
    | head::tail ->     
        head, (Stack tail)
// define push outside of state expressions
let pushStack newTop (Stack contents) = 
    Stack (newTop::contents)
// define an empty stack
let emptyStack = Stack []
// get the value of the stack when run
let getS = 
    let run state = 
        // return the current state in the first element of the tuple
        state, state
    S run
let putS newState = 
    let run _ = 
        // return nothing in the first element of the tuple
        // return the newState in the second element of the tuple
        (), newState
    S run
let getValue stackM = 
    runS stackM emptyStack |> fst
let pop() = state {
    let! stack = getS
    let top, remainingStack = popStack stack
    do! putS remainingStack 
    return top }
let push newTop = state {
    let! stack = getS
    let newStack = pushStack newTop stack
    do! putS newStack 
    return () }
