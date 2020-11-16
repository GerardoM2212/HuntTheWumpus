// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
let actuator = [">";"v";"<";"^"]
let mutable lookingAt = 0
let map = ["┌───┬───┬───┬───┐";
           "│ ? │ ? │ ? │ ? │";
           "│   │   │   │   │";
           "├───┼───┼───┼───┤";
           "│ ? │ ? │ ? │ ? │";
           "│   │   │   │   │";
           "├───┼───┼───┼───┤";
           "│ ? │ ? │ ? │ ? │";
           "│   │   │   │   │";
           "├───┼───┼───┼───┤";
           "│   │ ? │ ? │ ? │";
           "│   │   │   │   │";
           "└───┴───┴───┴───┘";]
let bounds = [["IZ|AR"; "AR"; "AR"; "AR|DER"];
            ["IZ"; "NON"; "NON"; "DER"];
            ["IZ"; "NON"; "NON"; "DER"];
            ["IZ|AB"; "AB"; "AB"; "AB|DE"];]
let coords = [[[2; 2]; [6; 2]; [10; 2]; [14; 2]];
            [[2; 5]; [6; 5]; [10; 5]; [14; 5]];
            [[2; 8]; [6; 8]; [10; 8]; [14; 8]];
            [[2; 11]; [6; 11]; [10; 11]; [14; 11]];]
let mutable wumpus = [0; 0]
let mutable gold = [0; 0]
let mutable isGameOver = false
let mutable wasGoldGrabbed = false
let mutable wasWumpusKilled = false
let pit = []
let mutable pitLoc = []
let mutable cordHist = []

let mutable simb:list<string> = []


let grabGold(cordAct:list<int>) =
    let mutable newSimb:list<string> = []
    for x in 0 .. simb.Length - 1 do
        if simb.[x].IndexOf("G") = 1 then
            newSimb <- newSimb @ [simb.[x].Replace("G"," ")]
        else
            newSimb <- newSimb @ [simb.[x]]
    simb <- newSimb
    wasGoldGrabbed <- true
let wumpusKilled(cordAct:list<int>) =
    let mutable newSimb:list<string> = []
    for x in 0 .. simb.Length - 1 do
        if simb.[x].IndexOf("S") = 2 then
            newSimb <- newSimb @ [simb.[x].Replace("S"," ")]
        else
            newSimb <- newSimb @ [simb.[x]]
    simb <- newSimb
let uncover(cordHist:list<list<int>>) (simb:list<string>) =
    for x in 0 .. cordHist.Length - 1 do
        Console.SetCursorPosition(cordHist.[x].[0] - 1, cordHist.[x].[1] - 1)
        printf "%s" simb.[x]

let visited(cordAct:list<int>)(simbol:string) =
    let mutable isRepeated = false
    if cordHist.Length = 0 then
        cordHist <- cordHist @ [cordAct]
        simb <- simb @ [simbol]
    else
        for x in 0 .. cordHist.Length - 1 do
            if cordHist.[x] = cordAct then
                isRepeated <- true
        if not isRepeated then
            cordHist <- cordHist @ [cordAct]
            simb <- simb @ [simbol]
    Console.SetCursorPosition(0, 21)
    // printf "Hist: %A" cordHist
    uncover (cordHist) (simb)
let checkSensors(cordAct:list<int>) =
    let mutable  surround:list<list<int>> = []
    let mutable smellSensed = " "
    let mutable breezeSensed = " "
    let mutable goldSensed = " "
    if cordAct.[0] <> 2 then
        let meter = [cordAct.[0] - 4; cordAct.[1] ]
        surround <- surround @ [meter]
    if cordAct.[0] <> 14 then
        let meter = [cordAct.[0] + 4; cordAct.[1] ]
        surround <- surround @ [meter]
    if cordAct.[1] <> 2 then
        let meter = [cordAct.[0]; cordAct.[1] - 3 ]
        surround <- surround @ [meter]
    if cordAct.[1] <> 11 then
        let meter = [cordAct.[0]; cordAct.[1] + 3 ]
        surround <- surround @ [meter]

    if gold = cordAct then
        goldSensed <- "G"
    for x in 0 .. surround.Length - 1 do
        for y in 0 .. pitLoc.Length - 1 do
            if surround.[x] = pitLoc.[y] then
                breezeSensed <- "B"
        if surround.[x] = wumpus then
            smellSensed <- "S"
    Console.SetCursorPosition(cordAct.[0]-1, cordAct.[1]-1)

    visited (cordAct) (breezeSensed + goldSensed + smellSensed)
let generateProps() =
    let rand = new System.Random()
    let mutable stop = false
    let n = rand.Next(4)
    let pit = [[rand.Next(4); rand.Next(4); rand.Next(4); rand.Next(4)];
          [rand.Next(4); rand.Next(4); rand.Next(4); rand.Next(4)];
          [rand.Next(4); rand.Next(4); rand.Next(4); rand.Next(4)];
          [4; rand.Next(4); rand.Next(4); rand.Next(4)];]
    while not stop do
        let rand = new System.Random()
        wumpus <- [rand.Next(3); rand.Next(3)]
        gold <- [rand.Next(3); rand.Next(3)]
        if pit.[wumpus.[0]].[wumpus.[1]] = 0 || wumpus = [0; 0] || pit.[gold.[0]].[gold.[1]] = 0 || gold = [0; 0] || wumpus = gold then
            stop <- false
        else
            stop <- true
    wumpus <- coords.[wumpus.[0]].[wumpus.[1]]
    gold <- coords.[gold.[0]].[gold.[1]]
    for x in 0 .. 3 do
        for y in 0 .. 3 do
            if pit.[x].[y] = 0 then
                pitLoc <- pitLoc @ [coords.[x].[y]];
    printfn "%A" pit
    printfn "%A" pitLoc
    printfn "%A" wumpus
    printfn "%A" gold
let moveBetween() =
    let mutable cordAct = [2; 11]
    let mutable line = ""
    let mutable message = ""
    generateProps ()
    while not(isGameOver) do
        System.Console.Clear();
        
        for x in 0 .. map.Length-1 do
            printfn "%s" map.[x]
        Console.SetCursorPosition(cordAct.[0], cordAct.[1])
        Console.Write(actuator.[lookingAt])
        Console.SetCursorPosition(0, 14)
        message |> printf "%s"
        message <- ""
        
        checkSensors (cordAct)
        Console.SetCursorPosition(0, 17)
        printf "Pits at: %A" pitLoc
        Console.SetCursorPosition(0, 18)
        printf "Wumpus at: %A" wumpus
        Console.SetCursorPosition(0, 19)
        printf "Gold At: %A" gold
        Console.SetCursorPosition(0, 15)
        printf "Elige una opcion: "
        line <- Console.ReadLine()
        
        match line with
        | "Vder" ->
            if(lookingAt = 0) then
                message <- "Estas viendo a la Derecha"
            else
                lookingAt <- 0
        | "Vizq" ->
            if(lookingAt = 2) then
                message <- "Estas viendo a la Izquierda"
            else
                lookingAt <- 2
        | "Var" ->
            if(lookingAt = 3) then
                message <- "Estas viendo a Arriba"
            else
                lookingAt <- 3
        | "Vab" ->
            if(lookingAt = 1) then
                message <- "Estas viendo a Abajo"
            else
                lookingAt <- 1
        | "Shoot" ->
            if lookingAt = 0 then
                if (cordAct.[1] - wumpus.[1]) = 0 && (cordAct.[0] - wumpus.[0]) = -4 then
                    message <- "Mataste al wumpus"
                    wumpusKilled (cordAct)
                else
                    message <- "No mataste al wumpus:" 
            elif lookingAt = 1 then
                if (cordAct.[0] - wumpus.[0]) = 0 && (cordAct.[1] - wumpus.[1]) = -3 then
                    message <- "Mataste al wumpus"
                    wumpusKilled (cordAct)
                else
                    message <- "No mataste al wumpus" 
            elif lookingAt = 2 then
                if (cordAct.[1] - wumpus.[1]) = 0 && (cordAct.[0] - wumpus.[0]) = 4 then
                    message <- "Mataste al wumpus"
                    wumpusKilled (cordAct)
                else
                    message <- "No mataste al wumpus" 
            else
                if (cordAct.[0] - wumpus.[0]) = 0 && (cordAct.[1] - wumpus.[1]) = 3 then
                    message <- "Mataste al wumpus"
                    wumpusKilled (cordAct)
                else
                    message <- "No mataste al wumpus" 
        | "Grab" ->
            if cordAct = gold then
                grabGold (cordAct)
            else
                message <- "Aquí no hay oro"
        | "Der" ->
            if lookingAt <> 0 then
                message <- "Tienes que mirar a la derecha para moverte"
            elif cordAct.[0] = 14 then
                message <- "No puedes moverte a la Derecha"
            else
                cordAct <- [cordAct.[0] + 4; cordAct.[1]]
        | "Izq" ->
            if lookingAt <> 2 then
                message <- "Tienes que mirar a la izquierda para moverte"
            elif cordAct.[0] = 2 then
                message <- "No puedes moverte a la Izquierda"
            else
                cordAct <- [cordAct.[0] - 4; cordAct.[1]]
        | "Ar" ->
            if lookingAt <> 3 then
                message <- "Tienes que mirar a arriba para moverte"
            elif cordAct.[1] = 2 then
                message <- "No puedes moverte a arriba"
            else
                cordAct <- [cordAct.[0]; cordAct.[1] - 3]
        | "Ab" ->
            if lookingAt <> 1 then
                message <- "Tienes que mirar a abajo para moverte"
            elif cordAct.[1] = 11 then
                message <- "No puedes moverte a abajo"
            else
                cordAct <- [cordAct.[0]; cordAct.[1] + 3]
        | _ -> message <- "< < < N O   V A L I D O > > >"

        for x in 0 .. pitLoc.Length - 1 do
            if cordAct = pitLoc.[x] then
                isGameOver <- true
                message <- "Perdiste el juego: Entraste a un cuarto con un pit"
        if cordAct = wumpus then
            isGameOver <- true
            message <- "Perdiste el juego: El wumpus te mató"
        elif cordAct = [2; 11] && wasGoldGrabbed = true then
            isGameOver <- true
            message <- "Felicidades: Ganaste el juego"
    System.Console.Clear();
    printfn "%s" message

[<EntryPoint>]
let main argv =
    // for x in 0 .. map.Length-1 do
    //     printfn "%s" map.[x]
    // (2, 3) (6, 3) (10, 3) (14, 3)
    // (2, 6) (6, 6) (10, 6) (14, 6) 
    // (2, 9) (6, 9) (10, 9) (14, 9) 
    // // (2, 12) (6, 12) (10, 12) (14, 12) 
    // Console.SetCursorPosition(14, 12)
    // Console.Write("V")
    // Console.SetCursorPosition(0, 15)
    // System.Console.Clear();
    moveBetween ()
    // generateProps ()
    0 // return an integer exit code