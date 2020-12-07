// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
//  Variables para el funcionamiento del juego
let pMatrix = [| [| [|0;0;0|] ; [|0;0;0|];  [|0;0;0|]; [|-1;-1;0|]; |]; [| [|0;0;0|] ; [|0;0;0|];  [|0;0;0|]; [|0;0;0|]; |]; [| [|0;0;0|] ; [|0;0;0|];  [|0;0;0|]; [|0;0;0|]; |]; [| [|0;0;0|] ; [|0;0;0|];  [|0;0;0|]; [|0;0;0|]; |];|]
//  Posibles vistas del agente con la direccion de vista
let actuator = [">";"v";"<";"^"]
let mutable lookingAt = 0
//  Mapa del juego
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
//  Cordenadas absolutas de impresion
let coords = [[[2; 2]; [6; 2]; [10; 2]; [14; 2]];
            [[2; 5]; [6; 5]; [10; 5]; [14; 5]];
            [[2; 8]; [6; 8]; [10; 8]; [14; 8]];
            [[2; 11]; [6; 11]; [10; 11]; [14; 11]];]
//  Variables de control para el juego
let mutable wumpus = [0; 0]
let mutable gold = [0; 0]
let mutable isGameOver = false
let mutable wasGoldGrabbed = false
let mutable wasWumpusKilled = false
//  Variables de alocacion del POZO, WUMPUS y ORO
let pit = []
let mutable pitLoc = []
let mutable cordHist = []
let mutable repCordHist = []
//   Varianle de colision para el verificador
let mutable colisionXFinal = false
//  Lista de simbolos a imprimir en el juego SGB
let mutable simb:list<string> = []
//   Posiciones ABSOLUTAS del JUGADOR
let mutable posx = 0
let mutable posy = 0
//  Variables de posicion de
let mutable deleteFirst = true
let mutable index = 1
//  Variable para el movimiento generado
let mutable movimiento = ""

// FUNCION PARA LA MATRIZ DE PROBABILIDAD
let probabilityMatrix (cordAct:list<int>) (sensor:string) =
    let mutable isDone = false
    //  Checa si las posiciones ya fueron recorridas
    for check in cordHist do
        if check = cordAct then
            isDone <- true
    for x in 0 .. coords.Length-1 do
        for y in 0 .. coords.[x].Length-1 do
            if coords.[x].[y] = cordAct then
                posx <- y
                posy <- x

    let mutable isRepeated = false
    // Obtiene la posicion Absoluta dentro de un arreglo 4x4
    if repCordHist.Length = 0 then
        repCordHist <- [[posx;posy];]  @  repCordHist
    else
        for x in 0 .. repCordHist.Length - 1 do
            if repCordHist.[x] = [posx;posy] then
                isRepeated <- true
        if not isRepeated then
            repCordHist <-  [[posx;posy];] @ repCordHist  
    // En caso de una nueva posicion...
    if not isDone then
        
        //  Pone la posicion actual como "NO PELIGO"
        pMatrix.[posx].[posy].[0] <- -1
        pMatrix.[posx].[posy].[1] <- -1
        //  Crea los arreglos para checar al rededor
        let allPosX = [|posx;posx+1;posx;posx-1;|]
        let allPosY = [|posy+1;posy;posy-1;posy;|]
        for i in 0 .. 3 do
            // Si recibe una S, pone las casillas de al rededor como "POSIBLE WUMPUS"
            try
               if sensor.Contains "S" && pMatrix.[allPosX.[i]].[allPosY.[i]].[0] <> -1 then
                    if pMatrix.[allPosX.[i]].[allPosY.[i]].[0] >= 1 then
                        pMatrix.[allPosX.[i]].[allPosY.[i]].[0] <- pMatrix.[allPosX.[i]].[allPosY.[i]].[0] + 1
                    else
                        //  Aumenta la probabilidad de que haya un wumpus en cierta casilla
                        pMatrix.[allPosX.[i]].[allPosY.[i]].[0] <- 1
                else
                    //  Si no, las pone cómo "No peligro"
                    pMatrix.[allPosX.[i]].[allPosY.[i]].[0] <- -1 
            with
                | :? System.IndexOutOfRangeException -> ()
            //  Recibe una B, pone la probabilidad en uno de "Posible pit"
            try
                if sensor.Contains "B" && pMatrix.[allPosX.[i]].[allPosY.[i]].[1] <> -1 then
                    if pMatrix.[allPosX.[i]].[allPosY.[i]].[1] >= 1 then
                        pMatrix.[allPosX.[i]].[allPosY.[i]].[1] <- pMatrix.[allPosX.[i]].[allPosY.[i]].[1] + 1
                    else
                        // Aumenta la probabilidad
                        pMatrix.[allPosX.[i]].[allPosY.[i]].[1] <- 1
                else
                    //  Si no, las pone cómo "No peligro"
                    pMatrix.[allPosX.[i]].[allPosY.[i]].[1] <- -1 
            with
                | :? System.IndexOutOfRangeException -> ()
        //  Impresion de control   
        Console.SetCursorPosition(0, 23)
        printf "X: %A   - Y: %A" posx posy   
        if sensor.Contains "G" then
            pMatrix.[posx].[posy].[2] <- 1   
// Funcion para moverse de manera automatica
let autoMove():unit =
    movimiento <- ""
    //  Si el oro no ha sido agarrado...
    if not wasGoldGrabbed then
        
        let mutable dirDerecha = true
        // Funcion de control para saber a donde se va a mover
        if posy = 3 || posy = 1 then
            dirDerecha <- true
        else 
            dirDerecha <- false
        if posx = 0 then
            colisionXFinal <- false
        //  Si no ha colisionado en X
        if not colisionXFinal then
            try
                // Se mueve a la derecha cuando no hay peligro, cuando a la izquierda hay mucha probabilidad de un pozo o cuando a abajo hay mucha probabilidad de un pozo
                if pMatrix.[posx+1].[posy] = [|-1;-1;0|] || pMatrix.[posx-1].[posy].[1] > 1 || pMatrix.[posx].[posy+1].[1] > 1 then
                        if lookingAt <> 0 then
                            movimiento <- "Vder"
                        else
                            movimiento <- "Der"
                //  Se mueve a Arriba cuando a la derecha hay mucha probabilidad de un pozo
                else if pMatrix.[posx+1].[posy].[1] > 1  then
                    if lookingAt <> 3 then
                        movimiento <- "Var"
                    else
                        movimiento <- "Ar"
                //  Se mueve a la IZQ cuando a la izquierda no hay peligro  
                else if pMatrix.[posx-1].[posy] = [|-1;-1;0|] then
                    colisionXFinal <- true
                    if lookingAt <> 2 then
                        movimiento <- "Vizq"
                    else
                        movimiento <- "Izq"
                
            with
                | :? System.IndexOutOfRangeException -> 
                    //  Como predeterminado al colisionar, te mueves a la IZQ
                    colisionXFinal <- true
                    if lookingAt <> 2 then
                        movimiento <- "Vizq"
                    else
                        movimiento <- "Izq"
        else
            //  Al activarse la colision...
            try
                //  Te mueves AR si la posicion de arriba es segura o si la probabilidad de un pozo a abajo es mucha
                if pMatrix.[posx].[posy-1] = [|-1;-1;0|]  || pMatrix.[posx].[posy-1].[1] > 1  then
                   if lookingAt <> 3 then
                        movimiento <- "Var"
                    else
                        movimiento <- "Ar"
                //  Si las posy es diferente a 3
                else if posy <> 3 then 
                    //  Nos movemos a AR sy la posibilidad de un pozo abajo es mucha
                    if pMatrix.[posx].[posy+1].[1] > 1 then
                        if lookingAt <> 3 then
                            movimiento <- "Var" 
                        else
                            movimiento <- "Ar"
                    //  De lo contrario, no seguimos moviendo a la IZQ
                    else
                        if lookingAt <> 2 then
                            movimiento <- "Vizq"
                        else
                            movimiento <- "Izq"        
                else
                    //  Como predeterminado nos seguimos moviemdo a la izquierda
                    if lookingAt <> 2 then
                        movimiento <- "Vizq"
                    else
                        movimiento <- "Izq" 
            with
                | :? System.IndexOutOfRangeException ->
                // Desactivamos la colision
                colisionXFinal <- false
                //  Checamos si llegamos al oro
                if pMatrix.[posx].[posy].[2] = 1 then
                    movimiento <- "Grab"
                //  De otra forma, nos segumos moviendo a la IZQ
                else
                    if lookingAt <> 2 then
                        movimiento <- "Vizq"
                    else
                        movimiento <- "Izq"
            try
                if not colisionXFinal && pMatrix.[posx+1].[posy].[0] > 1  then
                    //  Como predeterminado nos seguimos moviemdo a la derecha
                   if lookingAt <> 0 then
                        movimiento <- "Vder"
                    else
                        movimiento <- "Shoot"
                // Activamos la colision
                colisionXFinal <- true 
            with
                | :? System.IndexOutOfRangeException -> ()

    else 
        let mutable cicle = true
        let mutable deleteStart = false
        // Al agarrar el oro, tenemos que regresar
        while cicle do  
            //  Checamosel historial de movimientos
            //  Y dependiendo de eso, es que nos vamos a mover por donde llegamos al oro
            if posx = repCordHist.[index].[0] then
                if (posy - repCordHist.[index].[1]) = -1 then
                    if lookingAt <> 1 then
                        movimiento <- "Vab"
                    else
                        movimiento <- "Ab"
                        index <- index + 1
                else
                    if lookingAt <> 3 then
                        movimiento <- "Var"
                    else
                        movimiento <- "Ar"
                        index <- index + 1
                cicle <- false
            else if posy = repCordHist.[index].[1] then
                if (posx - repCordHist.[index].[0]) = -1 then
                    if lookingAt <> 0 then
                        movimiento <- "Vder"
                    else
                        movimiento <- "Der"
                        index <- index + 1
                else
                    if lookingAt <> 2 then
                        movimiento <- "Vizq"
                    else
                        movimiento <- "Izq"
                        index <- index + 1
                cicle <- false
            else 
                // En caso de que no coincida. Aumentaremos el indice, para saltarnos la casilla.
                // Se vuelve a ejecutar el proceso de checado
                index <- index + 1 
                cicle <- true
    //  Se imprime el historial y los movimientos que debes hacer
    Console.SetCursorPosition(0, 28)
    printf "Mov : %A" repCordHist
    Console.SetCursorPosition(0, 22)
    printf "Mov : %s" movimiento
    
    // movimiento <- ""    
// Funcion para agarrar el oro
let grabGold(cordAct:list<int>) =
    let mutable newSimb:list<string> = []
    // cambia la alerta
    for x in 0 .. simb.Length - 1 do
        if simb.[x].IndexOf("G") = 1 then
            newSimb <- newSimb @ [simb.[x].Replace("G"," ")]
        else
            newSimb <- newSimb @ [simb.[x]]
    //  Modifica la alerta
    simb <- newSimb
    //  Cambia la probabilidad
    pMatrix.[posx].[posy].[2] <- 1
    //  Cambia la variable del oror
    wasGoldGrabbed <- true
let wumpusKilled(cordAct:list<int>) =
    let mutable newSimb:list<string> = []
    // Checa la alerta
    for x in 0 .. simb.Length - 1 do
        if simb.[x].IndexOf("S") = 2 then
            newSimb <- newSimb @ [simb.[x].Replace("S"," ")]
        else
            newSimb <- newSimb @ [simb.[x]]
    // Modifica la alerta
    simb <- newSimb
    wasWumpusKilled <- true
    //  Cambia la probabilidad
    pMatrix.[posx+1].[posy].[0] <- -1
//  Funcion que se encarga de imprimir los simbolos
let uncover(cordHist:list<list<int>>) (simb:list<string>) =
    for x in 0 .. cordHist.Length - 1 do
        Console.SetCursorPosition(cordHist.[x].[0] - 1, cordHist.[x].[1] - 1)
        printf "%s" simb.[x]
//  Funcion para saber el Historial relativo visitado
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
    //  Llama a la funcion parab imprimir los simbolos
    uncover (cordHist) (simb)
//  Funcion para sensar el alrededor
let checkSensors(cordAct:list<int>) =
    let mutable  surround:list<list<int>> = []
    let mutable smellSensed = " "
    let mutable breezeSensed = " "
    let mutable goldSensed = " "
    //  Checa la posiciones al rededor y las mete a una lista
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
    //  Activa el Destello si se encuentra en la posicion del oro
    if gold = cordAct then
        goldSensed <- "G"
    // Verifica cada una de las posiciones y las compara con las de los PITS y la del Wumpus
    for x in 0 .. surround.Length - 1 do
        for y in 0 .. pitLoc.Length - 1 do
            if surround.[x] = pitLoc.[y] then
                breezeSensed <- "B"
        if surround.[x] = wumpus && not wasWumpusKilled then
            smellSensed <- "S"
    //  Llama a la funcion de probabilidad
    probabilityMatrix (cordAct) (breezeSensed + goldSensed + smellSensed)

    Console.SetCursorPosition(cordAct.[0]-1, cordAct.[1]-1)
    //  Llama a la funcion de visitado
    visited (cordAct) (breezeSensed + goldSensed + smellSensed)
let generateProps() =
    //  Funcion que inicialmente generaba al WUMPUS, ORO y PITS de manera aleatoria
    // let rand = new System.Random()
    // let mutable stop = false
    // let n = rand.Next(4)
    // let pit = [[rand.Next(4); rand.Next(4); rand.Next(4); rand.Next(4)];
    //       [rand.Next(4); rand.Next(4); rand.Next(4); rand.Next(4)];
    //       [rand.Next(4); rand.Next(4); rand.Next(4); rand.Next(4)];
    //       [4; rand.Next(4); rand.Next(4); rand.Next(4)];]
    // while not stop do
    //     let rand = new System.Random()
    //     wumpus <- [rand.Next(3); rand.Next(3)]
    //     gold <- [rand.Next(3); rand.Next(3)]
    //     if pit.[wumpus.[0]].[wumpus.[1]] = 0 || wumpus = [0; 0] || pit.[gold.[0]].[gold.[1]] = 0 || gold = [0; 0] || wumpus = gold then
    //         stop <- false
    //     else
    //         stop <- true
    // wumpus <- coords.[wumpus.[0]].[wumpus.[1]]
    // gold <- coords.[gold.[0]].[gold.[1]]
    // for x in 0 .. 3 do
    //     for y in 0 .. 3 do
    //         if pit.[x].[y] = 0 then
    //             pitLoc <- pitLoc @ [coords.[x].[y]];
    // printfn "%A" pit
    // printfn "%A" pitLoc
    // printfn "%A" wumpus
    // printfn "%A" gold

    // Pero se cambió, para que fuera solo un mapa posible
    pitLoc <- [[14; 11]; [10; 8]; [2; 5]; [2; 2]]
    wumpus <- [14; 2];
    gold <- [6;2];
let moveBetween() =
    //  Coordenada Inicial 
    let mutable cordAct = [2; 11]
    let mutable line = ""
    let mutable message = ""
    //  Genera el mapa
    generateProps ()
    //  Ciclo que se ejecuta mientras el juego esta activo
    while not(isGameOver) do
        System.Console.Clear();
        // Impresion del mapa
        for x in 0 .. map.Length-1 do
            printfn "%s" map.[x]
        Console.SetCursorPosition(cordAct.[0], cordAct.[1])
        Console.Write(actuator.[lookingAt])
        Console.SetCursorPosition(0, 14)
        //  Impresion del message
        message |> printf "%s"
        message <- ""
        //  Se imprime la posicion del PIT, WUMPUS y ORO
        //  Variables que el la maquina de movimiento no sabe
        checkSensors (cordAct)
        Console.SetCursorPosition(0, 17)
        printf "Pits at: %A" pitLoc
        Console.SetCursorPosition(0, 18)
        printf "Wumpus at: %A" wumpus
        
        Console.SetCursorPosition(0, 19)
        printf "Gold At: %A" gold
        //  Funcion para mover de manera automatica
        autoMove()
        // Imprime la matriz de probabilidad
        Console.SetCursorPosition(0, 24)
        for x in 0..3 do
            for y in 0..3 do
                printf "Matrixt: %A" pMatrix.[y].[x]
            printfn ""
        
        Console.SetCursorPosition(0, 15)
        printf "Enter para siguiente mov: "
        line <- Console.ReadLine()
        line <- movimiento

        //  Evalua lo generado y realiza el movimiento
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
            //  Verifica las posiciones para matar
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
            //  Verifica la posicion con la del oro
            if cordAct = gold then
                grabGold (cordAct)
            else
                message <- "Aquí no hay oro"
        //  Para el movimiento, checa que estes viendo en la direccion a moverse
        //  Y si sí, suma o resta las posiciones correspondientes
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
        //  Finalmente, realiza la verificacion de si moriste o sigues vivo o ganaste
        for x in 0 .. pitLoc.Length - 1 do
            if cordAct = pitLoc.[x] then
                isGameOver <- true
                message <- "Perdiste el juego: Entraste a un cuarto con un pit"
        if cordAct = wumpus && wasWumpusKilled = false then
            isGameOver <- true
            message <- "Perdiste el juego: El wumpus te mató"
        elif cordAct = [2; 11] && wasGoldGrabbed = true then
            isGameOver <- true
            message <- "Felicidades: Ganaste el juego"
    System.Console.Clear();
    printfn "%s" message

[<EntryPoint>]
let main argv =
    //  Manda a llamar al juego
    moveBetween ()


    0 // return an integer exit code