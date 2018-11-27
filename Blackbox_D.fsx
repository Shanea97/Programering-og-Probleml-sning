type pit = int
type board = int array
type player = Player1 | Player2

let distribute (b: int array) (p:player) (i: int) = //når funktionen bliver kaldt fordeler den kun bønner, men stjæler ikke. jeg tror det har noget at gøre turn-funktionen, da den kan stjæle, når jeg tester (se Test 5).

  let n = b.[i] // n: antal bønner i start-index
  let mutable k = i // lagring af index i k
  b.[i] <- 0
  for j = 1 to n do //fordeling af bønnerne ud fra antal i n
    k <- k + 1
    if k > (b.Length-1) then do
      k <- 0
    if p = Player1 then
      if k = 13 then // ingen bønner i Player2's hjem
        k <- 0
      else
        b.[k] <- b.[k] + 1 //bønner fordeles med k-index
    else
      if k = 6 then // ingen bønner i Player1's hjem
        k <- 7
      else
        b.[k] <- b.[k] + 1

//måske kunne der også være en uoverstemmelse mellem for-loop og match

  let oppisite t = //hjælpefunktion, der vender tal om
    match t with
      | 7 ->
        let t = 5
        t
      | 8 ->
        let t = 4
        t
      | 9 ->
        let t = 3
        t
      | 10 ->
        let t = 2
        t
      | 11 ->
        let t = 1
        t
      | 12 ->
        let t = 0
        t
      | 0 ->
        let t = 12
        t
      | 1 ->
        let t = 11
        t
      | 2 ->
        let t = 10
        t
      | 3 ->
        let t = 9
        t
      | 4 ->
        let t = 8
        t
      | 5 ->
        let t = 7
        t
      | _ -> t

  let t = ((i+n)%(b.Length)) // udregning af sidste felt med modolus
  match t with
    | 13 -> (b,p,t) // resultat printes som en trippel, hvis slutfelt er hjemfelt
    | 6 -> (b,p,t)
    | _ -> match b.[t] with
            | 1 when 6 > t ->
                  b.[6] <- b.[6] + b.[oppisite t] + b.[t] //hjemfelt får det modsatte felts bønner + det landede felts bønner
                  b.[oppisite t] <- 0 //der hvor bønnerne er stjålet fra sættes til 0
                  b.[t] <- 0 //det felt som er landet i sættes til 0
                  (b,p,t)
            | 1 when 6 < t ->
                  b.[13] <- b.[13] + b.[oppisite t] + b.[t]
                  b.[oppisite t] <- 0
                  b.[t] <- 0
                  (b,p,t)
            | _ -> (b,p,t)

printfn "Are we able to steal from the other player if we land in an empty pit?"
let x = [|3;3;3;0;4;4;1;3;3;3;3;3;3;0|]
let o = Player1
let g = 0
let f = (distribute x o g)
let x1 = [|3;3;3;0;4;4;1;3;3;3;3;3;3;0|]
printfn "The List:%A\nThe player: %s\nThe input pit: %i\nExp. O: %A" x1 "player1" g ([|0; 4; 4; 0; 4; 4; 5; 3; 3; 0; 3; 3; 3; 0|], Player1, 3)
printfn "Result: %A" f
printfn "Does the input match our expectations?: %b\n" (f = ([|0; 4; 4; 0; 4; 4; 5; 3; 3; 0; 3; 3; 3; 0|], Player1, 3))

printfn "Check to make sure that the players aren't able to give their opponent points"
let x2 = [|1;1;1;1;1;13;0;3;3;3;3;3;3;0|]
let f1 = (distribute x2 o 5)
let x3 = [|1;1;1;1;1;13;0;3;3;3;3;3;3;0|]
printfn "The List:%A\nThe player: %s\nThe input pit: %i\nExp. O: %A" x3 "player1" 5 ([|1; 2; 2; 2; 2; 1; 1; 4; 4; 4; 4; 4; 4; 0|], Player1, 4)
printfn "Result: %A" f1
printfn "Does the input match our expectations? %b\n" (f1 = ([|1; 2; 2; 2; 2; 1; 1; 4; 4; 4; 4; 4; 4; 0|], Player1, 4))

printfn "Let's also check that player 2 is able to move around"
let x4 = [|1;1;1;1;1;3;0;3;9;3;4;3;7;0|]
let f2 = (distribute x4 Player2 8)
let x5 = [|1;1;1;1;1;3;0;3;9;3;4;3;7;0|]
printfn "The List:%A\nThe player: %s\nThe input pit: %i\nExp. O: %A" x5 "player2" 8 ([|2; 2; 2; 2; 1; 3; 0; 3; 0; 4; 5; 4; 8; 1|], Player2, 3)
printfn "Result: %A" f2
printfn "Does the input match our expectations? %b\n" (f2 = ([|2; 2; 2; 2; 1; 3; 0; 3; 0; 4; 5; 4; 8; 1|], Player2, 3))

printfn "Let's check if Player 1 lands in an empty pit, on Player 2's side."
let x6 = [|3;3;3;4;4;4;1;3;3;0;4;4;4;0|]
let f3 = (distribute x6 Player1 5)
let x7 = [|3;3;3;4;4;4;1;3;3;0;4;4;4;0|]
printfn "The List:%A\nThe player: %s\nThe input pit: %i\nExp. O: %A" x7 "player1" 5 ([|3; 3; 3; 0; 4; 0; 2; 4; 4; 0; 4; 4; 4; 5|], Player1, 9)
printfn "Result: %A" f3
printfn "Does the input match our expectations? %b" (f3 = ([|3; 3; 3; 0; 4; 0; 2; 4; 4; 0; 4; 4; 4; 5|], Player1, 9))
