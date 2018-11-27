type pit = int
type board = int array
type player = Player1 | Player2

let printBoard (b: int array) =
  printf "%6i(5)%6i(4)%6i(3)%6i(2)%6i(1)%6i(0)" b.[5] b.[4] b.[3] b.[2] b.[1] b.[0]
  printfn " <- Player1's side w/ beans(pit no.)"
  printfn "\n%i <- Player1's home %40i <- Player2's home" b.[6] b.[13]
  printf "\n%6i(7)%6i(8)%6i(9)%6i(10)%5i(11)%5i(12)" b.[7] b.[8] b.[9] b.[10] b.[11] b.[12]
  printf " <- Player2's side w/ beans(pit no.)"

printfn "\nWhite-box test af printBoard:"
let x = [|1;2;3;4;5;6;7;8;9;10;11;12;13;14|]
printfn "%A" (printBoard x)


let isHome (p: player) (i: int) =
  match p with
  | Player1 -> i = 6
  | Player2 -> i = 13

printfn "\nWhite-box test af isHome for player1:"
printfn "Player1's home pit, 6"
printfn "%A" (isHome Player1 6)
printfn "En af Player1's normale pits, 4:"
printfn "%A" (isHome Player1 4)
printfn "En af Player2's normale pits, 10:"
printfn "%A" (isHome Player1 10)
printfn "Player2's home pit, 13:"
printfn "%A" (isHome Player1 13)

printfn "\nWhite-box test af isHome for player2:"
printfn "Player2's home pit, 13"
printfn "%A" (isHome Player2 13)
printfn "En af Player2's normale pits, 8:"
printfn "%A" (isHome Player2 8)
printfn "En af Player1's normale pits, 0:"
printfn "%A" (isHome Player2 0)
printfn "Player1's home pit, 6:"
printfn "%A" (isHome Player2 6)


let isGameOver (b:board) =
  let p1 = Array.toList b.[0..5]
  let p2 = Array.toList b.[7..12]
  let game1 = List.forall (fun x -> (x = 0)) p1
  let game2 = List.forall (fun x -> (x = 0)) p2
  if game1 = false then
    if game2 = false then
      false
    else
      true
  else
    true

printfn "\nWhite-box test af isGameover for player 1's side:"
printfn "[0..5] ingen bønner:"
let y = [|0;0;0;0;0;0;36;36;36;36;36;36;36;36|]
// forventet Output True:
printfn "%A" (isGameOver y)
printfn "[0..5] en enkel bønne:"
let y1 = [|0;0;0;1;0;0;36;36;36;36;36;36;36;36|]
printfn "%A" (isGameOver y1)

printfn "\nWhite-box test af isGameover for player 2's side:"
printfn "[7..12] ingen bønner:"
let y2 = [|36;36;36;36;36;36;36;0;0;0;0;0;0;36|]
printfn  "%A" (isGameOver y2)
printfn "[7..12] en enkelt bønne:"
let y3 = [|36;36;36;36;36;36;36;0;0;0;0;1;0;36|]
printfn  "%A" (isGameOver y3)


let rec getMove (b:board) (p:player) (q:string)=
  match p with
    | Player1 -> printfn "\n\n%A what pit would you like to move? Please type a number from 0 to 5." p
    | Player2 -> printfn "\n\n%A what pit would you like to move? Please type a number from 7 to 12." p
  match p with
    | Player1 -> match q with
                  | "0" -> int(q)
                  | "1" -> int(q)
                  | "2" -> int(q)
                  | "3" -> int(q)
                  | "4" -> int(q)
                  | "5" -> int(q)
                  | _ -> (getMove b p (System.Console.ReadLine())) //Hvis ikke no. 0-5 er givet kalder funktionen sig selv
    | Player2 -> match q with
                  | "7" -> int(q)
                  | "8" -> int(q)
                  | "9" -> int(q)
                  | "10" -> int(q)
                  | "11" -> int(q)
                  | "12" -> int(q)
                  | _ -> (getMove b p (System.Console.ReadLine())) //Hvis ikke no. 7-12 er givet kalder funktionen sig selv
printfn "Test for Player 1:\n"
printf "Nr.1\n"
let a = (getMove [|3;3;3;3;3;3;0;3;3;3;3;3;3;0|] Player1 "5")
printfn "With following inputs: \nList: %A\nPlayer: %s\nString: %s" [|3;3;3;3;3;3;0;3;3;3;3;3;3;0|] "Player 1" "5"
printfn "Exp O: %i\nResult: %A\nDid the result match our expectations? %b\n" 5 a (a = 5)

printf "Nr.2 - If Player 1 takes from the opponent\n"
let b = (getMove [|3;3;3;3;3;3;0;3;3;3;3;3;3;0|] Player1 "8")
printfn "With following inputs: \nList: %A\nPlayer: %s\nString: %s" [|3;3;3;3;3;3;0;3;3;3;3;3;3;0|] "Player 1" "8"
printfn "Exp O: %s\nResult: %A\nDid the result match our expectations: %s " "Here hopefully you should yourself, dial in a new number, cause you shouldn't be able to pick your opponents pits" b "true"

printfn "Test for Player 2:\n"
printf "Nr.3\n"
let c = (getMove [|3;3;3;3;3;3;0;3;3;3;3;3;3;0|] Player2 "9")
printfn "With following inputs: \nList: %A\nPlayer: %s\nString: %s" [|3;3;3;3;3;3;0;3;3;3;3;3;3;0|] "Player 2" "9"
printfn "Exp O: %i\nResult: %A\nDid the result match our expectations? %b\n" 9 c (c = 9)

printf "Nr.4 - If Player 2 takes from the opponent\n"
let d = (getMove [|3;3;3;3;3;3;0;3;3;3;3;3;3;0|] Player2 "2")
printfn "With following inputs: \nList: %A\nPlayer: %s\nString: %s" [|3;3;3;3;3;3;0;3;3;3;3;3;3;0|] "Player 2" "2"
printfn "Exp O: %s\nResult: %A\nDid the result match our expectations: %s " "Here hopefully you should yourself, dial in a new number, cause you shouldn't be able to pick your opponents pits" d "true"


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

printfn "\n\n Test of distribute"
printfn "Are we able to steal from the other player if we land in an empty pit?"
let xx = [|3;3;3;0;4;4;1;3;3;3;3;3;3;0|]
let o = Player1
let g = 0
let f = (distribute xx o g)
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
