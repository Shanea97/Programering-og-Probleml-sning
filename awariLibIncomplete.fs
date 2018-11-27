module Awari
type pit = int
type board = int array
type player = Player1 | Player2


/// <param name "b">
/// The Parameter is called b, and functions as a board
/// </param>
/// <summary>
/// The function takes and Integer Array, and puts it up as follows of the function. Thereby making the list look like a Player board.
/// </summary>


let printBoard (b: int array) =
  printf "%6i(5)%6i(4)%6i(3)%6i(2)%6i(1)%6i(0)" b.[5] b.[4] b.[3] b.[2] b.[1] b.[0]
  printfn " <- Player1's side w/ beans(pit no.)"
  printfn "\n%i <- Player1's home %40i <- Player2's home" b.[6] b.[13]
  printf "\n%6i(7)%6i(8)%6i(9)%6i(10)%5i(11)%5i(12)" b.[7] b.[8] b.[9] b.[10] b.[11] b.[12]
  printf " <- Player2's side w/ beans(pit no.)"

///<returns>
/// The function returns an Integer Array thats formed like a player board
///     3(5)     3(4)     3(3)     3(2)     3(1)     3(0) <- Player1's side w/ beans(pit no.)
///
///0 <- Player1's home                                        0 <- Player2's home
///
///     3(7)     3(8)     3(9)     3(10)    3(11)    3(12) <- Player2's side w/ beans(pit no.)
///</returns>


/// <Param name "p" and "i">
/// "p" is seen as one of the types defined in this game (a Player). "i" is the pit/integer in which each players have their home feeld.
/// </param name>
/// <summary>
/// The function checks if there the pit, in which the player chooses or lands ind, is the players home field.
/// </summary>
/// <returns>
/// The function returns a bool
let isHome (p: player) (i: int) = //board bliver ikke brugt i vores implementation
  match p with
  | Player1 -> i = 6
  | Player2 -> i = 13

/// <Param name "b">
/// The function takes a board as input
/// </param name>
/// <summary>
/// The function checks if either side contains any beans. If there consists a side where no beans are to be found, the game will terminate
/// </summary>
/// <returns>
/// board
/// </returns>

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

/// <param name "b". "p" and "q">
/// b is the board of which the player (p) can choose from. This it does via a string.
/// </param name>
/// <summary>
/// The function uses the current board, and which player turn it is to see if one is able to make the move. This it does by a string, constructed by System.Console.ReadLine and thereby making it into an integer.
/// </summary>
/// <returns>
/// the function returns the pit, of which the player have choosen
/// </returns>

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

/// <param name: "b", "p" and "i">
/// b: board
/// p: player
/// i: int
/// </param name>
/// <summary>
/// This functions distributes the beans around the board by changing the int array. Furthermore it makes it possible to "steal" by using the match and the help function oppisite.
/// </summary>
/// <returns>
/// returns a trippel: player, current player, the last pit.
/// </returns>

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

/// <param name: "b", "p" and "n">
/// b: board
/// p: player
/// n: int
/// </param name>
/// <summary>
/// This function makes use of most of the other functions, and checks if a player is able to have another turn. But bsc. it makes sure that eachs players turns runs smoothely
/// </summary>
/// <returns>
/// returns a board
/// </returns>


let turn (b : board) (p : player) : board =
  let rec repeat (b: board) (p: player) (n: int) : board =
    printBoard b
    let str =
      if n = 0 then // virker denne funktion?
        sprintf "Player %A's move? " p
      else
        "Again? "
    let i = getMove b p str
    let (newB, finalPitsPlayer, finalPit)= distribute b p i //virker vores funktion ikke her?
    if not (isHome finalPitsPlayer finalPit)
       || (isGameOver b) then
      newB
    else
      repeat newB p (n + 1)
  repeat b p 0

/// <param name "b" and "p">
/// b: board
/// p: player
/// </param name>
/// <summary>
/// This is the function thats actually runs the game, via check if "isGameOver" is true, or else calling the function "turn"
/// </summary>
/// <returns>
/// Returns a new board, whith the moves implemented and the new players turn (or same, if its the same player)
/// </returns>

let rec play (b : board) (p : player) : board =
  if isGameOver b then
    b
  else
    let newB = turn b p
    let nextP =
      if p = Player1 then
        Player2
      else
        Player1
play newB nextP
