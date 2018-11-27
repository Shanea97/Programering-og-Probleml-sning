type pit = int
/// A board consists of pits.
type board = int array
/// A game is played between two players
type player = Player1 | Player2


let b = [|3;3;3;3;3;3;0;3;3;3;3;3;3;0|]

let rec printBoard (b:int array) =
  printf "%4i%4i%4i%4i%4i%4i" b.[0] b.[1] b.[2] b.[3] b.[4] b.[5]
  printfn "     <- Player1's side"
  printfn "%i%31i" b.[6] b.[13]
  printf "%4i%4i%4i%4i%4i%4i" b.[7] b.[8] b.[9] b.[10] b.[11] b.[12]
  printf "     <- Player2's side"



let isHome (b:int array) (p: player) (i: int) =
  if b.[i] = b.[6] then
    if p = Player1 then
      true
    else
      false
  else
    false
  if b.[i] = b.[13] then
    if p = Player2 then
      true
    else
      false
  else
    false 
printf "%A" (isHome b Player2 6)
