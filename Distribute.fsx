type pit = int
type board = int array
type player = Player1 | Player2

let b = [|3;3;3;3;3;3;0;3;3;3;3;3;3;0|]

let rec printBoard (b:int array) =
  printf "%4i%4i%4i%4i%4i%4i" b.[0] b.[1] b.[2] b.[3] b.[4] b.[5]
  printfn "     <- Player1's side"
  printfn "%i%31i" b.[6] b.[13]
  printf "%4i%4i%4i%4i%4i%4i" b.[7] b.[8] b.[9] b.[10] b.[11] b.[12]
  printf "     <- Player2's side"


let rec getMove (l:board) (i:pit) (p:player) =
  printf "What pit would you like to move:"
  let q = Array.get l i
  let s = System.Console.ReadLine()
  if p = Player1 then
    if s >= "0" && "5" >= s then
      let a = if q > 0 then
                true
              else
                false
      let d = if a = false then
                printf "Not allowed\n"
              else
                printf " Allowed\n"
      let c = if a = true then
                (printBoard b)
              else
                (getMove l i p)
      c
    else
      (getMove l i p)
  else
    if s >= "7" && "12" >= s then
      let e = if q > 0 then
                true
              else
                false
      let f = if e = false then
                printf "Not allowed\n"
              else
                printf " Allowed\n"
      let g = if e = true then
                (printBoard b)
              else
                (getMove l i p)
      g
    else
      0
printf "%A" (getMove b 8 Player2)
