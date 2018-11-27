
let b = [|3;3;3;3;3;3;0;3;3;3;3;3;3;0|]

let rec printBoard (b:int array) =
  printf "%4i%4i%4i%4i%4i%4i" b.[0] b.[1] b.[2] b.[3] b.[4] b.[5]
  printfn "     <- Player1's side"
  printfn "%i%31i" b.[6] b.[13]
  printf "%4i%4i%4i%4i%4i%4i" b.[7] b.[8] b.[9] b.[10] b.[11] b.[12]
  printf "     <- Player2's side"




//printfn "%A" (printBoard b)
let rec Mystik (b: int array) (h:int) =
  let mutable q = 1
  let m = (Array.get b h)
  printfn "Pit value: %A" m
  while (m >= q) do
      b.[(13%h)+q] <- b.[(13%h)+q] + 1
      q         <- q+1
      b.[13%h]     <- b.[13%h]-1
  (printBoard b)

let rec repeating (l:int array) =
  printf "What pit would you like to move:"
  let s = int(System.Console.ReadLine())
  let q = Array.get l s
  let a = if q > 0 then
            true
          else
            false
  let d = if a = false then
            printf "Not allowed\n"
          else
            printf "Allowed\n"
  let c = if a = true then
            (Mystik l s)
          else
            (repeating l)
  c




//printfn "%A" (Mystik [|3;3;3;3;3;3;0;3;3;3;3;3;3;0|])
printf "%A" (repeating [|3;3;3;3;3;3;0;3;3;3;3;3;3;0|])
