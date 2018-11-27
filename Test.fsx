open Awari
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
