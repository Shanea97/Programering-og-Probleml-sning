open Geometry

let CheckFigure (f:figure) : bool =
  match figure with
  | Circle ((cx,cy),r,col) ->
      if r > 0 then
        true
      else
        false
  | Rectangle ((x0,y0), (x1,y1), col) ->
      if x0 < x1 && y0 < y1 then
        true
      else
        false
  | Mix (f1,f2) ->
      match (colourAt (x,y) f1, colourAt (x,y) f2) with
      | true -> true
      | _    -> false 

//let CheckColour (c:colour) : bool =
