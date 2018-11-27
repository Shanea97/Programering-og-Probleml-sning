module ImgUtil

// colors
type color = System.Drawing.Color
val red      : color
val blue     : color
val green    : color
val fromRgb  : int * int * int -> color
val fromArgb : int * int * int * int -> color

// bitmaps
type bitmap = System.Drawing.Bitmap
val mk       : int -> int -> bitmap
val setPixel : color -> int * int -> bitmap -> unit
val setLine  : color -> int * int -> int * int -> bitmap -> unit
val setBox   : color -> int * int -> int * int -> bitmap -> unit

// read a bitmap file
val fromFile : string -> bitmap

// save a bitmap as a png file
val toPngFile : string -> bitmap -> unit

// show bitmap in a gui
val show : string -> bitmap -> unit

// start a simple app
val runSimpleApp : string -> int -> int
                -> (bitmap -> unit) -> unit

type KeyEventArgs = System.Windows.Forms.KeyEventArgs
type point = int * int // A point (x,y) in the plane
type colour = int * int * int //(red, green, blue), 0...255
type figure =
  | Circle of point * int * colour // Defined by center, radius and colour
  | Rectangle of point * point * colour // Defined by corners bottom-left, top-right and colour
  | Mix of figure * figure // Combine figures with mixed colours at overlap
let rec colorAt (x,y) figure =
  match figure with
  | Circle ((cx,cy), r, col) ->
      if (x-cx)*(x-cx)+(y-cy)*(y-cy) <= r*r // Uses pythagoras to dertermine distance to center
      then Some col else None
  | Rectangle ((x0,y0), (x1,y1), col) ->
      if x0 <=x && x <= x1 && y0 <= y && y <= y1 // within corners
      then Some col else None
  | Mix (f1,f2) ->
      match (colourAt (x,y) f1, colourAt (x,y) f2) with
      | (None, c) -> c // No overlap
      | (c, None) -> c // No overlap
      | (Some (r1,g1,b1), Some (r2,g2,b2)) -> // average colour
         Some ((r1+r2)/2, (g1+g2)/2, (b1+b2)/2)

// start an app that can listen to key-events
val runApp : string -> int -> int
          -> (int -> int -> 's -> bitmap)
          -> ('s -> KeyEventArgs -> 's option)
          -> 's -> unit
