let rec insertObj o liste =
  match liste with 
  |[] -> (1,o):: liste |(x,y):: list-> if y=o 
      then ((x+1),y)::list else(x,y):: insertObj o list;;

let rec insert p o liste = 
  match liste with 
  | [] ->(p,((1,o)::[]))::liste
  |(x,y)::list->
      if x = p then (x,(insertObj o y)):: list
      else (x,y) :: insert p o list;;

let rec decompte liste = 
  match liste with
  |[] -> []
  |(x,y):: list -> insert x y (decompte list);;

let rec longueur liste = match liste with 
  |[] -> 0
  |(x,y):: list -> x + longueur list;;

let rec aumoins_indexed n l = 
  match l with
  |[]-> []
  |(x,y)::l' -> if (longueur y) >= n then x :: (aumoins_indexed n l')
      else (aumoins_indexed n l');;

let aumoins n l = aumoins_indexed n (decompte l)