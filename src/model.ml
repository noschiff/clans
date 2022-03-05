type life = { nation : string }
type cell = Cell of life | Wall | Empty 
type world = cell array array

let new_world dimx dimy = Array.make_matrix dimx dimy Empty

let get_cell world x y = failwith "TODO"
let get_size world = (0, 0)
let get_nation cell = failwith "TODO"
let get_coordinate cell = (0, 0)
let simulate world = world
let clear_cell world x y = world
let inject_cell world x y nation = failwith "TODO"