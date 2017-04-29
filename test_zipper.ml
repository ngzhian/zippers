open Zipper

(* Some helpers for composing actions on zippers and printing lists *)
let bind (ma : 'a option) (f : 'a -> 'mb) : 'mb =
  match ma with
  | None  -> None
  | Some a -> f a

let and_then f ma = bind ma f

let some_string_of_zipper z =
  match z with
  | None  -> "Error in traversal"
  | Some z -> String.concat "; " (List.map string_of_int (list_of_zipper z))

(* construct a simple list zipper *)
let z = zipper_of_list [1; 2; 3; 4]

let print_zipper_structure z =
  string_of_zipper z string_of_int
  |> (fun (xs, ys) -> (String.concat ", " xs) ^ "||" ^ (String.concat ", " ys))

let twice_forward_once_backward_and_set =
  z
  |> forward
  |> and_then forward
  |> and_then backward
  |> and_then (set 5)

let _ = print_endline (some_string_of_zipper twice_forward_once_backward_and_set)
let _ = print_endline (some_string_of_zipper (Some z))
let _ = match twice_forward_once_backward_and_set with
  | None -> ()
  | Some z -> print_endline (print_zipper_structure z)
