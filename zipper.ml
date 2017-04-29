(** A list zipper.
    It maintains focus on 1 particular element in the list. *)
type 'a list_zipper = 'a list * 'a list

(** Builds a list zipper from a list. *)
let zipper_of_list xs = ([], xs)

(** Move the focus in the list zipper forward *)
let forward z =
  match z with
  | (bs, x::xs) -> Some (x::bs, xs)
  | (_, []) -> None

(** Move the focus in the list zipper backward *)
let backward z =
  match z with
  | (b::bs, xs) -> Some (bs, b::xs)
  | ([], _) -> None

(** Set the current focused value in the list to x *)
let set x z =
  match z with
  | (bs, _::xs) -> Some (bs, x::xs)
  | (_, []) -> None

(** Converts a list zipper back into a list. *)
let list_of_zipper (z : 'a list_zipper) =
  match z with
  | xs, ys -> List.rev xs @ ys
  (* let rec go z acc = *)
  (*   match z with *)
  (*   | [], [] -> acc *)
  (*   | x::xs, [] -> go (xs, []) (x::acc) *)
  (*   | [], ys -> ys *)
  (*   | x::xs, ys -> go (xs, []) (x::ys) *)
  (* in go z [] *)

let string_of_zipper z string_of_el : string list * string list =
  match z with
  | xs, ys -> List.map string_of_el xs, List.map string_of_el ys

(* a binary tree *)
type 'a tree =
  | Empty
  | Node of 'a * ('a tree) * ('a tree)

(* example tree *)
let example_tree =
  Node(
    'H',
    Node('E',
         Node('L', Empty, Empty),
         Empty),
    Node('L',
        Node('O', Empty, Empty),
        Empty)
  )

type direction =
  | Left
  | Right

type 'a tree_zipper =
  'a tree * (direction * 'a * 'a tree) list

let left z = match z with
  | Node(n, l, r), cs -> Some(l, (Left, n, r)::cs)
  | _, _ -> None

let right z = match z with
  | Node(n, l, r), cs -> Some(r, (Right, n, r)::cs)
  | _, _ -> None

let up z = match z with
  | t, (Left, node, tree)::cs -> Some(Node (node, t, tree))
  | t, (Right, node, tree)::cs -> Some(Node (node, tree, t))
  | _, [] -> None
