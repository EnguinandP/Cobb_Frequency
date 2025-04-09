open Combinators

(* preconditions for list *)
let is_sized (n, l) = List.length l <= n
let has_same_size (s, l) = List.length l = s
let is_empty l = l = []
let if_head_map_f_else_true f = function h :: t -> f h | _ -> true

let rec is_sorted = function
  | h1 :: h2 :: t -> if h1 <= h2 then is_sorted (h2 :: t) else false
  | _ -> true

let rec is_duplicate = function
  | h1 :: h2 :: t -> if h1 = h2 then is_duplicate (h2 :: t) else false
  | _ -> true

let rec is_even_list =
  function
  | [] -> true
  | h :: t -> h mod 2 = 0 && is_even_list t

let is_unique l =
  let len = List.length l in
  let set = Hashtbl.create len in
  let () = List.iter (fun x -> Hashtbl.replace set x ()) l in
  len = Hashtbl.length set

(* units for filter functions *)

let rec has_duplicate_int_x l x =
  match l with [] -> true | h :: t -> h = x && has_duplicate_int_x t x

let rec num_black t h : bool =
  match t with
  | Rbtleaf -> h = 0
  | Rbtnode (c, l, _, r) ->
      if c then num_black l (h - 1) && num_black r (h - 1)
      else num_black l h && num_black r h

(* No red node has a red child *)
let rec no_red_red t : bool =
  match t with
  | Rbtleaf -> true
  | Rbtnode (c, l, _, r) -> (
      if not c then no_red_red l && no_red_red r
      else (* c is true *)
        match (l, r) with
        | Rbtnode (c', _, _, _), Rbtnode (c'', _, _, _) ->
            (not c') && (not c'') && no_red_red l && no_red_red r
        | Rbtnode (c', _, _, _), Rbtleaf -> (not c') && no_red_red l
        | Rbtleaf, Rbtnode (c'', _, _, _) -> (not c'') && no_red_red r
        | Rbtleaf, Rbtleaf -> true)

let rb_root_color t c : bool =
  match t with Rbtleaf -> false | Rbtnode (c', _, _, _) -> c = c'

let rec rbtree_invariant t h : bool =
  match t with
  | Rbtleaf -> h = 0
  | Rbtnode (c, l, _, r) ->
      if not c then rbtree_invariant l (h - 1) && rbtree_invariant r (h - 1)
      else
        ((not (rb_root_color l true)) && not (rb_root_color r true))
        && rbtree_invariant l h && rbtree_invariant r h

let rec rbdepth t =
  match t with
  | Rbtleaf -> 0
  | Rbtnode (_, l, _, r) -> 1 + max (rbdepth l) (rbdepth r)

let rec depth t =
  match t with Leaf -> 0 | Node (_, l, r) -> 1 + max (depth l) (depth r)

let rec complete t =
  match t with
  | Leaf -> true
  | Node (_, l, r) -> complete l && complete r && depth l = depth r

let rec lower_bound t x =
  match t with
  | Leaf -> true
  | Node (y, l, r) -> x <= y && lower_bound l x && lower_bound r x

let rec upper_bound t x =
  match t with
  | Leaf -> true
  | Node (y, l, r) -> y <= x && upper_bound l x && upper_bound r x

let rec bst t =
  match t with
  | Leaf -> true
  | Node (x, l, r) -> bst l && bst r && upper_bound l x && lower_bound r x

let is_leaf t = match t with Leaf -> true | _ -> false

let rec data_value_scales_by_size t =
  match t with
  | Leaf -> true
  | Node (x, l, r) ->
      x = depth t && data_value_scales_by_size l && data_value_scales_by_size r

let rec rbtree_inv_as_data_in t inv color =
  match t with
  | Rbtleaf -> true
  | Rbtnode (true, Rbtleaf, _, Rbtleaf) -> true
  | Rbtnode (c, l, x, r) ->
      let new_inv = if color then inv - 1 else if c then inv - 1 else inv - 2 in
      (if c = color then x = inv else true)
      && rbtree_inv_as_data_in l new_inv c
      && rbtree_inv_as_data_in r new_inv c

let rec rbtree_missing_case_when_black t color missing_node_color =
  match t with
  | Rbtleaf -> true
  | Rbtnode (c, l, _, r) ->
      if color then
        rbtree_missing_case_when_black l c missing_node_color
        && rbtree_missing_case_when_black r c missing_node_color
      else
        c = missing_node_color
        && rbtree_missing_case_when_black l c missing_node_color
        && rbtree_missing_case_when_black r c missing_node_color

(* Tests *)
let () = assert (rbtree_invariant (Rbtnode (true, Rbtleaf, 1, Rbtleaf)) 0)
let () = assert (not (rbtree_invariant (Rbtnode (true, Rbtleaf, 1, Rbtleaf)) 1))
let () = assert (rbtree_invariant (Rbtnode (false, Rbtleaf, 1, Rbtleaf)) 1)

let () =
  assert (
    rbtree_invariant
      (Rbtnode
         ( false,
           Rbtnode (false, Rbtleaf, 1, Rbtleaf),
           1,
           Rbtnode (false, Rbtleaf, 1, Rbtleaf) ))
      2)

let () =
  assert (
    not
      (rbtree_invariant
         (Rbtnode
            ( false,
              Rbtnode (false, Rbtleaf, 1, Rbtleaf),
              1,
              Rbtnode (true, Rbtleaf, 1, Rbtleaf) ))
         2))

let () =
  assert (
    not
      (rbtree_invariant
         (Rbtnode
            ( true,
              Rbtnode (true, Rbtleaf, 1, Rbtleaf),
              1,
              Rbtnode (true, Rbtleaf, 1, Rbtleaf) ))
         0))

let () = assert (bst Leaf)
let () = assert (bst (Node (1, Leaf, Leaf)))

let () =
  assert (
    bst (Node (1, Node (0, Leaf, Leaf), Leaf))
    && bst (Node (1, Leaf, Node (2, Leaf, Leaf))))

let () = assert (not (bst (Node (1, Node (2, Leaf, Leaf), Leaf))))
let () = assert (bst (Node (2, Node (1, Leaf, Leaf), Node (3, Leaf, Leaf))))
