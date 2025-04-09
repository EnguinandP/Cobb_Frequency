let black_height = 6 (* From Luck RBTree benchmark *)
let tree_height = 12 (* black_height * 2 *)
let int_gen () = QCheck.Gen.int (QCheck_runner.random_state ())
let nat_gen () = QCheck.Gen.nat (QCheck_runner.random_state ())

let black_height_gen () =
  QCheck.Gen.int_bound black_height (QCheck_runner.random_state ())

(* Tree height is double the height of black height in general *)
let tree_height_gen () =
  QCheck.Gen.int_bound tree_height (QCheck_runner.random_state ())

let bool_gen () = QCheck.Gen.bool (QCheck_runner.random_state ())

let int_range a b =
  QCheck.Gen.int_range (a + 1) (b + 2) (QCheck_runner.random_state ())

let gt_int_gen a =
  QCheck.Gen.int_range (a + 1) Int.max_int (QCheck_runner.random_state ())

let freq_gen size ~base_case ~recursive_case =
  QCheck.Gen.frequency
    [ (1, base_case); (size, recursive_case) ]
    (QCheck_runner.random_state ())

(* re-implementing unit functions from Cobb *)
let sizecheck s = s <= 0
let subs s = s - 1
let incr s = s + 1
let double s = s * 2
let list_mem l x = List.mem x l

(* for Err/ Exn *)
exception BailOut

(* int list gen of size s *)
let rec int_list_size_gen s =
  if s <= 0 then [] else int_gen () :: int_list_size_gen (s - 1)

(* default int list gen with size s *)
(* achievable with as an a' arbitrary *)
let int_list_gen () =
  let size = nat_gen () in
  int_list_size_gen size

type 'a rbtree = Rbtleaf | Rbtnode of bool * 'a rbtree * 'a * 'a rbtree

let rec count_rbtree_nodes t : int =
  match t with
  | Rbtleaf -> 1
  | Rbtnode (_, l, _, r) -> 1 + count_rbtree_nodes l + count_rbtree_nodes r

(* TODO: Somehow I would want a slight amount of variation in height here ... *)

(** This is our default rbtree generator *)
let rec gen_sized_int_rbtree size =
  if size == 0 then Rbtleaf
  else
    let c = bool_gen () in
    let l = gen_sized_int_rbtree (subs size) in
    let v = int_gen () in
    let r = gen_sized_int_rbtree (subs size) in
    Rbtnode (c, l, v, r)

let gen_sized_int_root_red_rbtree size =
  if size == 0 then Rbtleaf
  else
    let c = true in
    let l = gen_sized_int_rbtree (subs size) in
    let v = int_gen () in
    let r = gen_sized_int_rbtree (subs size) in
    Rbtnode (c, l, v, r)

let default_rbtree_gen () =
  let size = tree_height_gen () in
  gen_sized_int_rbtree size

type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

let tree_gen () =
  let size = tree_height_gen () in
  let rec gen_tree s =
    if s == 0 then Leaf
    else
      freq_gen s
        ~base_case:(fun _ -> Leaf)
        ~recursive_case:(fun _ ->
          let l = gen_tree (subs s) in
          let v = int_gen () in
          let r = gen_tree (subs s) in
          Node (v, l, r))
  in
  gen_tree size

let () = assert (gt_int_gen 0 > 0)
