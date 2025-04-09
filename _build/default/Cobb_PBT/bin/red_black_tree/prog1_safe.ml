open Combinators

let rec rbtree_gen (inv : int) (color : bool) (h : int) : int rbtree =
  if sizecheck h then
    if color then Rbtleaf
    else if bool_gen () then Rbtleaf
    else Rbtnode (true, Rbtleaf, int_gen (), Rbtleaf)
  else if color then
    let (lt2 : int rbtree) = rbtree_gen (subs inv) false (subs h) in
    let (rt2 : int rbtree) = rbtree_gen (subs inv) false (subs h) in
    Rbtnode (false, lt2, int_gen (), rt2)
  else
    let (c : bool) = bool_gen () in
    if c then
      let (lt3 : int rbtree) = rbtree_gen (subs inv) true h in
      let (rt3 : int rbtree) = rbtree_gen (subs inv) true h in
      Rbtnode (true, lt3, int_gen (), rt3)
    else
      let (lt4 : int rbtree) = rbtree_gen (subs (subs inv)) false (subs h) in
      let (rt4 : int rbtree) = rbtree_gen (subs (subs inv)) false (subs h) in
      Rbtnode (false, lt4, int_gen (), rt4)
