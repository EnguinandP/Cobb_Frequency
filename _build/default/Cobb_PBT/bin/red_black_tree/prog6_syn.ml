open Combinators

let rec rbtree_gen = fun inv ->
  fun color ->
    fun h ->
      let (xccc25) = sizecheck h in
      match xccc25 with
      | true ->
          (match color with
           | true -> Rbtleaf
           | false ->
               let (xccc26) = bool_gen () in
               (match xccc26 with
                | true -> Rbtleaf
                | false ->
                    let (xccc27) = Rbtleaf in
                    let (xccc28) = int_gen () in
                    let (xccc29) = Rbtleaf in
                    Rbtnode (true, xccc29, xccc28, xccc27)))
      | false ->
          (match color with
           | true ->
               let (xccc30) = subs inv in
               let (xccc31) = rbtree_gen xccc30 in
               let (xccc32) = xccc31 false in
               let (xccc33) = subs h in
               let (lt2) = xccc32 xccc33 in
               let (xccc34) = subs inv in
               let (xccc35) = rbtree_gen xccc34 in
               let (xccc36) = xccc35 false in
               let (xccc37) = subs h in
               let (rt2) = xccc36 xccc37 in
               let (xccc38) = int_gen () in Rbtnode (false, lt2, xccc38, rt2)
           | false ->
               let (idx10) = bool_gen () in
               (match idx10 with
                | true ->
                    let (xccc39) = subs inv in
                    let (xccc40) = rbtree_gen xccc39 in
                    let (xccc41) = xccc40 true in
                    let (lt3) = xccc41 h in
                    let (xccc42) = subs inv in
                    let (xccc43) = rbtree_gen xccc42 in
                    let (xccc44) = xccc43 true in
                    let (rt3) = xccc44 h in
                    let (xccc45) = int_gen () in
                    Rbtnode (true, lt3, xccc45, rt3)
                | false ->
                    let (idx4) = inv in
                    let (idx68ccc0) = subs idx4 in
                    let (idx79) = rbtree_gen idx68ccc0 in
                    let (idx0) = true in
                    let (idx80) = idx79 idx0 in let (idx6) = h in idx80 idx6))