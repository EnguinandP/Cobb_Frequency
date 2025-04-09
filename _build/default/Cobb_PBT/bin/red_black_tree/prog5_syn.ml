open Combinators

let rec rbtree_gen = fun inv ->
  fun color ->
    fun h ->
      let (xccc29) = sizecheck h in
      match xccc29 with
      | true ->
          (match color with
           | true -> Rbtleaf
           | false ->
               let (xccc30) = bool_gen () in
               (match xccc30 with
                | true -> Rbtleaf
                | false ->
                    let (xccc31) = Rbtleaf in
                    let (xccc32) = int_gen () in
                    let (xccc33) = Rbtleaf in
                    Rbtnode (true, xccc33, xccc32, xccc31)))
      | false ->
          (match color with
           | true ->
               let (xccc34) = subs inv in
               let (xccc35) = rbtree_gen xccc34 in
               let (xccc36) = xccc35 false in
               let (xccc37) = subs h in
               let (lt2) = xccc36 xccc37 in
               let (xccc38) = subs inv in
               let (xccc39) = rbtree_gen xccc38 in
               let (xccc40) = xccc39 false in
               let (xccc41) = subs h in
               let (rt2) = xccc40 xccc41 in
               let (xccc42) = int_gen () in Rbtnode (false, lt2, xccc42, rt2)
           | false ->
               let (c) = bool_gen () in
               (match c with
                | true ->
                    let (idx4) = inv in
                    let (idx68ccc0) = subs idx4 in
                    let (idx79) = rbtree_gen idx68ccc0 in
                    let (idx0) = true in
                    let (idx80) = idx79 idx0 in
                    let (idx6) = h in
                    let (idx81ccc24) = idx80 idx6 in
                    let (idx3ccc18) = int_gen () in
                    let (idx81ccc23) = idx80 idx6 in
                    Rbtnode (idx0, idx81ccc23, idx3ccc18, idx81ccc24)
                | false ->
                    let (xccc43) = subs inv in
                    let (xccc44) = subs xccc43 in
                    let (xccc45) = rbtree_gen xccc44 in
                    let (xccc46) = xccc45 false in
                    let (xccc47) = subs h in
                    let (lt4) = xccc46 xccc47 in
                    let (xccc48) = subs inv in
                    let (xccc49) = subs xccc48 in
                    let (xccc50) = rbtree_gen xccc49 in
                    let (xccc51) = xccc50 false in
                    let (xccc52) = subs h in
                    let (rt4) = xccc51 xccc52 in
                    let (xccc53) = int_gen () in
                    Rbtnode (false, lt4, xccc53, rt4)))