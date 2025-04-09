open Combinators

let rec rbtree_gen = fun inv ->
  fun color ->
    fun h ->
      let (xccc33) = sizecheck h in
      match xccc33 with
      | true ->
          (match color with
           | true -> Rbtleaf
           | false ->
               let (idx7) = bool_gen () in
               (match idx7 with
                | true -> Rbtleaf
                | false ->
                    let (idx2) = Rbtleaf in
                    let (idx3ccc10) = int_gen () in
                    let (idx0) = true in
                    Rbtnode (idx0, idx2, idx3ccc10, idx2)))
      | false ->
          (match color with
           | true ->
               let (xccc35) = subs inv in
               let (xccc36) = rbtree_gen xccc35 in
               let (xccc37) = xccc36 false in
               let (xccc38) = subs h in
               let (lt2) = xccc37 xccc38 in
               let (xccc39) = subs inv in
               let (xccc40) = rbtree_gen xccc39 in
               let (xccc41) = xccc40 false in
               let (xccc42) = subs h in
               let (rt2) = xccc41 xccc42 in
               let (xccc43) = int_gen () in Rbtnode (false, lt2, xccc43, rt2)
           | false ->
               let (c) = bool_gen () in
               (match c with
                | true ->
                    let (xccc44) = subs inv in
                    let (xccc45) = rbtree_gen xccc44 in
                    let (xccc46) = xccc45 true in
                    let (lt3) = xccc46 h in
                    let (xccc47) = subs inv in
                    let (xccc48) = rbtree_gen xccc47 in
                    let (xccc49) = xccc48 true in
                    let (rt3) = xccc49 h in
                    let (xccc50) = int_gen () in
                    Rbtnode (true, lt3, xccc50, rt3)
                | false ->
                    let (xccc51) = subs inv in
                    let (xccc52) = subs xccc51 in
                    let (xccc53) = rbtree_gen xccc52 in
                    let (xccc54) = xccc53 false in
                    let (xccc55) = subs h in
                    let (lt4) = xccc54 xccc55 in
                    let (xccc56) = subs inv in
                    let (xccc57) = subs xccc56 in
                    let (xccc58) = rbtree_gen xccc57 in
                    let (xccc59) = xccc58 false in
                    let (xccc60) = subs h in
                    let (rt4) = xccc59 xccc60 in
                    let (xccc61) = int_gen () in
                    Rbtnode (false, lt4, xccc61, rt4)))