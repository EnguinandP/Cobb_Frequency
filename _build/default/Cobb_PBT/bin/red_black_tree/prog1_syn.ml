open Combinators

let rec rbtree_gen = fun inv ->
  fun color ->
    fun h ->
      let (xccc37) = sizecheck h in
      match xccc37 with
      | true ->
          (match color with
           | true -> Rbtleaf
           | false ->
               let (xccc38) = bool_gen () in
               (match xccc38 with
                | true -> Rbtleaf
                | false ->
                    let (xccc39) = Rbtleaf in
                    let (xccc40) = int_gen () in
                    let (xccc41) = Rbtleaf in
                    Rbtnode (true, xccc41, xccc40, xccc39)))
      | false ->
          (match color with
           | true ->
               let (xccc42) = subs inv in
               let (xccc43) = rbtree_gen xccc42 in
               let (xccc44) = xccc43 false in
               let (xccc45) = subs h in
               let (lt2) = xccc44 xccc45 in
               let (xccc46) = subs inv in
               let (xccc47) = rbtree_gen xccc46 in
               let (xccc48) = xccc47 false in
               let (xccc49) = subs h in
               let (rt2) = xccc48 xccc49 in
               let (xccc50) = int_gen () in Rbtnode (false, lt2, xccc50, rt2)
           | false ->
               let (c) = bool_gen () in
               (match c with
                | true ->
                    let (xccc51) = subs inv in
                    let (xccc52) = rbtree_gen xccc51 in
                    let (xccc53) = xccc52 true in
                    let (lt3) = xccc53 h in
                    let (xccc54) = subs inv in
                    let (xccc55) = rbtree_gen xccc54 in
                    let (xccc56) = xccc55 true in
                    let (rt3) = xccc56 h in
                    let (xccc57) = int_gen () in
                    Rbtnode (true, lt3, xccc57, rt3)
                | false ->
                    let (xccc58) = subs inv in
                    let (xccc59) = subs xccc58 in
                    let (xccc60) = rbtree_gen xccc59 in
                    let (xccc61) = xccc60 false in
                    let (xccc62) = subs h in
                    let (lt4) = xccc61 xccc62 in
                    let (xccc63) = subs inv in
                    let (xccc64) = subs xccc63 in
                    let (xccc65) = rbtree_gen xccc64 in
                    let (xccc66) = xccc65 false in
                    let (xccc67) = subs h in
                    let (rt4) = xccc66 xccc67 in
                    let (xccc68) = int_gen () in
                    Rbtnode (false, lt4, xccc68, rt4)))