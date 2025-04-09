open Combinators

let rec rbtree_gen =
 fun inv ->
  fun color ->
   fun h ->
    let xccc2 = sizecheck h in
    match xccc2 with
    | true -> (
        match color with
        | true -> Rbtleaf
        | false -> (
            let idx480 = bool_gen () in
            match idx480 with
            | true ->
                let idx2ccc39 = Rbtleaf in
                let idx3ccc37 = int_gen () in
                let idx2ccc38 = Rbtleaf in
                let idx0ccc3 = true in
                Rbtnode (idx0ccc3, idx2ccc38, idx3ccc37, idx2ccc39)
            | false -> Rbtleaf))
    | false -> (
        match color with
        | true ->
            let idx4ccc0 = inv in
            let idx174ccc0 = subs idx4ccc0 in
            let idx389 = rbtree_gen idx174ccc0 in
            let idx1ccc26 = false in
            let idx390 = idx389 idx1ccc26 in
            let idx6ccc0 = h in
            let idx173ccc8 = subs idx6ccc0 in
            let idx391ccc32 = idx390 idx173ccc8 in
            let idx3ccc49 = int_gen () in
            let idx391ccc31 = idx390 idx173ccc8 in
            let idx1ccc0 = false in
            Rbtnode (idx1ccc0, idx391ccc31, idx3ccc49, idx391ccc32)
        | false -> (
            let idx481 = bool_gen () in
            match idx481 with
            | true ->
                let idx4ccc2 = inv in
                let idx177ccc0 = subs idx4ccc2 in
                let idx263 = rbtree_gen idx177ccc0 in
                let idx0ccc22 = true in
                let idx264 = idx263 idx0ccc22 in
                let idx6ccc33 = h in
                let idx265ccc24 = idx264 idx6ccc33 in
                let idx3ccc43 = int_gen () in
                let idx265ccc23 = idx264 idx6ccc33 in
                let idx0ccc2 = true in
                Rbtnode (idx0ccc2, idx265ccc23, idx3ccc43, idx265ccc24)
            | false ->
                let idx4ccc2 = inv in
                let idx177ccc0 = subs idx4ccc2 in
                let idx263 = rbtree_gen idx177ccc0 in
                let idx0ccc22 = true in
                let idx264 = idx263 idx0ccc22 in
                let idx6ccc33 = h in
                idx264 idx6ccc33))
