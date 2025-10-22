open Combinators
open Frequency_combinators

let rec rbtree_gen =
 fun inv ->
  fun color ->
   fun h ->
    let x_11 = sizecheck h in
    match x_11 with
    | true -> (
        match color with
        | true -> Rbtleaf
        | false ->
            let w0 = get_weight_idx 0 in
            let w1 = get_weight_idx 1 in
            let base_case = frequency_gen_list (w0, fun _ -> Rbtleaf) in
            let recursive_case =
              base_case
                ( w1,
                  fun _ ->
                    let x_13 = Rbtleaf in
                    let x_14 = int_gen () in
                    let x_15 = Rbtleaf in
                    Rbtnode (true, x_15, x_14, x_13) )
            in
            recursive_case)
    | false -> (
        match color with
        | true ->
            (* let x_16 = subs inv in
            let x_17 = rbtree_gen x_16 in
            let x_18 = x_17 false in
            let x_19 = subs h in
            let lt2 = x_18 x_19 in *)

            let sinv = subs inv in
            let sh = subs h in
            let scolor = false in
            
            (* begin lt2 *)
            let lt2 =
            let x_11 = sizecheck sh in
            match x_11 with
            | true -> (
                    let w0 = get_weight_idx 2 in
                    let w1 = get_weight_idx 3 in
                    let base_case = frequency_gen_list (w0, fun _ -> Rbtleaf) in
                    let recursive_case =
                      base_case
                        ( w1,
                          fun _ ->
                            let x_13 = Rbtleaf in
                            let x_14 = int_gen () in
                            let x_15 = Rbtleaf in
                            Rbtnode (true, x_15, x_14, x_13) )
                    in
                    recursive_case)
            | false -> (
                
                    let w0 = get_weight_idx 4 in
                    let w1 = get_weight_idx 5 in
                    let base_case =
                      frequency_gen_list
                        ( w0,
                          fun _ ->
                            let x_25 = subs sinv in
                            let x_26 = rbtree_gen x_25 in
                            let x_27 = x_26 true in
                            let slt3 = x_27 sh in

                            let x_28 = subs sinv in
                            let x_29 = rbtree_gen x_28 in
                            let x_30 = x_29 true in
                            let srt3 = x_30 sh in

                            let x_31 = int_gen () in
                            Rbtnode (true, slt3, x_31, srt3) )
                    in
                    let recursive_case =
                      base_case
                        ( w1,
                          fun _ ->
                            let x_32 = subs sinv in
                            let x_33 = subs x_32 in
                            let x_34 = rbtree_gen x_33 in
                            let x_35 = x_34 false in
                            let x_36 = subs sh in
                            let slt4 = x_35 x_36 in

                            let x_37 = subs sinv in
                            let x_38 = subs x_37 in
                            let x_39 = rbtree_gen x_38 in
                            let x_40 = x_39 false in
                            let x_41 = subs sh in
                            let srt4 = x_40 x_41 in
                            
                            let x_42 = int_gen () in
                            Rbtnode (false, slt4, x_42, srt4) )     
            in
            recursive_case)
            in

            (* end lt2 *)

            (* let x_20 = subs inv in
            let x_21 = rbtree_gen x_20 in
            let x_22 = x_21 false in
            let x_23 = subs h in
            let rt2 = x_22 x_23 in *)

            let sinv = subs inv in
            let sh = subs h in
            let scolor = false in
            
            (* begin rt2 *)
            let rt2 =
            let x_11 = sizecheck sh in
            match x_11 with
            | true -> (
                    let w0 = get_weight_idx 6 in
                    let w1 = get_weight_idx 7 in
                    let base_case = frequency_gen_list (w0, fun _ -> Rbtleaf) in
                    let recursive_case =
                      base_case
                        ( w1,
                          fun _ ->
                            let x_13 = Rbtleaf in
                            let x_14 = int_gen () in
                            let x_15 = Rbtleaf in
                            Rbtnode (true, x_15, x_14, x_13) )
                    in
                    recursive_case)
            | false -> (
                    let w0 = get_weight_idx 8 in
                    let w1 = get_weight_idx 9 in
                    let base_case =
                      frequency_gen_list
                        ( w0,
                          fun _ ->
                            let x_25 = subs sinv in
                            let x_26 = rbtree_gen x_25 in
                            let x_27 = x_26 true in
                            let slt3 = x_27 sh in

                            let x_28 = subs sinv in
                            let x_29 = rbtree_gen x_28 in
                            let x_30 = x_29 true in
                            let srt3 = x_30 sh in

                            let x_31 = int_gen () in
                            Rbtnode (true, slt3, x_31, srt3) )
                    in
                    let recursive_case =
                      base_case
                        ( w1,
                          fun _ ->
                            let x_32 = subs sinv in
                            let x_33 = subs x_32 in
                            let x_34 = rbtree_gen x_33 in
                            let x_35 = x_34 false in
                            let x_36 = subs sh in
                            let slt4 = x_35 x_36 in

                            let x_37 = subs sinv in
                            let x_38 = subs x_37 in
                            let x_39 = rbtree_gen x_38 in
                            let x_40 = x_39 false in
                            let x_41 = subs sh in
                            let srt4 = x_40 x_41 in
                            
                            let x_42 = int_gen () in
                            Rbtnode (false, slt4, x_42, srt4) )     
            in
            recursive_case)
            in
            (* end rt2 *)

            let x_24 = int_gen () in
            Rbtnode (false, lt2, x_24, rt2)
        | false ->
            let w0 = get_weight_idx 10 in
            let w1 = get_weight_idx 11 in
            let base_case =
              frequency_gen_list
                ( w0,
                  fun _ ->
                    (* let x_25 = subs inv in
                    let x_26 = rbtree_gen x_25 in
                    let x_27 = x_26 true in
                    let lt3 = x_27 h in *)

                    let sinv = subs inv in
                    let sh = h in
                    let scolor = true in
                    
                    (* begin lt3 *)
                    let lt3 =
                    let x_11 = sizecheck sh in
                    match x_11 with
                    | true -> (
                        Rbtleaf
                        )
                    | false -> (
                            let x_16 = subs sinv in
                            let x_17 = rbtree_gen x_16 in
                            let x_18 = x_17 false in
                            let x_19 = subs sh in
                            let slt2 = x_18 x_19 in

                            let x_20 = subs sinv in
                            let x_21 = rbtree_gen x_20 in
                            let x_22 = x_21 false in
                            let x_23 = subs sh in
                            let srt2 = x_22 x_23 in

                            let x_24 = int_gen () in
                            Rbtnode (false, slt2, x_24, srt2)
                         )
                    in
                    (* end lt3 *)

                    (* let x_28 = subs inv in
                    let x_29 = rbtree_gen x_28 in
                    let x_30 = x_29 true in
                    let rt3 = x_30 h in *)

                    let sinv = subs inv in
                    let sh = h in
                    let scolor = true in
                    
                    (* begin rt3 *)
                    let rt3 =
                    let x_11 = sizecheck sh in
                    match x_11 with
                    | true -> (
                        Rbtleaf
                        )
                    | false -> (
                            let x_16 = subs sinv in
                            let x_17 = rbtree_gen x_16 in
                            let x_18 = x_17 false in
                            let x_19 = subs sh in
                            let slt2 = x_18 x_19 in

                            let x_20 = subs sinv in
                            let x_21 = rbtree_gen x_20 in
                            let x_22 = x_21 false in
                            let x_23 = subs sh in
                            let srt2 = x_22 x_23 in

                            let x_24 = int_gen () in
                            Rbtnode (false, slt2, x_24, srt2)
                        )
                    in
                    (* end rt3 *)

                    let x_31 = int_gen () in
                    Rbtnode (true, lt3, x_31, rt3) )
            in
            let recursive_case =
              base_case
                ( w1,
                  fun _ ->
                    (* let x_32 = subs inv in
                    let x_33 = subs x_32 in
                    let x_34 = rbtree_gen x_33 in
                    let x_35 = x_34 false in
                    let x_36 = subs h in
                    let lt4 = x_35 x_36 in *)

                    let x_32 = subs inv in
                    let sinv = subs x_32 in
                    let sh = subs h in
                    let scolor = false in
                    
                    (* begin lt4 *)
                    let lt4 =
                    let x_11 = sizecheck sh in
                    match x_11 with
                    | true -> (
                            let w0 = get_weight_idx 12 in
                            let w1 = get_weight_idx 13 in
                            let base_case = frequency_gen_list (w0, fun _ -> Rbtleaf) in
                            let recursive_case =
                              base_case
                                ( w1,
                                  fun _ ->
                                    let x_13 = Rbtleaf in
                                    let x_14 = int_gen () in
                                    let x_15 = Rbtleaf in
                                    Rbtnode (true, x_15, x_14, x_13) )
                            in
                            recursive_case)
                    | false -> (
                            let w0 = get_weight_idx 14 in
                            let w1 = get_weight_idx 15 in
                            let base_case =
                              frequency_gen_list
                                ( w0,
                                  fun _ ->
                                    let x_25 = subs sinv in
                                    let x_26 = rbtree_gen x_25 in
                                    let x_27 = x_26 true in
                                    let slt3 = x_27 sh in

                                    let x_28 = subs sinv in
                                    let x_29 = rbtree_gen x_28 in
                                    let x_30 = x_29 true in
                                    let srt3 = x_30 sh in

                                    let x_31 = int_gen () in
                                    Rbtnode (true, slt3, x_31, srt3) )
                            in
                            let recursive_case =
                              base_case
                                ( w1,
                                  fun _ ->
                                    let x_32 = subs sinv in
                                    let x_33 = subs x_32 in
                                    let x_34 = rbtree_gen x_33 in
                                    let x_35 = x_34 false in
                                    let x_36 = subs sh in
                                    let slt4 = x_35 x_36 in

                                    let x_37 = subs sinv in
                                    let x_38 = subs x_37 in
                                    let x_39 = rbtree_gen x_38 in
                                    let x_40 = x_39 false in
                                    let x_41 = subs sh in
                                    let srt4 = x_40 x_41 in
                                    
                                    let x_42 = int_gen () in
                                    Rbtnode (false, slt4, x_42, srt4) )     
                    in
                    recursive_case)
                    in
                    (* end lt4 *)

                    (* let x_37 = subs inv in
                    let x_38 = subs x_37 in
                    let x_39 = rbtree_gen x_38 in
                    let x_40 = x_39 false in
                    let x_41 = subs h in
                    let rt4 = x_40 x_41 in *)

                    let x_32 = subs inv in
                    let sinv = subs x_32 in
                    let sh = subs h in
                    let scolor = false in
                    
                    (* begin rt4 *)
                    let rt4 =
                    let x_11 = sizecheck sh in
                    match x_11 with
                    | true -> (
                            let w0 = get_weight_idx 16 in
                            let w1 = get_weight_idx 17 in
                            let base_case = frequency_gen_list (w0, fun _ -> Rbtleaf) in
                            let recursive_case =
                              base_case
                                ( w1,
                                  fun _ ->
                                    let x_13 = Rbtleaf in
                                    let x_14 = int_gen () in
                                    let x_15 = Rbtleaf in
                                    Rbtnode (true, x_15, x_14, x_13) )
                            in
                            recursive_case)
                    | false -> (
                            let w0 = get_weight_idx 18 in
                            let w1 = get_weight_idx 19 in
                            let base_case =
                              frequency_gen_list
                                ( w0,
                                  fun _ ->
                                    let x_25 = subs sinv in
                                    let x_26 = rbtree_gen x_25 in
                                    let x_27 = x_26 true in
                                    let slt3 = x_27 sh in

                                    let x_28 = subs sinv in
                                    let x_29 = rbtree_gen x_28 in
                                    let x_30 = x_29 true in
                                    let srt3 = x_30 sh in

                                    let x_31 = int_gen () in
                                    Rbtnode (true, slt3, x_31, srt3) )
                            in
                            let recursive_case =
                              base_case
                                ( w1,
                                  fun _ ->
                                    let x_32 = subs sinv in
                                    let x_33 = subs x_32 in
                                    let x_34 = rbtree_gen x_33 in
                                    let x_35 = x_34 false in
                                    let x_36 = subs sh in
                                    let slt4 = x_35 x_36 in

                                    let x_37 = subs sinv in
                                    let x_38 = subs x_37 in
                                    let x_39 = rbtree_gen x_38 in
                                    let x_40 = x_39 false in
                                    let x_41 = subs sh in
                                    let srt4 = x_40 x_41 in
                                    
                                    let x_42 = int_gen () in
                                    Rbtnode (false, slt4, x_42, srt4) )     
                    in
                    recursive_case)
                    in
                    (* end rt4 *)

                    let x_42 = int_gen () in
                    Rbtnode (false, lt4, x_42, rt4) )
            in
            recursive_case)
