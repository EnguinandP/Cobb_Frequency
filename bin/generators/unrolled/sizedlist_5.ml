open Combinators
open Frequency_combinators
let rec sized_list_gen = fun s ->
  let (x_0) = sizecheck s in
  match x_0 with
  | true -> []
  | false ->
      let (w_base) = get_weight_idx 9 in
      let (w_recursive) = get_weight_idx 8 in
      let (base_case) = frequency_gen_list (w_base, (fun _ -> [])) in
      let (recursive_case) =
        base_case
          (w_recursive,
            (fun _ ->
               let (x_2) = subs s in
               let (x_3) =
                 fun s ->
                   let (x_0) = sizecheck s in
                   match x_0 with
                   | true -> []
                   | false ->
                       let (w_base) = get_weight_idx 7 in
                       let (w_recursive) = get_weight_idx 6 in
                       let (base_case) =
                         frequency_gen_list (w_base, (fun _ -> [])) in
                       let (recursive_case) =
                         base_case
                           (w_recursive,
                             (fun _ ->
                                let (x_2) = subs s in
                                let (x_3) =
                                  fun s ->
                                    let (x_0) = sizecheck s in
                                    match x_0 with
                                    | true -> []
                                    | false ->
                                        let (w_base) = get_weight_idx 5 in
                                        let (w_recursive) = get_weight_idx 4 in
                                        let (base_case) =
                                          frequency_gen_list
                                            (w_base, (fun _ -> [])) in
                                        let (recursive_case) =
                                          base_case
                                            (w_recursive,
                                              (fun _ ->
                                                 let (x_2) = subs s in
                                                 let (x_3) =
                                                   fun s ->
                                                     let (x_0) = sizecheck s in
                                                     match x_0 with
                                                     | true -> []
                                                     | false ->
                                                         let (w_base) =
                                                           get_weight_idx 3 in
                                                         let (w_recursive) =
                                                           get_weight_idx 2 in
                                                         let (base_case) =
                                                           frequency_gen_list
                                                             (w_base,
                                                               (fun _ -> [])) in
                                                         let (recursive_case)
                                                           =
                                                           base_case
                                                             (w_recursive,
                                                               (fun _ ->
                                                                  let 
                                                                    (x_2) =
                                                                    subs s in
                                                                  let 
                                                                    (x_3) =
                                                                    fun s ->
                                                                    let 
                                                                    (x_0) =
                                                                    sizecheck
                                                                    s in
                                                                    match x_0
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    []
                                                                    | 
                                                                    false ->
                                                                    let 
                                                                    (w_base)
                                                                    =
                                                                    get_weight_idx
                                                                    1 in
                                                                    let 
                                                                    (w_recursive)
                                                                    =
                                                                    get_weight_idx
                                                                    0 in
                                                                    let 
                                                                    (base_case)
                                                                    =
                                                                    frequency_gen_list
                                                                    (w_base,
                                                                    (fun _ ->
                                                                    [])) in
                                                                    let 
                                                                    (recursive_case)
                                                                    =
                                                                    base_case
                                                                    (w_recursive,
                                                                    (fun _ ->
                                                                    let 
                                                                    (x_2) =
                                                                    subs s in
                                                                    let 
                                                                    (x_3) =
                                                                    sized_list_gen
                                                                    x_2 in
                                                                    let 
                                                                    (x_4) =
                                                                    int_gen
                                                                    () in x_4
                                                                    :: x_3)) in
                                                                    recursive_case in
                                                                  let 
                                                                    (x_3) =
                                                                    x_3 x_2 in
                                                                  let 
                                                                    (x_4) =
                                                                    int_gen
                                                                    () in
                                                                  x_4 :: x_3)) in
                                                         recursive_case in
                                                 let (x_3) = x_3 x_2 in
                                                 let (x_4) = int_gen () in
                                                 x_4 :: x_3)) in
                                        recursive_case in
                                let (x_3) = x_3 x_2 in
                                let (x_4) = int_gen () in x_4 :: x_3)) in
                       recursive_case in
               let (x_3) = x_3 x_2 in let (x_4) = int_gen () in x_4 :: x_3)) in
      recursive_case