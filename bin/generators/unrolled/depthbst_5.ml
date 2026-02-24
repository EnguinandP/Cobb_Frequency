open Combinators
open Frequency_combinators
let rec size_bst_gen = fun d ->
  fun lo ->
    fun hi ->
      let (x_0) = sizecheck d in
      match x_0 with
      | true -> Leaf
      | false ->
          let (x_1) = incr lo in
          let (x_2) = x_1 < hi in
          (match x_2 with
           | true ->
               let (x_3) = int_range lo in
               let (x) = x_3 hi in
               let (w_base) = get_weight_idx 61 in
               let (w_recursive) = get_weight_idx 60 in
               let (base_case) = frequency_gen_list (w_base, (fun _ -> Leaf)) in
               let (recursive_case) =
                 base_case
                   (w_recursive,
                     (fun _ ->
                        let (x_5) = subs d in
                        let (x_6) =
                          fun d ->
                            fun lo ->
                              fun hi ->
                                let (x_0) = sizecheck d in
                                match x_0 with
                                | true -> Leaf
                                | false ->
                                    let (x_1) = incr lo in
                                    let (x_2) = x_1 < hi in
                                    (match x_2 with
                                     | true ->
                                         let (x_3) = int_range lo in
                                         let (x) = x_3 hi in
                                         let (w_base) = get_weight_idx 59 in
                                         let (w_recursive) =
                                           get_weight_idx 58 in
                                         let (base_case) =
                                           frequency_gen_list
                                             (w_base, (fun _ -> Leaf)) in
                                         let (recursive_case) =
                                           base_case
                                             (w_recursive,
                                               (fun _ ->
                                                  let (x_5) = subs d in
                                                  let (x_6) =
                                                    fun d ->
                                                      fun lo ->
                                                        fun hi ->
                                                          let (x_0) =
                                                            sizecheck d in
                                                          match x_0 with
                                                          | true -> Leaf
                                                          | false ->
                                                              let (x_1) =
                                                                incr lo in
                                                              let (x_2) =
                                                                x_1 < hi in
                                                              (match x_2 with
                                                               | true ->
                                                                   let 
                                                                    (x_3) =
                                                                    int_range
                                                                    lo in
                                                                   let 
                                                                    (x) =
                                                                    x_3 hi in
                                                                   let 
                                                                    (w_base)
                                                                    =
                                                                    get_weight_idx
                                                                    57 in
                                                                   let 
                                                                    (w_recursive)
                                                                    =
                                                                    get_weight_idx
                                                                    56 in
                                                                   let 
                                                                    (base_case)
                                                                    =
                                                                    frequency_gen_list
                                                                    (w_base,
                                                                    (fun _ ->
                                                                    Leaf)) in
                                                                   let 
                                                                    (recursive_case)
                                                                    =
                                                                    base_case
                                                                    (w_recursive,
                                                                    (fun _ ->
                                                                    let 
                                                                    (x_5) =
                                                                    subs d in
                                                                    let 
                                                                    (x_6) =
                                                                    fun d ->
                                                                    fun lo ->
                                                                    fun hi ->
                                                                    let 
                                                                    (x_0) =
                                                                    sizecheck
                                                                    d in
                                                                    match x_0
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    Leaf
                                                                    | 
                                                                    false ->
                                                                    let 
                                                                    (x_1) =
                                                                    incr lo in
                                                                    let 
                                                                    (x_2) =
                                                                    x_1 < hi in
                                                                    (match x_2
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    let 
                                                                    (x_3) =
                                                                    int_range
                                                                    lo in
                                                                    let 
                                                                    (x) =
                                                                    x_3 hi in
                                                                    let 
                                                                    (w_base)
                                                                    =
                                                                    get_weight_idx
                                                                    55 in
                                                                    let 
                                                                    (w_recursive)
                                                                    =
                                                                    get_weight_idx
                                                                    54 in
                                                                    let 
                                                                    (base_case)
                                                                    =
                                                                    frequency_gen_list
                                                                    (w_base,
                                                                    (fun _ ->
                                                                    Leaf)) in
                                                                    let 
                                                                    (recursive_case)
                                                                    =
                                                                    base_case
                                                                    (w_recursive,
                                                                    (fun _ ->
                                                                    let 
                                                                    (x_5) =
                                                                    subs d in
                                                                    let 
                                                                    (x_6) =
                                                                    fun d ->
                                                                    fun lo ->
                                                                    fun hi ->
                                                                    let 
                                                                    (x_0) =
                                                                    sizecheck
                                                                    d in
                                                                    match x_0
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    Leaf
                                                                    | 
                                                                    false ->
                                                                    let 
                                                                    (x_1) =
                                                                    incr lo in
                                                                    let 
                                                                    (x_2) =
                                                                    x_1 < hi in
                                                                    (match x_2
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    let 
                                                                    (x_3) =
                                                                    int_range
                                                                    lo in
                                                                    let 
                                                                    (x) =
                                                                    x_3 hi in
                                                                    let 
                                                                    (w_base)
                                                                    =
                                                                    get_weight_idx
                                                                    53 in
                                                                    let 
                                                                    (w_recursive)
                                                                    =
                                                                    get_weight_idx
                                                                    52 in
                                                                    let 
                                                                    (base_case)
                                                                    =
                                                                    frequency_gen_list
                                                                    (w_base,
                                                                    (fun _ ->
                                                                    Leaf)) in
                                                                    let 
                                                                    (recursive_case)
                                                                    =
                                                                    base_case
                                                                    (w_recursive,
                                                                    (fun _ ->
                                                                    let 
                                                                    (x_5) =
                                                                    subs d in
                                                                    let 
                                                                    (x_6) =
                                                                    size_bst_gen
                                                                    x_5 in
                                                                    let 
                                                                    (x_7) =
                                                                    x_6 lo in
                                                                    let 
                                                                    (lt) =
                                                                    x_7 x in
                                                                    let 
                                                                    (x_8) =
                                                                    subs d in
                                                                    let 
                                                                    (x_9) =
                                                                    size_bst_gen
                                                                    x_8 in
                                                                    let 
                                                                    (x_10) =
                                                                    x_9 x in
                                                                    let 
                                                                    (rt) =
                                                                    x_10 hi in
                                                                    Node
                                                                    (x, lt,
                                                                    rt))) in
                                                                    recursive_case
                                                                    | 
                                                                    false ->
                                                                    Leaf) in
                                                                    let 
                                                                    (x_6) =
                                                                    x_6 x_5 in
                                                                    let 
                                                                    (x_7) =
                                                                    x_6 lo in
                                                                    let 
                                                                    (lt) =
                                                                    x_7 x in
                                                                    let 
                                                                    (x_8) =
                                                                    subs d in
                                                                    let 
                                                                    (x_9) =
                                                                    fun d ->
                                                                    fun lo ->
                                                                    fun hi ->
                                                                    let 
                                                                    (x_0) =
                                                                    sizecheck
                                                                    d in
                                                                    match x_0
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    Leaf
                                                                    | 
                                                                    false ->
                                                                    let 
                                                                    (x_1) =
                                                                    incr lo in
                                                                    let 
                                                                    (x_2) =
                                                                    x_1 < hi in
                                                                    (match x_2
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    let 
                                                                    (x_3) =
                                                                    int_range
                                                                    lo in
                                                                    let 
                                                                    (x) =
                                                                    x_3 hi in
                                                                    let 
                                                                    (w_base)
                                                                    =
                                                                    get_weight_idx
                                                                    51 in
                                                                    let 
                                                                    (w_recursive)
                                                                    =
                                                                    get_weight_idx
                                                                    50 in
                                                                    let 
                                                                    (base_case)
                                                                    =
                                                                    frequency_gen_list
                                                                    (w_base,
                                                                    (fun _ ->
                                                                    Leaf)) in
                                                                    let 
                                                                    (recursive_case)
                                                                    =
                                                                    base_case
                                                                    (w_recursive,
                                                                    (fun _ ->
                                                                    let 
                                                                    (x_5) =
                                                                    subs d in
                                                                    let 
                                                                    (x_6) =
                                                                    size_bst_gen
                                                                    x_5 in
                                                                    let 
                                                                    (x_7) =
                                                                    x_6 lo in
                                                                    let 
                                                                    (lt) =
                                                                    x_7 x in
                                                                    let 
                                                                    (x_8) =
                                                                    subs d in
                                                                    let 
                                                                    (x_9) =
                                                                    size_bst_gen
                                                                    x_8 in
                                                                    let 
                                                                    (x_10) =
                                                                    x_9 x in
                                                                    let 
                                                                    (rt) =
                                                                    x_10 hi in
                                                                    Node
                                                                    (x, lt,
                                                                    rt))) in
                                                                    recursive_case
                                                                    | 
                                                                    false ->
                                                                    Leaf) in
                                                                    let 
                                                                    (x_9) =
                                                                    x_9 x_8 in
                                                                    let 
                                                                    (x_10) =
                                                                    x_9 x in
                                                                    let 
                                                                    (rt) =
                                                                    x_10 hi in
                                                                    Node
                                                                    (x, lt,
                                                                    rt))) in
                                                                    recursive_case
                                                                    | 
                                                                    false ->
                                                                    Leaf) in
                                                                    let 
                                                                    (x_6) =
                                                                    x_6 x_5 in
                                                                    let 
                                                                    (x_7) =
                                                                    x_6 lo in
                                                                    let 
                                                                    (lt) =
                                                                    x_7 x in
                                                                    let 
                                                                    (x_8) =
                                                                    subs d in
                                                                    let 
                                                                    (x_9) =
                                                                    fun d ->
                                                                    fun lo ->
                                                                    fun hi ->
                                                                    let 
                                                                    (x_0) =
                                                                    sizecheck
                                                                    d in
                                                                    match x_0
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    Leaf
                                                                    | 
                                                                    false ->
                                                                    let 
                                                                    (x_1) =
                                                                    incr lo in
                                                                    let 
                                                                    (x_2) =
                                                                    x_1 < hi in
                                                                    (match x_2
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    let 
                                                                    (x_3) =
                                                                    int_range
                                                                    lo in
                                                                    let 
                                                                    (x) =
                                                                    x_3 hi in
                                                                    let 
                                                                    (w_base)
                                                                    =
                                                                    get_weight_idx
                                                                    49 in
                                                                    let 
                                                                    (w_recursive)
                                                                    =
                                                                    get_weight_idx
                                                                    48 in
                                                                    let 
                                                                    (base_case)
                                                                    =
                                                                    frequency_gen_list
                                                                    (w_base,
                                                                    (fun _ ->
                                                                    Leaf)) in
                                                                    let 
                                                                    (recursive_case)
                                                                    =
                                                                    base_case
                                                                    (w_recursive,
                                                                    (fun _ ->
                                                                    let 
                                                                    (x_5) =
                                                                    subs d in
                                                                    let 
                                                                    (x_6) =
                                                                    fun d ->
                                                                    fun lo ->
                                                                    fun hi ->
                                                                    let 
                                                                    (x_0) =
                                                                    sizecheck
                                                                    d in
                                                                    match x_0
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    Leaf
                                                                    | 
                                                                    false ->
                                                                    let 
                                                                    (x_1) =
                                                                    incr lo in
                                                                    let 
                                                                    (x_2) =
                                                                    x_1 < hi in
                                                                    (match x_2
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    let 
                                                                    (x_3) =
                                                                    int_range
                                                                    lo in
                                                                    let 
                                                                    (x) =
                                                                    x_3 hi in
                                                                    let 
                                                                    (w_base)
                                                                    =
                                                                    get_weight_idx
                                                                    47 in
                                                                    let 
                                                                    (w_recursive)
                                                                    =
                                                                    get_weight_idx
                                                                    46 in
                                                                    let 
                                                                    (base_case)
                                                                    =
                                                                    frequency_gen_list
                                                                    (w_base,
                                                                    (fun _ ->
                                                                    Leaf)) in
                                                                    let 
                                                                    (recursive_case)
                                                                    =
                                                                    base_case
                                                                    (w_recursive,
                                                                    (fun _ ->
                                                                    let 
                                                                    (x_5) =
                                                                    subs d in
                                                                    let 
                                                                    (x_6) =
                                                                    size_bst_gen
                                                                    x_5 in
                                                                    let 
                                                                    (x_7) =
                                                                    x_6 lo in
                                                                    let 
                                                                    (lt) =
                                                                    x_7 x in
                                                                    let 
                                                                    (x_8) =
                                                                    subs d in
                                                                    let 
                                                                    (x_9) =
                                                                    size_bst_gen
                                                                    x_8 in
                                                                    let 
                                                                    (x_10) =
                                                                    x_9 x in
                                                                    let 
                                                                    (rt) =
                                                                    x_10 hi in
                                                                    Node
                                                                    (x, lt,
                                                                    rt))) in
                                                                    recursive_case
                                                                    | 
                                                                    false ->
                                                                    Leaf) in
                                                                    let 
                                                                    (x_6) =
                                                                    x_6 x_5 in
                                                                    let 
                                                                    (x_7) =
                                                                    x_6 lo in
                                                                    let 
                                                                    (lt) =
                                                                    x_7 x in
                                                                    let 
                                                                    (x_8) =
                                                                    subs d in
                                                                    let 
                                                                    (x_9) =
                                                                    fun d ->
                                                                    fun lo ->
                                                                    fun hi ->
                                                                    let 
                                                                    (x_0) =
                                                                    sizecheck
                                                                    d in
                                                                    match x_0
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    Leaf
                                                                    | 
                                                                    false ->
                                                                    let 
                                                                    (x_1) =
                                                                    incr lo in
                                                                    let 
                                                                    (x_2) =
                                                                    x_1 < hi in
                                                                    (match x_2
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    let 
                                                                    (x_3) =
                                                                    int_range
                                                                    lo in
                                                                    let 
                                                                    (x) =
                                                                    x_3 hi in
                                                                    let 
                                                                    (w_base)
                                                                    =
                                                                    get_weight_idx
                                                                    45 in
                                                                    let 
                                                                    (w_recursive)
                                                                    =
                                                                    get_weight_idx
                                                                    44 in
                                                                    let 
                                                                    (base_case)
                                                                    =
                                                                    frequency_gen_list
                                                                    (w_base,
                                                                    (fun _ ->
                                                                    Leaf)) in
                                                                    let 
                                                                    (recursive_case)
                                                                    =
                                                                    base_case
                                                                    (w_recursive,
                                                                    (fun _ ->
                                                                    let 
                                                                    (x_5) =
                                                                    subs d in
                                                                    let 
                                                                    (x_6) =
                                                                    size_bst_gen
                                                                    x_5 in
                                                                    let 
                                                                    (x_7) =
                                                                    x_6 lo in
                                                                    let 
                                                                    (lt) =
                                                                    x_7 x in
                                                                    let 
                                                                    (x_8) =
                                                                    subs d in
                                                                    let 
                                                                    (x_9) =
                                                                    size_bst_gen
                                                                    x_8 in
                                                                    let 
                                                                    (x_10) =
                                                                    x_9 x in
                                                                    let 
                                                                    (rt) =
                                                                    x_10 hi in
                                                                    Node
                                                                    (x, lt,
                                                                    rt))) in
                                                                    recursive_case
                                                                    | 
                                                                    false ->
                                                                    Leaf) in
                                                                    let 
                                                                    (x_9) =
                                                                    x_9 x_8 in
                                                                    let 
                                                                    (x_10) =
                                                                    x_9 x in
                                                                    let 
                                                                    (rt) =
                                                                    x_10 hi in
                                                                    Node
                                                                    (x, lt,
                                                                    rt))) in
                                                                    recursive_case
                                                                    | 
                                                                    false ->
                                                                    Leaf) in
                                                                    let 
                                                                    (x_9) =
                                                                    x_9 x_8 in
                                                                    let 
                                                                    (x_10) =
                                                                    x_9 x in
                                                                    let 
                                                                    (rt) =
                                                                    x_10 hi in
                                                                    Node
                                                                    (x, lt,
                                                                    rt))) in
                                                                   recursive_case
                                                               | false ->
                                                                   Leaf) in
                                                  let (x_6) = x_6 x_5 in
                                                  let (x_7) = x_6 lo in
                                                  let (lt) = x_7 x in
                                                  let (x_8) = subs d in
                                                  let (x_9) =
                                                    fun d ->
                                                      fun lo ->
                                                        fun hi ->
                                                          let (x_0) =
                                                            sizecheck d in
                                                          match x_0 with
                                                          | true -> Leaf
                                                          | false ->
                                                              let (x_1) =
                                                                incr lo in
                                                              let (x_2) =
                                                                x_1 < hi in
                                                              (match x_2 with
                                                               | true ->
                                                                   let 
                                                                    (x_3) =
                                                                    int_range
                                                                    lo in
                                                                   let 
                                                                    (x) =
                                                                    x_3 hi in
                                                                   let 
                                                                    (w_base)
                                                                    =
                                                                    get_weight_idx
                                                                    43 in
                                                                   let 
                                                                    (w_recursive)
                                                                    =
                                                                    get_weight_idx
                                                                    42 in
                                                                   let 
                                                                    (base_case)
                                                                    =
                                                                    frequency_gen_list
                                                                    (w_base,
                                                                    (fun _ ->
                                                                    Leaf)) in
                                                                   let 
                                                                    (recursive_case)
                                                                    =
                                                                    base_case
                                                                    (w_recursive,
                                                                    (fun _ ->
                                                                    let 
                                                                    (x_5) =
                                                                    subs d in
                                                                    let 
                                                                    (x_6) =
                                                                    fun d ->
                                                                    fun lo ->
                                                                    fun hi ->
                                                                    let 
                                                                    (x_0) =
                                                                    sizecheck
                                                                    d in
                                                                    match x_0
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    Leaf
                                                                    | 
                                                                    false ->
                                                                    let 
                                                                    (x_1) =
                                                                    incr lo in
                                                                    let 
                                                                    (x_2) =
                                                                    x_1 < hi in
                                                                    (match x_2
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    let 
                                                                    (x_3) =
                                                                    int_range
                                                                    lo in
                                                                    let 
                                                                    (x) =
                                                                    x_3 hi in
                                                                    let 
                                                                    (w_base)
                                                                    =
                                                                    get_weight_idx
                                                                    41 in
                                                                    let 
                                                                    (w_recursive)
                                                                    =
                                                                    get_weight_idx
                                                                    40 in
                                                                    let 
                                                                    (base_case)
                                                                    =
                                                                    frequency_gen_list
                                                                    (w_base,
                                                                    (fun _ ->
                                                                    Leaf)) in
                                                                    let 
                                                                    (recursive_case)
                                                                    =
                                                                    base_case
                                                                    (w_recursive,
                                                                    (fun _ ->
                                                                    let 
                                                                    (x_5) =
                                                                    subs d in
                                                                    let 
                                                                    (x_6) =
                                                                    fun d ->
                                                                    fun lo ->
                                                                    fun hi ->
                                                                    let 
                                                                    (x_0) =
                                                                    sizecheck
                                                                    d in
                                                                    match x_0
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    Leaf
                                                                    | 
                                                                    false ->
                                                                    let 
                                                                    (x_1) =
                                                                    incr lo in
                                                                    let 
                                                                    (x_2) =
                                                                    x_1 < hi in
                                                                    (match x_2
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    let 
                                                                    (x_3) =
                                                                    int_range
                                                                    lo in
                                                                    let 
                                                                    (x) =
                                                                    x_3 hi in
                                                                    let 
                                                                    (w_base)
                                                                    =
                                                                    get_weight_idx
                                                                    39 in
                                                                    let 
                                                                    (w_recursive)
                                                                    =
                                                                    get_weight_idx
                                                                    38 in
                                                                    let 
                                                                    (base_case)
                                                                    =
                                                                    frequency_gen_list
                                                                    (w_base,
                                                                    (fun _ ->
                                                                    Leaf)) in
                                                                    let 
                                                                    (recursive_case)
                                                                    =
                                                                    base_case
                                                                    (w_recursive,
                                                                    (fun _ ->
                                                                    let 
                                                                    (x_5) =
                                                                    subs d in
                                                                    let 
                                                                    (x_6) =
                                                                    size_bst_gen
                                                                    x_5 in
                                                                    let 
                                                                    (x_7) =
                                                                    x_6 lo in
                                                                    let 
                                                                    (lt) =
                                                                    x_7 x in
                                                                    let 
                                                                    (x_8) =
                                                                    subs d in
                                                                    let 
                                                                    (x_9) =
                                                                    size_bst_gen
                                                                    x_8 in
                                                                    let 
                                                                    (x_10) =
                                                                    x_9 x in
                                                                    let 
                                                                    (rt) =
                                                                    x_10 hi in
                                                                    Node
                                                                    (x, lt,
                                                                    rt))) in
                                                                    recursive_case
                                                                    | 
                                                                    false ->
                                                                    Leaf) in
                                                                    let 
                                                                    (x_6) =
                                                                    x_6 x_5 in
                                                                    let 
                                                                    (x_7) =
                                                                    x_6 lo in
                                                                    let 
                                                                    (lt) =
                                                                    x_7 x in
                                                                    let 
                                                                    (x_8) =
                                                                    subs d in
                                                                    let 
                                                                    (x_9) =
                                                                    fun d ->
                                                                    fun lo ->
                                                                    fun hi ->
                                                                    let 
                                                                    (x_0) =
                                                                    sizecheck
                                                                    d in
                                                                    match x_0
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    Leaf
                                                                    | 
                                                                    false ->
                                                                    let 
                                                                    (x_1) =
                                                                    incr lo in
                                                                    let 
                                                                    (x_2) =
                                                                    x_1 < hi in
                                                                    (match x_2
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    let 
                                                                    (x_3) =
                                                                    int_range
                                                                    lo in
                                                                    let 
                                                                    (x) =
                                                                    x_3 hi in
                                                                    let 
                                                                    (w_base)
                                                                    =
                                                                    get_weight_idx
                                                                    37 in
                                                                    let 
                                                                    (w_recursive)
                                                                    =
                                                                    get_weight_idx
                                                                    36 in
                                                                    let 
                                                                    (base_case)
                                                                    =
                                                                    frequency_gen_list
                                                                    (w_base,
                                                                    (fun _ ->
                                                                    Leaf)) in
                                                                    let 
                                                                    (recursive_case)
                                                                    =
                                                                    base_case
                                                                    (w_recursive,
                                                                    (fun _ ->
                                                                    let 
                                                                    (x_5) =
                                                                    subs d in
                                                                    let 
                                                                    (x_6) =
                                                                    size_bst_gen
                                                                    x_5 in
                                                                    let 
                                                                    (x_7) =
                                                                    x_6 lo in
                                                                    let 
                                                                    (lt) =
                                                                    x_7 x in
                                                                    let 
                                                                    (x_8) =
                                                                    subs d in
                                                                    let 
                                                                    (x_9) =
                                                                    size_bst_gen
                                                                    x_8 in
                                                                    let 
                                                                    (x_10) =
                                                                    x_9 x in
                                                                    let 
                                                                    (rt) =
                                                                    x_10 hi in
                                                                    Node
                                                                    (x, lt,
                                                                    rt))) in
                                                                    recursive_case
                                                                    | 
                                                                    false ->
                                                                    Leaf) in
                                                                    let 
                                                                    (x_9) =
                                                                    x_9 x_8 in
                                                                    let 
                                                                    (x_10) =
                                                                    x_9 x in
                                                                    let 
                                                                    (rt) =
                                                                    x_10 hi in
                                                                    Node
                                                                    (x, lt,
                                                                    rt))) in
                                                                    recursive_case
                                                                    | 
                                                                    false ->
                                                                    Leaf) in
                                                                    let 
                                                                    (x_6) =
                                                                    x_6 x_5 in
                                                                    let 
                                                                    (x_7) =
                                                                    x_6 lo in
                                                                    let 
                                                                    (lt) =
                                                                    x_7 x in
                                                                    let 
                                                                    (x_8) =
                                                                    subs d in
                                                                    let 
                                                                    (x_9) =
                                                                    fun d ->
                                                                    fun lo ->
                                                                    fun hi ->
                                                                    let 
                                                                    (x_0) =
                                                                    sizecheck
                                                                    d in
                                                                    match x_0
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    Leaf
                                                                    | 
                                                                    false ->
                                                                    let 
                                                                    (x_1) =
                                                                    incr lo in
                                                                    let 
                                                                    (x_2) =
                                                                    x_1 < hi in
                                                                    (match x_2
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    let 
                                                                    (x_3) =
                                                                    int_range
                                                                    lo in
                                                                    let 
                                                                    (x) =
                                                                    x_3 hi in
                                                                    let 
                                                                    (w_base)
                                                                    =
                                                                    get_weight_idx
                                                                    35 in
                                                                    let 
                                                                    (w_recursive)
                                                                    =
                                                                    get_weight_idx
                                                                    34 in
                                                                    let 
                                                                    (base_case)
                                                                    =
                                                                    frequency_gen_list
                                                                    (w_base,
                                                                    (fun _ ->
                                                                    Leaf)) in
                                                                    let 
                                                                    (recursive_case)
                                                                    =
                                                                    base_case
                                                                    (w_recursive,
                                                                    (fun _ ->
                                                                    let 
                                                                    (x_5) =
                                                                    subs d in
                                                                    let 
                                                                    (x_6) =
                                                                    fun d ->
                                                                    fun lo ->
                                                                    fun hi ->
                                                                    let 
                                                                    (x_0) =
                                                                    sizecheck
                                                                    d in
                                                                    match x_0
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    Leaf
                                                                    | 
                                                                    false ->
                                                                    let 
                                                                    (x_1) =
                                                                    incr lo in
                                                                    let 
                                                                    (x_2) =
                                                                    x_1 < hi in
                                                                    (match x_2
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    let 
                                                                    (x_3) =
                                                                    int_range
                                                                    lo in
                                                                    let 
                                                                    (x) =
                                                                    x_3 hi in
                                                                    let 
                                                                    (w_base)
                                                                    =
                                                                    get_weight_idx
                                                                    33 in
                                                                    let 
                                                                    (w_recursive)
                                                                    =
                                                                    get_weight_idx
                                                                    32 in
                                                                    let 
                                                                    (base_case)
                                                                    =
                                                                    frequency_gen_list
                                                                    (w_base,
                                                                    (fun _ ->
                                                                    Leaf)) in
                                                                    let 
                                                                    (recursive_case)
                                                                    =
                                                                    base_case
                                                                    (w_recursive,
                                                                    (fun _ ->
                                                                    let 
                                                                    (x_5) =
                                                                    subs d in
                                                                    let 
                                                                    (x_6) =
                                                                    size_bst_gen
                                                                    x_5 in
                                                                    let 
                                                                    (x_7) =
                                                                    x_6 lo in
                                                                    let 
                                                                    (lt) =
                                                                    x_7 x in
                                                                    let 
                                                                    (x_8) =
                                                                    subs d in
                                                                    let 
                                                                    (x_9) =
                                                                    size_bst_gen
                                                                    x_8 in
                                                                    let 
                                                                    (x_10) =
                                                                    x_9 x in
                                                                    let 
                                                                    (rt) =
                                                                    x_10 hi in
                                                                    Node
                                                                    (x, lt,
                                                                    rt))) in
                                                                    recursive_case
                                                                    | 
                                                                    false ->
                                                                    Leaf) in
                                                                    let 
                                                                    (x_6) =
                                                                    x_6 x_5 in
                                                                    let 
                                                                    (x_7) =
                                                                    x_6 lo in
                                                                    let 
                                                                    (lt) =
                                                                    x_7 x in
                                                                    let 
                                                                    (x_8) =
                                                                    subs d in
                                                                    let 
                                                                    (x_9) =
                                                                    fun d ->
                                                                    fun lo ->
                                                                    fun hi ->
                                                                    let 
                                                                    (x_0) =
                                                                    sizecheck
                                                                    d in
                                                                    match x_0
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    Leaf
                                                                    | 
                                                                    false ->
                                                                    let 
                                                                    (x_1) =
                                                                    incr lo in
                                                                    let 
                                                                    (x_2) =
                                                                    x_1 < hi in
                                                                    (match x_2
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    let 
                                                                    (x_3) =
                                                                    int_range
                                                                    lo in
                                                                    let 
                                                                    (x) =
                                                                    x_3 hi in
                                                                    let 
                                                                    (w_base)
                                                                    =
                                                                    get_weight_idx
                                                                    31 in
                                                                    let 
                                                                    (w_recursive)
                                                                    =
                                                                    get_weight_idx
                                                                    30 in
                                                                    let 
                                                                    (base_case)
                                                                    =
                                                                    frequency_gen_list
                                                                    (w_base,
                                                                    (fun _ ->
                                                                    Leaf)) in
                                                                    let 
                                                                    (recursive_case)
                                                                    =
                                                                    base_case
                                                                    (w_recursive,
                                                                    (fun _ ->
                                                                    let 
                                                                    (x_5) =
                                                                    subs d in
                                                                    let 
                                                                    (x_6) =
                                                                    size_bst_gen
                                                                    x_5 in
                                                                    let 
                                                                    (x_7) =
                                                                    x_6 lo in
                                                                    let 
                                                                    (lt) =
                                                                    x_7 x in
                                                                    let 
                                                                    (x_8) =
                                                                    subs d in
                                                                    let 
                                                                    (x_9) =
                                                                    size_bst_gen
                                                                    x_8 in
                                                                    let 
                                                                    (x_10) =
                                                                    x_9 x in
                                                                    let 
                                                                    (rt) =
                                                                    x_10 hi in
                                                                    Node
                                                                    (x, lt,
                                                                    rt))) in
                                                                    recursive_case
                                                                    | 
                                                                    false ->
                                                                    Leaf) in
                                                                    let 
                                                                    (x_9) =
                                                                    x_9 x_8 in
                                                                    let 
                                                                    (x_10) =
                                                                    x_9 x in
                                                                    let 
                                                                    (rt) =
                                                                    x_10 hi in
                                                                    Node
                                                                    (x, lt,
                                                                    rt))) in
                                                                    recursive_case
                                                                    | 
                                                                    false ->
                                                                    Leaf) in
                                                                    let 
                                                                    (x_9) =
                                                                    x_9 x_8 in
                                                                    let 
                                                                    (x_10) =
                                                                    x_9 x in
                                                                    let 
                                                                    (rt) =
                                                                    x_10 hi in
                                                                    Node
                                                                    (x, lt,
                                                                    rt))) in
                                                                   recursive_case
                                                               | false ->
                                                                   Leaf) in
                                                  let (x_9) = x_9 x_8 in
                                                  let (x_10) = x_9 x in
                                                  let (rt) = x_10 hi in
                                                  Node (x, lt, rt))) in
                                         recursive_case
                                     | false -> Leaf) in
                        let (x_6) = x_6 x_5 in
                        let (x_7) = x_6 lo in
                        let (lt) = x_7 x in
                        let (x_8) = subs d in
                        let (x_9) =
                          fun d ->
                            fun lo ->
                              fun hi ->
                                let (x_0) = sizecheck d in
                                match x_0 with
                                | true -> Leaf
                                | false ->
                                    let (x_1) = incr lo in
                                    let (x_2) = x_1 < hi in
                                    (match x_2 with
                                     | true ->
                                         let (x_3) = int_range lo in
                                         let (x) = x_3 hi in
                                         let (w_base) = get_weight_idx 29 in
                                         let (w_recursive) =
                                           get_weight_idx 28 in
                                         let (base_case) =
                                           frequency_gen_list
                                             (w_base, (fun _ -> Leaf)) in
                                         let (recursive_case) =
                                           base_case
                                             (w_recursive,
                                               (fun _ ->
                                                  let (x_5) = subs d in
                                                  let (x_6) =
                                                    fun d ->
                                                      fun lo ->
                                                        fun hi ->
                                                          let (x_0) =
                                                            sizecheck d in
                                                          match x_0 with
                                                          | true -> Leaf
                                                          | false ->
                                                              let (x_1) =
                                                                incr lo in
                                                              let (x_2) =
                                                                x_1 < hi in
                                                              (match x_2 with
                                                               | true ->
                                                                   let 
                                                                    (x_3) =
                                                                    int_range
                                                                    lo in
                                                                   let 
                                                                    (x) =
                                                                    x_3 hi in
                                                                   let 
                                                                    (w_base)
                                                                    =
                                                                    get_weight_idx
                                                                    27 in
                                                                   let 
                                                                    (w_recursive)
                                                                    =
                                                                    get_weight_idx
                                                                    26 in
                                                                   let 
                                                                    (base_case)
                                                                    =
                                                                    frequency_gen_list
                                                                    (w_base,
                                                                    (fun _ ->
                                                                    Leaf)) in
                                                                   let 
                                                                    (recursive_case)
                                                                    =
                                                                    base_case
                                                                    (w_recursive,
                                                                    (fun _ ->
                                                                    let 
                                                                    (x_5) =
                                                                    subs d in
                                                                    let 
                                                                    (x_6) =
                                                                    fun d ->
                                                                    fun lo ->
                                                                    fun hi ->
                                                                    let 
                                                                    (x_0) =
                                                                    sizecheck
                                                                    d in
                                                                    match x_0
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    Leaf
                                                                    | 
                                                                    false ->
                                                                    let 
                                                                    (x_1) =
                                                                    incr lo in
                                                                    let 
                                                                    (x_2) =
                                                                    x_1 < hi in
                                                                    (match x_2
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    let 
                                                                    (x_3) =
                                                                    int_range
                                                                    lo in
                                                                    let 
                                                                    (x) =
                                                                    x_3 hi in
                                                                    let 
                                                                    (w_base)
                                                                    =
                                                                    get_weight_idx
                                                                    25 in
                                                                    let 
                                                                    (w_recursive)
                                                                    =
                                                                    get_weight_idx
                                                                    24 in
                                                                    let 
                                                                    (base_case)
                                                                    =
                                                                    frequency_gen_list
                                                                    (w_base,
                                                                    (fun _ ->
                                                                    Leaf)) in
                                                                    let 
                                                                    (recursive_case)
                                                                    =
                                                                    base_case
                                                                    (w_recursive,
                                                                    (fun _ ->
                                                                    let 
                                                                    (x_5) =
                                                                    subs d in
                                                                    let 
                                                                    (x_6) =
                                                                    fun d ->
                                                                    fun lo ->
                                                                    fun hi ->
                                                                    let 
                                                                    (x_0) =
                                                                    sizecheck
                                                                    d in
                                                                    match x_0
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    Leaf
                                                                    | 
                                                                    false ->
                                                                    let 
                                                                    (x_1) =
                                                                    incr lo in
                                                                    let 
                                                                    (x_2) =
                                                                    x_1 < hi in
                                                                    (match x_2
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    let 
                                                                    (x_3) =
                                                                    int_range
                                                                    lo in
                                                                    let 
                                                                    (x) =
                                                                    x_3 hi in
                                                                    let 
                                                                    (w_base)
                                                                    =
                                                                    get_weight_idx
                                                                    23 in
                                                                    let 
                                                                    (w_recursive)
                                                                    =
                                                                    get_weight_idx
                                                                    22 in
                                                                    let 
                                                                    (base_case)
                                                                    =
                                                                    frequency_gen_list
                                                                    (w_base,
                                                                    (fun _ ->
                                                                    Leaf)) in
                                                                    let 
                                                                    (recursive_case)
                                                                    =
                                                                    base_case
                                                                    (w_recursive,
                                                                    (fun _ ->
                                                                    let 
                                                                    (x_5) =
                                                                    subs d in
                                                                    let 
                                                                    (x_6) =
                                                                    size_bst_gen
                                                                    x_5 in
                                                                    let 
                                                                    (x_7) =
                                                                    x_6 lo in
                                                                    let 
                                                                    (lt) =
                                                                    x_7 x in
                                                                    let 
                                                                    (x_8) =
                                                                    subs d in
                                                                    let 
                                                                    (x_9) =
                                                                    size_bst_gen
                                                                    x_8 in
                                                                    let 
                                                                    (x_10) =
                                                                    x_9 x in
                                                                    let 
                                                                    (rt) =
                                                                    x_10 hi in
                                                                    Node
                                                                    (x, lt,
                                                                    rt))) in
                                                                    recursive_case
                                                                    | 
                                                                    false ->
                                                                    Leaf) in
                                                                    let 
                                                                    (x_6) =
                                                                    x_6 x_5 in
                                                                    let 
                                                                    (x_7) =
                                                                    x_6 lo in
                                                                    let 
                                                                    (lt) =
                                                                    x_7 x in
                                                                    let 
                                                                    (x_8) =
                                                                    subs d in
                                                                    let 
                                                                    (x_9) =
                                                                    fun d ->
                                                                    fun lo ->
                                                                    fun hi ->
                                                                    let 
                                                                    (x_0) =
                                                                    sizecheck
                                                                    d in
                                                                    match x_0
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    Leaf
                                                                    | 
                                                                    false ->
                                                                    let 
                                                                    (x_1) =
                                                                    incr lo in
                                                                    let 
                                                                    (x_2) =
                                                                    x_1 < hi in
                                                                    (match x_2
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    let 
                                                                    (x_3) =
                                                                    int_range
                                                                    lo in
                                                                    let 
                                                                    (x) =
                                                                    x_3 hi in
                                                                    let 
                                                                    (w_base)
                                                                    =
                                                                    get_weight_idx
                                                                    21 in
                                                                    let 
                                                                    (w_recursive)
                                                                    =
                                                                    get_weight_idx
                                                                    20 in
                                                                    let 
                                                                    (base_case)
                                                                    =
                                                                    frequency_gen_list
                                                                    (w_base,
                                                                    (fun _ ->
                                                                    Leaf)) in
                                                                    let 
                                                                    (recursive_case)
                                                                    =
                                                                    base_case
                                                                    (w_recursive,
                                                                    (fun _ ->
                                                                    let 
                                                                    (x_5) =
                                                                    subs d in
                                                                    let 
                                                                    (x_6) =
                                                                    size_bst_gen
                                                                    x_5 in
                                                                    let 
                                                                    (x_7) =
                                                                    x_6 lo in
                                                                    let 
                                                                    (lt) =
                                                                    x_7 x in
                                                                    let 
                                                                    (x_8) =
                                                                    subs d in
                                                                    let 
                                                                    (x_9) =
                                                                    size_bst_gen
                                                                    x_8 in
                                                                    let 
                                                                    (x_10) =
                                                                    x_9 x in
                                                                    let 
                                                                    (rt) =
                                                                    x_10 hi in
                                                                    Node
                                                                    (x, lt,
                                                                    rt))) in
                                                                    recursive_case
                                                                    | 
                                                                    false ->
                                                                    Leaf) in
                                                                    let 
                                                                    (x_9) =
                                                                    x_9 x_8 in
                                                                    let 
                                                                    (x_10) =
                                                                    x_9 x in
                                                                    let 
                                                                    (rt) =
                                                                    x_10 hi in
                                                                    Node
                                                                    (x, lt,
                                                                    rt))) in
                                                                    recursive_case
                                                                    | 
                                                                    false ->
                                                                    Leaf) in
                                                                    let 
                                                                    (x_6) =
                                                                    x_6 x_5 in
                                                                    let 
                                                                    (x_7) =
                                                                    x_6 lo in
                                                                    let 
                                                                    (lt) =
                                                                    x_7 x in
                                                                    let 
                                                                    (x_8) =
                                                                    subs d in
                                                                    let 
                                                                    (x_9) =
                                                                    fun d ->
                                                                    fun lo ->
                                                                    fun hi ->
                                                                    let 
                                                                    (x_0) =
                                                                    sizecheck
                                                                    d in
                                                                    match x_0
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    Leaf
                                                                    | 
                                                                    false ->
                                                                    let 
                                                                    (x_1) =
                                                                    incr lo in
                                                                    let 
                                                                    (x_2) =
                                                                    x_1 < hi in
                                                                    (match x_2
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    let 
                                                                    (x_3) =
                                                                    int_range
                                                                    lo in
                                                                    let 
                                                                    (x) =
                                                                    x_3 hi in
                                                                    let 
                                                                    (w_base)
                                                                    =
                                                                    get_weight_idx
                                                                    19 in
                                                                    let 
                                                                    (w_recursive)
                                                                    =
                                                                    get_weight_idx
                                                                    18 in
                                                                    let 
                                                                    (base_case)
                                                                    =
                                                                    frequency_gen_list
                                                                    (w_base,
                                                                    (fun _ ->
                                                                    Leaf)) in
                                                                    let 
                                                                    (recursive_case)
                                                                    =
                                                                    base_case
                                                                    (w_recursive,
                                                                    (fun _ ->
                                                                    let 
                                                                    (x_5) =
                                                                    subs d in
                                                                    let 
                                                                    (x_6) =
                                                                    fun d ->
                                                                    fun lo ->
                                                                    fun hi ->
                                                                    let 
                                                                    (x_0) =
                                                                    sizecheck
                                                                    d in
                                                                    match x_0
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    Leaf
                                                                    | 
                                                                    false ->
                                                                    let 
                                                                    (x_1) =
                                                                    incr lo in
                                                                    let 
                                                                    (x_2) =
                                                                    x_1 < hi in
                                                                    (match x_2
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    let 
                                                                    (x_3) =
                                                                    int_range
                                                                    lo in
                                                                    let 
                                                                    (x) =
                                                                    x_3 hi in
                                                                    let 
                                                                    (w_base)
                                                                    =
                                                                    get_weight_idx
                                                                    17 in
                                                                    let 
                                                                    (w_recursive)
                                                                    =
                                                                    get_weight_idx
                                                                    16 in
                                                                    let 
                                                                    (base_case)
                                                                    =
                                                                    frequency_gen_list
                                                                    (w_base,
                                                                    (fun _ ->
                                                                    Leaf)) in
                                                                    let 
                                                                    (recursive_case)
                                                                    =
                                                                    base_case
                                                                    (w_recursive,
                                                                    (fun _ ->
                                                                    let 
                                                                    (x_5) =
                                                                    subs d in
                                                                    let 
                                                                    (x_6) =
                                                                    size_bst_gen
                                                                    x_5 in
                                                                    let 
                                                                    (x_7) =
                                                                    x_6 lo in
                                                                    let 
                                                                    (lt) =
                                                                    x_7 x in
                                                                    let 
                                                                    (x_8) =
                                                                    subs d in
                                                                    let 
                                                                    (x_9) =
                                                                    size_bst_gen
                                                                    x_8 in
                                                                    let 
                                                                    (x_10) =
                                                                    x_9 x in
                                                                    let 
                                                                    (rt) =
                                                                    x_10 hi in
                                                                    Node
                                                                    (x, lt,
                                                                    rt))) in
                                                                    recursive_case
                                                                    | 
                                                                    false ->
                                                                    Leaf) in
                                                                    let 
                                                                    (x_6) =
                                                                    x_6 x_5 in
                                                                    let 
                                                                    (x_7) =
                                                                    x_6 lo in
                                                                    let 
                                                                    (lt) =
                                                                    x_7 x in
                                                                    let 
                                                                    (x_8) =
                                                                    subs d in
                                                                    let 
                                                                    (x_9) =
                                                                    fun d ->
                                                                    fun lo ->
                                                                    fun hi ->
                                                                    let 
                                                                    (x_0) =
                                                                    sizecheck
                                                                    d in
                                                                    match x_0
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    Leaf
                                                                    | 
                                                                    false ->
                                                                    let 
                                                                    (x_1) =
                                                                    incr lo in
                                                                    let 
                                                                    (x_2) =
                                                                    x_1 < hi in
                                                                    (match x_2
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    let 
                                                                    (x_3) =
                                                                    int_range
                                                                    lo in
                                                                    let 
                                                                    (x) =
                                                                    x_3 hi in
                                                                    let 
                                                                    (w_base)
                                                                    =
                                                                    get_weight_idx
                                                                    15 in
                                                                    let 
                                                                    (w_recursive)
                                                                    =
                                                                    get_weight_idx
                                                                    14 in
                                                                    let 
                                                                    (base_case)
                                                                    =
                                                                    frequency_gen_list
                                                                    (w_base,
                                                                    (fun _ ->
                                                                    Leaf)) in
                                                                    let 
                                                                    (recursive_case)
                                                                    =
                                                                    base_case
                                                                    (w_recursive,
                                                                    (fun _ ->
                                                                    let 
                                                                    (x_5) =
                                                                    subs d in
                                                                    let 
                                                                    (x_6) =
                                                                    size_bst_gen
                                                                    x_5 in
                                                                    let 
                                                                    (x_7) =
                                                                    x_6 lo in
                                                                    let 
                                                                    (lt) =
                                                                    x_7 x in
                                                                    let 
                                                                    (x_8) =
                                                                    subs d in
                                                                    let 
                                                                    (x_9) =
                                                                    size_bst_gen
                                                                    x_8 in
                                                                    let 
                                                                    (x_10) =
                                                                    x_9 x in
                                                                    let 
                                                                    (rt) =
                                                                    x_10 hi in
                                                                    Node
                                                                    (x, lt,
                                                                    rt))) in
                                                                    recursive_case
                                                                    | 
                                                                    false ->
                                                                    Leaf) in
                                                                    let 
                                                                    (x_9) =
                                                                    x_9 x_8 in
                                                                    let 
                                                                    (x_10) =
                                                                    x_9 x in
                                                                    let 
                                                                    (rt) =
                                                                    x_10 hi in
                                                                    Node
                                                                    (x, lt,
                                                                    rt))) in
                                                                    recursive_case
                                                                    | 
                                                                    false ->
                                                                    Leaf) in
                                                                    let 
                                                                    (x_9) =
                                                                    x_9 x_8 in
                                                                    let 
                                                                    (x_10) =
                                                                    x_9 x in
                                                                    let 
                                                                    (rt) =
                                                                    x_10 hi in
                                                                    Node
                                                                    (x, lt,
                                                                    rt))) in
                                                                   recursive_case
                                                               | false ->
                                                                   Leaf) in
                                                  let (x_6) = x_6 x_5 in
                                                  let (x_7) = x_6 lo in
                                                  let (lt) = x_7 x in
                                                  let (x_8) = subs d in
                                                  let (x_9) =
                                                    fun d ->
                                                      fun lo ->
                                                        fun hi ->
                                                          let (x_0) =
                                                            sizecheck d in
                                                          match x_0 with
                                                          | true -> Leaf
                                                          | false ->
                                                              let (x_1) =
                                                                incr lo in
                                                              let (x_2) =
                                                                x_1 < hi in
                                                              (match x_2 with
                                                               | true ->
                                                                   let 
                                                                    (x_3) =
                                                                    int_range
                                                                    lo in
                                                                   let 
                                                                    (x) =
                                                                    x_3 hi in
                                                                   let 
                                                                    (w_base)
                                                                    =
                                                                    get_weight_idx
                                                                    13 in
                                                                   let 
                                                                    (w_recursive)
                                                                    =
                                                                    get_weight_idx
                                                                    12 in
                                                                   let 
                                                                    (base_case)
                                                                    =
                                                                    frequency_gen_list
                                                                    (w_base,
                                                                    (fun _ ->
                                                                    Leaf)) in
                                                                   let 
                                                                    (recursive_case)
                                                                    =
                                                                    base_case
                                                                    (w_recursive,
                                                                    (fun _ ->
                                                                    let 
                                                                    (x_5) =
                                                                    subs d in
                                                                    let 
                                                                    (x_6) =
                                                                    fun d ->
                                                                    fun lo ->
                                                                    fun hi ->
                                                                    let 
                                                                    (x_0) =
                                                                    sizecheck
                                                                    d in
                                                                    match x_0
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    Leaf
                                                                    | 
                                                                    false ->
                                                                    let 
                                                                    (x_1) =
                                                                    incr lo in
                                                                    let 
                                                                    (x_2) =
                                                                    x_1 < hi in
                                                                    (match x_2
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    let 
                                                                    (x_3) =
                                                                    int_range
                                                                    lo in
                                                                    let 
                                                                    (x) =
                                                                    x_3 hi in
                                                                    let 
                                                                    (w_base)
                                                                    =
                                                                    get_weight_idx
                                                                    11 in
                                                                    let 
                                                                    (w_recursive)
                                                                    =
                                                                    get_weight_idx
                                                                    10 in
                                                                    let 
                                                                    (base_case)
                                                                    =
                                                                    frequency_gen_list
                                                                    (w_base,
                                                                    (fun _ ->
                                                                    Leaf)) in
                                                                    let 
                                                                    (recursive_case)
                                                                    =
                                                                    base_case
                                                                    (w_recursive,
                                                                    (fun _ ->
                                                                    let 
                                                                    (x_5) =
                                                                    subs d in
                                                                    let 
                                                                    (x_6) =
                                                                    fun d ->
                                                                    fun lo ->
                                                                    fun hi ->
                                                                    let 
                                                                    (x_0) =
                                                                    sizecheck
                                                                    d in
                                                                    match x_0
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    Leaf
                                                                    | 
                                                                    false ->
                                                                    let 
                                                                    (x_1) =
                                                                    incr lo in
                                                                    let 
                                                                    (x_2) =
                                                                    x_1 < hi in
                                                                    (match x_2
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    let 
                                                                    (x_3) =
                                                                    int_range
                                                                    lo in
                                                                    let 
                                                                    (x) =
                                                                    x_3 hi in
                                                                    let 
                                                                    (w_base)
                                                                    =
                                                                    get_weight_idx
                                                                    9 in
                                                                    let 
                                                                    (w_recursive)
                                                                    =
                                                                    get_weight_idx
                                                                    8 in
                                                                    let 
                                                                    (base_case)
                                                                    =
                                                                    frequency_gen_list
                                                                    (w_base,
                                                                    (fun _ ->
                                                                    Leaf)) in
                                                                    let 
                                                                    (recursive_case)
                                                                    =
                                                                    base_case
                                                                    (w_recursive,
                                                                    (fun _ ->
                                                                    let 
                                                                    (x_5) =
                                                                    subs d in
                                                                    let 
                                                                    (x_6) =
                                                                    size_bst_gen
                                                                    x_5 in
                                                                    let 
                                                                    (x_7) =
                                                                    x_6 lo in
                                                                    let 
                                                                    (lt) =
                                                                    x_7 x in
                                                                    let 
                                                                    (x_8) =
                                                                    subs d in
                                                                    let 
                                                                    (x_9) =
                                                                    size_bst_gen
                                                                    x_8 in
                                                                    let 
                                                                    (x_10) =
                                                                    x_9 x in
                                                                    let 
                                                                    (rt) =
                                                                    x_10 hi in
                                                                    Node
                                                                    (x, lt,
                                                                    rt))) in
                                                                    recursive_case
                                                                    | 
                                                                    false ->
                                                                    Leaf) in
                                                                    let 
                                                                    (x_6) =
                                                                    x_6 x_5 in
                                                                    let 
                                                                    (x_7) =
                                                                    x_6 lo in
                                                                    let 
                                                                    (lt) =
                                                                    x_7 x in
                                                                    let 
                                                                    (x_8) =
                                                                    subs d in
                                                                    let 
                                                                    (x_9) =
                                                                    fun d ->
                                                                    fun lo ->
                                                                    fun hi ->
                                                                    let 
                                                                    (x_0) =
                                                                    sizecheck
                                                                    d in
                                                                    match x_0
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    Leaf
                                                                    | 
                                                                    false ->
                                                                    let 
                                                                    (x_1) =
                                                                    incr lo in
                                                                    let 
                                                                    (x_2) =
                                                                    x_1 < hi in
                                                                    (match x_2
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    let 
                                                                    (x_3) =
                                                                    int_range
                                                                    lo in
                                                                    let 
                                                                    (x) =
                                                                    x_3 hi in
                                                                    let 
                                                                    (w_base)
                                                                    =
                                                                    get_weight_idx
                                                                    7 in
                                                                    let 
                                                                    (w_recursive)
                                                                    =
                                                                    get_weight_idx
                                                                    6 in
                                                                    let 
                                                                    (base_case)
                                                                    =
                                                                    frequency_gen_list
                                                                    (w_base,
                                                                    (fun _ ->
                                                                    Leaf)) in
                                                                    let 
                                                                    (recursive_case)
                                                                    =
                                                                    base_case
                                                                    (w_recursive,
                                                                    (fun _ ->
                                                                    let 
                                                                    (x_5) =
                                                                    subs d in
                                                                    let 
                                                                    (x_6) =
                                                                    size_bst_gen
                                                                    x_5 in
                                                                    let 
                                                                    (x_7) =
                                                                    x_6 lo in
                                                                    let 
                                                                    (lt) =
                                                                    x_7 x in
                                                                    let 
                                                                    (x_8) =
                                                                    subs d in
                                                                    let 
                                                                    (x_9) =
                                                                    size_bst_gen
                                                                    x_8 in
                                                                    let 
                                                                    (x_10) =
                                                                    x_9 x in
                                                                    let 
                                                                    (rt) =
                                                                    x_10 hi in
                                                                    Node
                                                                    (x, lt,
                                                                    rt))) in
                                                                    recursive_case
                                                                    | 
                                                                    false ->
                                                                    Leaf) in
                                                                    let 
                                                                    (x_9) =
                                                                    x_9 x_8 in
                                                                    let 
                                                                    (x_10) =
                                                                    x_9 x in
                                                                    let 
                                                                    (rt) =
                                                                    x_10 hi in
                                                                    Node
                                                                    (x, lt,
                                                                    rt))) in
                                                                    recursive_case
                                                                    | 
                                                                    false ->
                                                                    Leaf) in
                                                                    let 
                                                                    (x_6) =
                                                                    x_6 x_5 in
                                                                    let 
                                                                    (x_7) =
                                                                    x_6 lo in
                                                                    let 
                                                                    (lt) =
                                                                    x_7 x in
                                                                    let 
                                                                    (x_8) =
                                                                    subs d in
                                                                    let 
                                                                    (x_9) =
                                                                    fun d ->
                                                                    fun lo ->
                                                                    fun hi ->
                                                                    let 
                                                                    (x_0) =
                                                                    sizecheck
                                                                    d in
                                                                    match x_0
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    Leaf
                                                                    | 
                                                                    false ->
                                                                    let 
                                                                    (x_1) =
                                                                    incr lo in
                                                                    let 
                                                                    (x_2) =
                                                                    x_1 < hi in
                                                                    (match x_2
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    let 
                                                                    (x_3) =
                                                                    int_range
                                                                    lo in
                                                                    let 
                                                                    (x) =
                                                                    x_3 hi in
                                                                    let 
                                                                    (w_base)
                                                                    =
                                                                    get_weight_idx
                                                                    5 in
                                                                    let 
                                                                    (w_recursive)
                                                                    =
                                                                    get_weight_idx
                                                                    4 in
                                                                    let 
                                                                    (base_case)
                                                                    =
                                                                    frequency_gen_list
                                                                    (w_base,
                                                                    (fun _ ->
                                                                    Leaf)) in
                                                                    let 
                                                                    (recursive_case)
                                                                    =
                                                                    base_case
                                                                    (w_recursive,
                                                                    (fun _ ->
                                                                    let 
                                                                    (x_5) =
                                                                    subs d in
                                                                    let 
                                                                    (x_6) =
                                                                    fun d ->
                                                                    fun lo ->
                                                                    fun hi ->
                                                                    let 
                                                                    (x_0) =
                                                                    sizecheck
                                                                    d in
                                                                    match x_0
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    Leaf
                                                                    | 
                                                                    false ->
                                                                    let 
                                                                    (x_1) =
                                                                    incr lo in
                                                                    let 
                                                                    (x_2) =
                                                                    x_1 < hi in
                                                                    (match x_2
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    let 
                                                                    (x_3) =
                                                                    int_range
                                                                    lo in
                                                                    let 
                                                                    (x) =
                                                                    x_3 hi in
                                                                    let 
                                                                    (w_base)
                                                                    =
                                                                    get_weight_idx
                                                                    3 in
                                                                    let 
                                                                    (w_recursive)
                                                                    =
                                                                    get_weight_idx
                                                                    2 in
                                                                    let 
                                                                    (base_case)
                                                                    =
                                                                    frequency_gen_list
                                                                    (w_base,
                                                                    (fun _ ->
                                                                    Leaf)) in
                                                                    let 
                                                                    (recursive_case)
                                                                    =
                                                                    base_case
                                                                    (w_recursive,
                                                                    (fun _ ->
                                                                    let 
                                                                    (x_5) =
                                                                    subs d in
                                                                    let 
                                                                    (x_6) =
                                                                    size_bst_gen
                                                                    x_5 in
                                                                    let 
                                                                    (x_7) =
                                                                    x_6 lo in
                                                                    let 
                                                                    (lt) =
                                                                    x_7 x in
                                                                    let 
                                                                    (x_8) =
                                                                    subs d in
                                                                    let 
                                                                    (x_9) =
                                                                    size_bst_gen
                                                                    x_8 in
                                                                    let 
                                                                    (x_10) =
                                                                    x_9 x in
                                                                    let 
                                                                    (rt) =
                                                                    x_10 hi in
                                                                    Node
                                                                    (x, lt,
                                                                    rt))) in
                                                                    recursive_case
                                                                    | 
                                                                    false ->
                                                                    Leaf) in
                                                                    let 
                                                                    (x_6) =
                                                                    x_6 x_5 in
                                                                    let 
                                                                    (x_7) =
                                                                    x_6 lo in
                                                                    let 
                                                                    (lt) =
                                                                    x_7 x in
                                                                    let 
                                                                    (x_8) =
                                                                    subs d in
                                                                    let 
                                                                    (x_9) =
                                                                    fun d ->
                                                                    fun lo ->
                                                                    fun hi ->
                                                                    let 
                                                                    (x_0) =
                                                                    sizecheck
                                                                    d in
                                                                    match x_0
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    Leaf
                                                                    | 
                                                                    false ->
                                                                    let 
                                                                    (x_1) =
                                                                    incr lo in
                                                                    let 
                                                                    (x_2) =
                                                                    x_1 < hi in
                                                                    (match x_2
                                                                    with
                                                                    | 
                                                                    true ->
                                                                    let 
                                                                    (x_3) =
                                                                    int_range
                                                                    lo in
                                                                    let 
                                                                    (x) =
                                                                    x_3 hi in
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
                                                                    Leaf)) in
                                                                    let 
                                                                    (recursive_case)
                                                                    =
                                                                    base_case
                                                                    (w_recursive,
                                                                    (fun _ ->
                                                                    let 
                                                                    (x_5) =
                                                                    subs d in
                                                                    let 
                                                                    (x_6) =
                                                                    size_bst_gen
                                                                    x_5 in
                                                                    let 
                                                                    (x_7) =
                                                                    x_6 lo in
                                                                    let 
                                                                    (lt) =
                                                                    x_7 x in
                                                                    let 
                                                                    (x_8) =
                                                                    subs d in
                                                                    let 
                                                                    (x_9) =
                                                                    size_bst_gen
                                                                    x_8 in
                                                                    let 
                                                                    (x_10) =
                                                                    x_9 x in
                                                                    let 
                                                                    (rt) =
                                                                    x_10 hi in
                                                                    Node
                                                                    (x, lt,
                                                                    rt))) in
                                                                    recursive_case
                                                                    | 
                                                                    false ->
                                                                    Leaf) in
                                                                    let 
                                                                    (x_9) =
                                                                    x_9 x_8 in
                                                                    let 
                                                                    (x_10) =
                                                                    x_9 x in
                                                                    let 
                                                                    (rt) =
                                                                    x_10 hi in
                                                                    Node
                                                                    (x, lt,
                                                                    rt))) in
                                                                    recursive_case
                                                                    | 
                                                                    false ->
                                                                    Leaf) in
                                                                    let 
                                                                    (x_9) =
                                                                    x_9 x_8 in
                                                                    let 
                                                                    (x_10) =
                                                                    x_9 x in
                                                                    let 
                                                                    (rt) =
                                                                    x_10 hi in
                                                                    Node
                                                                    (x, lt,
                                                                    rt))) in
                                                                   recursive_case
                                                               | false ->
                                                                   Leaf) in
                                                  let (x_9) = x_9 x_8 in
                                                  let (x_10) = x_9 x in
                                                  let (rt) = x_10 hi in
                                                  Node (x, lt, rt))) in
                                         recursive_case
                                     | false -> Leaf) in
                        let (x_9) = x_9 x_8 in
                        let (x_10) = x_9 x in
                        let (rt) = x_10 hi in Node (x, lt, rt))) in
               recursive_case
           | false -> Leaf)