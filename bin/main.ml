open Search
open Dumb_enumerate
open Greedy_enumerate
open Frequency_combinators
open Feature_vectors

let usage_msg =
  "Usage: dune exec Cobb_Frequency <data_type> [-i] [-r] [-one] [-s]"

let set_data_type d = data_type := d

let speclist =
  [
    ("-i", Arg.Int (fun s -> iterations := s), "Set iterations");
    ("-r", Arg.Int (fun s -> n_reset := s), "Set random restarts");
    ( "-f",
      Arg.String (fun s -> feature_vector := s),
      "run with specified feature vector" );
    ("-one", Arg.Set print_one, "print one in one file");
    ( "-s",
      Arg.String (fun s -> search_strat_str := s),
      "Set search strategy: [e|er|ers|erw|ge|sa]" );
  ]

let pp_res fmt
    ( gen_name,
      fv_name,
      goal,
      iterations,
      n_reset,
      init_dist,
      init_time,
      init_weights,
      fin_dist,
      fin_time,
      fin_weights ) =
  let open Format in
  let aux version dist time weights =
    fprintf fmt "%-16s" version;
    let _ =
      match dist with
      | dist', [] ->
          fprintf fmt "%-10.3f %27s %-7s %-10.3f %s" dist' "-" " " time "("
      | dist', chi_pieces ->
          fprintf fmt "(";
          List.iteri
            (fun i x ->
              if i > 0 then fprintf fmt ", ";
              fprintf fmt "%.3f" x)
            chi_pieces;
          fprintf fmt ") %-5s " " ";
          fprintf fmt "%-10.3f %-10.3f %s" dist' time "("
    in

    Array.iteri
      (fun i x ->
        if i > 0 then fprintf fmt ", ";
        fprintf fmt "%d" x)
      weights;
    fprintf fmt ")\n"
  in

  fprintf fmt "\nTest: %s - %s \n" gen_name fv_name;
  let _ =
    match goal with
    | g :: [] -> fprintf fmt "Goal: %.3f" g
    | [] -> failwith "printing error"
    | _ ->
        fprintf fmt "Goal: ";
        List.iteri
          (fun i x ->
            if i > 0 then fprintf fmt ", ";
            fprintf fmt "%.3f" x)
          goal
  in

  fprintf fmt "\n\nRan %d iterations & %d restarts\n\n" iterations n_reset;
  fprintf fmt "%16s %-35s %-10s %-10s %-10s\n" "" "dist" "chi" "time" "weights";
  fprintf fmt "%s\n" (String.make 100 '-');

  aux "Initial" init_dist init_time init_weights;
  aux "Final" fin_dist fin_time fin_weights

let print_csv oc
    ( gen_name,
      fv_name,
      goal,
      iterations,
      n_reset,
      init_dist,
      init_time,
      init_weights,
      init_score,
      fin_dist,
      fin_time,
      fin_weights,
      fin_score ) =
  let aux version dist time weights score =
    Printf.fprintf oc "%s,%s,%s," version gen_name fv_name;
    let _ =
      match goal with
      | g :: [] -> Printf.fprintf oc "%.3f," g
      | [] -> failwith "printing error"
      | _ ->
          Printf.fprintf oc "\"(";
          List.iteri
            (fun i x ->
              if i > 0 then Printf.fprintf oc ", ";
              Printf.fprintf oc "%.3f" x)
            goal;
          Printf.fprintf oc ")\","
    in

    let _ =
      match dist with
      | dist', [] -> Printf.fprintf oc "%.3f,\"(" dist'
      | dist', chi_pieces ->
          Printf.fprintf oc "\"(";
          List.iteri
            (fun i x ->
              if i > 0 then Printf.fprintf oc ", ";
              Printf.fprintf oc "%.3f" x)
            chi_pieces;
          Printf.fprintf oc ")\",\"("
    in

    Array.iteri
      (fun i x ->
        if i > 0 then Printf.fprintf oc ", ";
        Printf.fprintf oc "%d" x)
      weights;

    let _ =
      match dist with
      | dist', [] -> Printf.fprintf oc ")\",,"
      | dist', chi_pieces -> Printf.fprintf oc ")\", %.3f," dist'
    in
    Printf.fprintf oc "%.3f," score;
    Printf.fprintf oc "%.3f,%d,%d\n" time iterations n_reset
  in

  Printf.fprintf oc
    "version,generator,fv,goal,dist,weights,chi,score,time,iterations,restarts\n";
  aux "initial" init_dist init_time init_weights init_score;
  aux "final" fin_dist fin_time fin_weights fin_score;
  ()

let evaluate gen
    (fv : string * (float list -> 'a list -> (float * float list) * float))
    (goal_list : float list) =
  let gen_name, g, n_weights, n_bool, n_nat = gen in
  let fv_name, f = fv in

  (* let gen_name = "dumb_enumerate_ratios/" ^ gen_name in *)
  total_iterations := 0;
  total_restarts := 0;

  let gen_name =
    (match !search_strat_str with
    | "sa" -> ""
    | "e" -> "dumb_enumerate_weights/"
    | "er" -> "dumb_enumerate_ratios/"
    | "ers" -> "dumb_enumerate_ratios_smaller/"
    | "erw" -> "dumb_enumerate_ratios_w2/"
    | "ge" -> "greedy_enumerate/"
    | "t" -> "test/"
    | _ -> failwith "invalid search stragtegy/")
    ^ gen_name
  in

  let file_path =
    Printf.sprintf "results/%s/%s_%s%d_%d.csv" gen_name fv_name
      (List.fold_left (fun acc x -> acc ^ string_of_float x ^ "_") "" goal_list)
      !iterations !n_reset
  in

  (* Printf.printf "results/%s/%s_%s%d_%d.csv" gen_name fv_name
     (List.fold_left (fun acc x -> acc ^ string_of_float x ^ "_") "" goal_list)
     !iterations !n_reset; *)
  let result_oc =
    if !print_one then top_oc else (* results_oc *)
                                open_out file_path
  in

  weights := Array.init n_weights (fun _ -> 500);

  (* run initial *)
  let start_time = Unix.gettimeofday () in
  let results = collect sample_size g in
  let end_time = Unix.gettimeofday () in

  let init_dist, init_score = f goal_list results in
  let init_weights = !weights in

  (* run with adjustment *)
  let use_neg_w =
    List.mem gen_name
      [
        "parametrized/sized_list_1";
        "parametrized/sized_list_2";
        "parametrized/depthtree_2";
        "parametrized/depthbst_2";
        "parametrized/rbtree_2";
      ]
  in
  let oc = open_out "bin/iterations.csv" in

  let fin_weights, fin_score, fin_dist, fin_time =
    match !search_strat_str with
    | "sa" ->
        random_restart oc g f goal_list !iterations use_neg_w
          simulated_annealing
    | "e" -> dumb_enumerate oc g f goal_list true
    | "er" | "ers" | "erw" -> dumb_enumerate_ratios oc g f goal_list true
    | "ge" -> greedy_enumerate oc g f goal_list true
    | "t" -> test oc g f goal_list true
    | _ -> failwith "invalid search stragtegy"
  in

  close_out oc;

  (* let fin_weights = !weights in *)
  pp_res Format.std_formatter
    ( gen_name,
      fv_name,
      goal_list,
      !total_iterations,
      !total_restarts,
      init_dist,
      end_time -. start_time,
      init_weights,
      fin_dist,
      fin_time,
      fin_weights );

  print_csv result_oc
    ( gen_name,
      fv_name,
      goal_list,
      !total_iterations,
      !total_restarts,
      init_dist,
      end_time -. start_time,
      init_weights,
      init_score,
      fin_dist,
      fin_time,
      fin_weights,
      fin_score );
  ()

(* generators *)
let sortedlist_gen = ("frequency/sorted_list", sortedlist, 4, 0, 0)
let uniquelist_gen = ("frequency/unique_list", uniquelist, 0, 0, 0)
let sizedlist_gen = ("frequency/sized_list", sizedlist, 2, 1, 0)
let evenlist_gen = ("frequency/even_list", evenlist, 2, 1, 0)
let rbtree_gen = ("frequency/rb_tree", rbtree, 4, 2, 0)
let depthtree_gen = ("frequency/depth_tree", depthtree, 2, 1, 0)
let depthbst_gen = ("frequency/depth_bst", depthbst, 2, 1, 0)
let dragen_gen = ("Dragen", dragen_tree, 6, 2, 0)
let ld_rbtree_gen = ("LoadedDice", ld_rbtree, 5 * 8, 0, 0)

let sizedlist_para_enum_gen_5 =
  ("parametrized_enumeration/sized_list_5", sizedlist_para_enum, 12, 1, 0)

let sizedlist_para_enum_gen_10 =
  ("parametrized_enumeration/sized_list_10", sizedlist_para_enum, 22, 1, 0)

let sizedlist_para_1_gen =
  ("parametrized/sized_list_1_const", sizedlist_para_1, 3, 1, 0)

let sizedlist_para_2_gen = ("parametrized/sized_list", sizedlist_para_2, 4, 1, 0)
let evenlist_para_2_gen = ("parametrized/even_list", evenlist_para_2, 4, 1, 0)
let depthtree_para_2_gen = ("parametrized/depth_tree", depthtree_para_2, 4, 1, 0)
let depthbst_para_2_gen = ("parametrized/depth_bst", depthbst_para_2, 4, 1, 0)
let rbtree_para_2_gen = ("parametrized/rb_tree", rbtree_para_2, 8, 2, 0)
let ur_depthtree_gen = ("unrolled/depth_tree", depthtree_ur, 6, 1, 0)
let ur_depthbst_gen = ("unrolled/depth_bst", depthbst_ur, 6, 1, 0)
let ur_rbtree_gen = ("unrolled/rb_tree", rbtree_ur, 20, 2, 0)
let ur_sizedlist_gen = ("unrolled/sized_list", sizedlist_ur, 4, 1, 0)
let ur_evenlist_gen = ("unrolled/even_list", evenlist_ur, 4, 1, 0)
let ur5_depthtree_gen = ("unrolled/depth_tree_5", depthtree_5_ur, 62, 1, 0)
let ur5_depthbst_gen = ("unrolled/depth_bst_5", depthbst_5_ur, 62, 1, 0)
let ur5_sizedlist_gen = ("unrolled/sized_list_5", sizedlist_5_ur, 10, 1, 0)
let ur10_sizedlist_gen = ("unrolled/sized_list_10", sizedlist_10_ur, 20, 1, 0)

let ur_lin_depthtree_gen =
  ("unrolled_linear/depth_tree", depthtree_ur_lin, 12, 1, 0)

let ur_lin_depthbst_gen =
  ("unrolled_linear/depth_bst", depthbst_ur_lin, 12, 1, 0)

(* feature vectors *)
let nil_list_fv = ("nil", get_exact_score nil_fv)
let min_nil_list_fv = ("min_nil", get_score nil_fv)
let len_list_fv = ("len", get_exact_score len_fv)
let min_len_list_fv = ("min_len", get_score len_fv)
let nil_list_fv_2 = ("nil2", get_exact_score2 nil_fv)
let min_nil_list_fv_2 = ("min_nil2", get_score2 nil_fv)
let len_list_fv_2 = ("len2", get_exact_score2 len_fv)
let min_len_list_fv_2 = ("min_len2", get_score2 len_fv)
let bail_list_fv = ("bailouts", get_score bailout_fv)
let b_rbtree_fv = ("black", get_exact_score b_fv)
let min_b_rbtree_fv = ("min_black", get_score b_fv)
let height_tree_fv = ("height", get_exact_score height_fv)
let min_height_tree_fv = ("min_height", get_score height_fv)
let stick_tree_fv = ("stick", get_exact_score stick_fv)
let min_stick_tree_fv = ("min_stick", get_score stick_fv)
let h_balanced_tree_fv = ("h_bal", get_exact_score h_balanced_fv)
let count_cons = ("constructors", get_chi_score count_constr_list)
let uni_len_list_fv = ("uni_len", get_uniform_score length_acc)
let uni_height_rbtree_fv = ("uni_height", get_uniform_score height_rbt_acc)
let uni_height_tree_fv = ("uni_height", get_uniform_score height_tree_acc)

(* (fv, goal) *)
let (sizedlist_tests :
      ((string * (float list -> 'a list list -> (float * float list) * float))
      * float list)
      list) =
  [
    (* (nil_list_fv, [ 0.1 ]); *)
    (* (min_nil_list_fv, [ 0.1 ]); *)
    (* (len_list_fv, [ 5. ]); *)
    (* (min_len_list_fv, [ 5. ]); *)
    (uni_len_list_fv, [ 0.; 1.; 2.; 3.; 4.; 5.; 6.; 7.; 8.; 9.; 10. ]);
  ]

let sizedlist_tests_2 =
  [
    (nil_list_fv_2, [ 0.1 ]);
    (* (min_nil_list_fv_2, [ 0.1 ]); *)
    (len_list_fv_2, [ 5. ]);
    (* (min_len_list_fv_2, [ 5. ]); *)
  ]

let sortedlist_tests = [ (bail_list_fv, [ 0.01 ]) ]

let evenlist_tests =
  [
    (nil_list_fv, [ 0.1 ]);
    (* (min_nil_list_fv, [ 0.1 ]); *)
    (len_list_fv, [ 5. ]);
    (* (min_len_list_fv, [ 5. ]); *)
    (uni_len_list_fv, [ 1.; 2.; 3.; 4.; 5.; 6.; 7.; 8.; 9.; 10.; 11. ]);
  ]

let rbtree_tests =
  [
    (* (uni_height_rbtree_fv, [ 2.; 3.; 4. ]); *)
    (* (b_rbtree_fv, [ 0.2 ]); *)
    (* (b_rbtree_fv, [ 0.4 ]); *)
    (b_rbtree_fv, [ 0.333 ]);
    (* (b_rbtree_fv, [ 0.6 ]); *)
    (* (min_b_rbtree_fv, [ 0.2 ]); *)
  ]

let depthtree_tests =
  [
    (height_tree_fv, [ 3. ]);
    (h_balanced_tree_fv, [ 1.5 ]);
    (h_balanced_tree_fv, [ 0.3 ]);
    (stick_tree_fv, [ 0.8 ]);
    (stick_tree_fv, [ 0.5 ]);
    (stick_tree_fv, [ 0.1 ]);
    (* (min_height_tree_fv, [ 3. ]); *)
    (* (min_stick_tree_fv, [ 0.1 ]); *)
    (uni_height_tree_fv, [ 0.; 1.; 2.; 3.; 4.; 5. ]);
    (h_balanced_tree_fv, [ 2. ]);
  ]

let depthbst_tests =
  [
    (height_tree_fv, [ 5. ]);
    (h_balanced_tree_fv, [ 0.3 ]);
    (h_balanced_tree_fv, [ 2. ]);
    (stick_tree_fv, [ 0.8 ]);
    (stick_tree_fv, [ 0.5 ]);
    (stick_tree_fv, [ 0.1 ]);
    (* (min_height_tree_fv, [ 3. ]); *)
    (* (min_stick_tree_fv, [ 0.1 ]); *)
    (uni_height_tree_fv, [ 0.; 1.; 2.; 3.; 4.; 5. ]);
  ]

let dragen_tests =
  [
    (count_cons, [ 10.; 10.; 10.; 10. ]);
    (* uniform *)
    (count_cons, [ 30.; 10.; 10.; -1. ]);
    (* weighted A *)
    (count_cons, [ 10.; -1.; -1.; 30. ]);
    (* weighted B *)
    (count_cons, [ 10.; 0.01; 0.01; 0.01 ]);
    (* only A *)
    (count_cons, [ -1.; -1.; 0.01; -1. ]) (* without A *);
  ]

let para_enum_10_sizedlist_tests = [ (uni_len_list_fv, [ 10. ]) ]
let para_enum_5_sizedlist_tests = [ (uni_len_list_fv, [ 5. ]) ]
let p_sizedlist_tests = [ (uni_len_list_fv, [ 10. ]) ]
let rq3_sizedlist_tests = [ (uni_len_list_fv, [ 10. ]) ]
let rq3_depthtree_tests = [ (uni_height_tree_fv, [ 0.; 1.; 2.; 3.; 4.; 5. ]) ]

type test =
  | List_type of
      ((string * (unit -> int list) * int * int * int)
      * ((string
         * (float list -> int list list -> (float * float list) * float))
        * float list)
        list)
  | List_opt_type of
      ((string * (unit -> int list option) * int * int * int)
      * ((string
         * (float list -> int list option list -> (float * float list) * float))
        * float list)
        list)
  | Rb_type of
      ((string * (unit -> int Combinators.rbtree) * int * int * int)
      * ((string
         * (float list ->
           int Combinators.rbtree list ->
           (float * float list) * float))
        * float list)
        list)
  | Tree_type of
      ((string * (unit -> int Combinators.tree) * int * int * int)
      * ((string
         * (float list ->
           int Combinators.tree list ->
           (float * float list) * float))
        * float list)
        list)
  | Dragen_type of
      ((string * (unit -> Frequency_combinators.dragen_tree) * int * int * int)
      * ((string
         * (float list ->
           Frequency_combinators.dragen_tree list ->
           (float * float list) * float))
        * float list)
        list)

let tests =
  [
    ("sized_list_2", List_type (sizedlist_gen, sizedlist_tests_2));
    ("sized_list", List_type (sizedlist_gen, sizedlist_tests));
    ("even_list", List_type (evenlist_gen, evenlist_tests));
    ("rb_tree", Rb_type (rbtree_gen, rbtree_tests));
    ("depth_tree", Tree_type (depthtree_gen, depthtree_tests));
    ("depth_bst", Tree_type (depthbst_gen, depthbst_tests));
    ("dragen", Dragen_type (dragen_gen, dragen_tests));
    ("loaded_dice", Rb_type (ld_rbtree_gen, rbtree_tests));
    ("sorted_list", List_opt_type (sortedlist_gen, sortedlist_tests));
    ( "pe_sized_list_5",
      List_type (sizedlist_para_enum_gen_5, para_enum_5_sizedlist_tests) );
    ( "pe_sized_list_10",
      List_type (sizedlist_para_enum_gen_10, para_enum_10_sizedlist_tests) );
    ("p1_sized_list", List_type (sizedlist_para_1_gen, sizedlist_tests));
    ("p2_sized_list", List_type (sizedlist_para_2_gen, sizedlist_tests));
    ("p2_even_list", List_type (evenlist_para_2_gen, evenlist_tests));
    ("p2_depth_tree", Tree_type (depthtree_para_2_gen, depthtree_tests));
    ("p2_depth_bst", Tree_type (depthbst_para_2_gen, depthbst_tests));
    ("p2_rb_tree", Rb_type (rbtree_para_2_gen, rbtree_tests));
    ("ur_depth_tree", Tree_type (ur_depthtree_gen, depthtree_tests));
    ("ur_depth_bst", Tree_type (ur_depthbst_gen, depthbst_tests));
    ("ur_rb_tree", Rb_type (ur_rbtree_gen, rbtree_tests));
    ("ur_sized_list", List_type (ur_sizedlist_gen, sizedlist_tests));
    ("ur_even_list", List_type (ur_evenlist_gen, evenlist_tests));
    ("ur5_depth_tree", Tree_type (ur5_depthtree_gen, depthtree_tests));
    ("ur5_depth_bst", Tree_type (ur5_depthbst_gen, depthbst_tests));
    ("ur5_sized_list", List_type (ur5_sizedlist_gen, sizedlist_tests));
    ("ur10_sized_list", List_type (ur10_sizedlist_gen, sizedlist_tests));
    ("ur_lin_depth_tree", Tree_type (ur_lin_depthtree_gen, depthtree_tests));
    ("ur_lin_depth_bst", Tree_type (ur_depthbst_gen, depthbst_tests));
    ("rq3_p2_sized_list", List_type (sizedlist_para_2_gen, rq3_sizedlist_tests));
    (* ("rq3_ur_depth_tree", Tree_type (ur_depthtree_gen, rq3_depthtree_tests)); *)
  ]

let evaluate_test test_list =
  match test_list with
  | List_type (g, fvl) ->
      List.iter
        (fun x ->
          let fv, goal = x in
          evaluate g fv goal)
        fvl
  | List_opt_type (g, fvl) ->
      List.iter
        (fun x ->
          let fv, goal = x in
          evaluate g fv goal)
        fvl
  | Rb_type (g, fvl) ->
      List.iter
        (fun x ->
          let fv, goal = x in
          evaluate g fv goal)
        fvl
  | Tree_type (g, fvl) ->
      List.iter
        (fun x ->
          let fv, goal = x in
          evaluate g fv goal)
        fvl
  | Dragen_type (g, fvl) ->
      List.iter
        (fun x ->
          let fv, goal = x in
          evaluate g fv goal)
        fvl

let () =
  Arg.parse speclist set_data_type usage_msg;

  let test =
    match List.assoc_opt !data_type tests with
    | Some s -> s
    | None -> failwith "unknown test"
  in

  let _ = evaluate_test test in
  close_out top_oc;
  ()

(* Kolmogorov–Smirnov test / make buckets *)
