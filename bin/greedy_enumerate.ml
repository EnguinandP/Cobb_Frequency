open Search
open Frequency_combinators

let print_int_array arr =
  print_string "[|";
  Array.iter
    (fun x ->
      print_int x;
      print_string "; ")
    arr;
  print_endline "|]"

(* we know 5 unrollings *)
let greedy_enumerate (result_oc : out_channel) (gen : unit -> 'a) score_func
    goal print_all =
  let extra_oc = if print_all then Some result_oc else None in
  print_header extra_oc;

  Printf.printf "start\n";

  let ratios_list = [ 0.; 0.15; 0.3; 0.5; 0.9; 1.0 ] in

  let n_bool = Array.length !weights in

  let sample_weights (w : int array) =
    weights := w;

    let collect_start_time = Unix.gettimeofday () in
    let results = collect sample_size gen in
    let collect_end_time = Unix.gettimeofday () in

    let (dist, chi_buckets), cand_score = score_func goal results in

    print_iterations result_oc "0" [| 0; 0 |] cand_score dist
      (collect_end_time -. collect_start_time);

    (cand_score, dist, chi_buckets)
  in

  let min_score (score_a, dist_a, chi_buckets_a, weights_a)
      (score_b, dist_b, chi_buckets_b, weights_b) =
    if score_a < score_b then (score_a, dist_a, chi_buckets_a, weights_a)
    else (score_b, dist_b, chi_buckets_b, weights_b)
  in

  let buffer = Array.make n_bool 50 in

  (* at each depth, enumerate weights for each ratio in list *)
  let rec enum depth best =
    if depth <= 0 then best
    else
      let best =
        List.fold_left
          (fun acc r ->
            let w1 = Int.of_float (r *. 100.) in
            let w2 = Int.of_float ((1. -. r) *. 100.) in

            (* Printf.printf "%d %d\n" w1 w2; *)
            let _, _, _, best_weights = best in
            let w = Array.map (fun r -> r) best_weights in

            w.(depth - 1) <- w1;
            w.(depth - 2) <- w2;

            print_int_array w;

            (* calc score *)
            let score_b, dist_b, chi_buckets_b = sample_weights w in
            let w = Array.map (fun r -> r) w in
            min_score acc (score_b, dist_b, chi_buckets_b, w))
          best ratios_list
      in
      let s, _, _, _ = best in

      (* Printf.printf "s=%f\n" s; *)
      enum (depth - 2) best
  in

  let enum_back_at_depth depth best =
    if depth <= 0 then best
    else
      let best =
        List.fold_left
          (fun acc r ->
            let w1 = Int.of_float (r *. 100.) in
            let w2 = Int.of_float ((1. -. r) *. 100.) in

            (* Printf.printf "%d %d\n" w1 w2; *)
            let _, _, _, best_weights = best in
            let w = Array.map (fun r -> r) best_weights in

            w.(depth - 1) <- w1;
            w.(depth - 2) <- w2;

            print_int_array w;

            (* calc score *)
            let score_b, dist_b, chi_buckets_b = sample_weights w in
            let w = Array.map (fun r -> r) w in
            min_score acc (score_b, dist_b, chi_buckets_b, w))
          best ratios_list
      in
      best
  in

  let rec enum_back max_depth backtrack_amount best =
    if backtrack_amount > max_depth then best
    else
      let rec explore_depths current_depth best_acc =
        if current_depth < max_depth - backtrack_amount || current_depth <= 0
        then best_acc
        else
          let updated_best = enum_back_at_depth current_depth best_acc in
          explore_depths (current_depth - 2) updated_best
      in
      let best_at_this_backtrack = explore_depths max_depth best in
      enum_back max_depth (backtrack_amount + 2) best_at_this_backtrack
  in

  let start_time = Unix.gettimeofday () in
  let best_score, best_dist, best_chi_buckets, best_weights =
    enum_back n_bool 2 (max_float, 0., [], buffer)
  in
  let end_time = Unix.gettimeofday () in

  let _ = print_solutions extra_oc best_weights best_score in

  ( best_weights,
    best_score,
    (best_dist, best_chi_buckets),
    end_time -. start_time )

let test (result_oc : out_channel) (gen : unit -> 'a) score_func
    goal print_all =
  let extra_oc = if print_all then Some result_oc else None in
  print_header extra_oc;

  Printf.printf "start\n";

  let ratios_list = [ 0.; 0.15; 0.3; 0.5; 0.9; 1.0 ] in

  let n_bool = Array.length !weights in

  let sample_weights (w : int array) =
    weights := w;

    let collect_start_time = Unix.gettimeofday () in
    let results = collect sample_size gen in
    let collect_end_time = Unix.gettimeofday () in

    let (dist, chi_buckets), cand_score = score_func goal results in

    print_iterations result_oc "0" [| 0; 0 |] cand_score dist
      (collect_end_time -. collect_start_time);

    (cand_score, dist, chi_buckets)
  in

  let buffer = Array.make n_bool 50 in

  let buffer = [|50; 50; 66; 33; 75; 25; 80; 20; 83; 17; 86; 14; 87; 13; 89; 11; 90; 10; 91; 9|] in


  let start_time = Unix.gettimeofday () in
  let best_score, best_dist, best_chi_buckets = sample_weights buffer 
  in
  let end_time = Unix.gettimeofday () in

  let _ = print_solutions extra_oc !weights best_score in

  ( !weights,
    best_score,
    (best_dist, best_chi_buckets),
    end_time -. start_time )


    