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

  let buffer = Array.make n_bool 500 in

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
            let score_b, dist_b, chi_buckets_b = sample_weights gen score_func goal result_oc w in
            let w = Array.map (fun r -> r) w in
            min_score acc (score_b, dist_b, chi_buckets_b, w))
          best ratios_list
      in
      let s, _, _, _ = best in

      (* Printf.printf "s=%f\n" s; *)
      enum (depth - 2) best
  in

  let start_time = Unix.gettimeofday () in
  let best_score, best_dist, best_chi_buckets, best_weights =
    enum n_bool (max_float, 0., [], buffer)
  in
  let end_time = Unix.gettimeofday () in

  let _ = print_solutions extra_oc best_weights best_score in

  ( best_weights,
    best_score,
    (best_dist, best_chi_buckets),
    end_time -. start_time )

let greedy_enumerate_back (result_oc : out_channel) (gen : unit -> 'a) score_func
    goal print_all =
  let extra_oc = if print_all then Some result_oc else None in
  print_header extra_oc;

  Printf.printf "start\n";

  let ratios_list = [ 0.; 0.15; 0.3; 0.5; 0.9; 1.0 ] in

  let n_bool = Array.length !weights in

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
            let score_b, dist_b, chi_buckets_b = sample_weights gen score_func goal result_oc w in
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
            let score_b, dist_b, chi_buckets_b = sample_weights gen score_func goal result_oc w in
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



    