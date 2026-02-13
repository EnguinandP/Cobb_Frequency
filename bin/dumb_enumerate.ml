open Search
open Frequency_combinators

let dumb_enumerate (result_oc : out_channel) (gen : unit -> 'a) score_func goal
    print_all =
  let extra_oc = if print_all then Some result_oc else None in
  print_header extra_oc;

  let weights_list = [ 1; 100; 200; 300; 400; 500; 600; 700; 800; 900; 1000 ] in

  let n_weights = Array.length !weights in

  let sample_weights (w : int array) =
    weights := w;

    let collect_start_time = Unix.gettimeofday () in
    let results = collect sample_size gen in
    let collect_end_time = Unix.gettimeofday () in

    let (dist, chi_buckets), cand_score = score_func goal results in

    (* print_iterations result_oc "0" [| 0; 0 |] cand_score dist
       (collect_end_time -. collect_start_time); *)
    (cand_score, dist, chi_buckets)
  in

  let min_score (score_a, dist_a, chi_buckets_a, weights_a)
      (score_b, dist_b, chi_buckets_b, weights_b) =
    if score_a < score_b then (score_a, dist_a, chi_buckets_a, weights_a)
    else (score_b, dist_b, chi_buckets_b, weights_b)
  in

  let buffer = Array.make n_weights 0 in

  let rec iterate_list depth best =
    if depth = n_weights then
      let score_b, dist_b, chi_buckets_b = sample_weights buffer in
      let w = Array.map (fun r -> r) buffer in
      min_score best (score_b, dist_b, chi_buckets_b, w)
    else
      List.fold_left
        (fun acc elem ->
          buffer.(depth) <- elem;
          let result = iterate_list (depth + 1) acc in
          result)
        best weights_list
  in

  let start_time = Unix.gettimeofday () in
  let best_score, best_dist, best_chi_buckets, best_weights =
    iterate_list 0 (max_float, 0., [], [| 0 |])
  in
  let end_time = Unix.gettimeofday () in

  let _ = print_solutions extra_oc best_weights best_score in

  ( best_weights,
    best_score,
    (best_dist, best_chi_buckets),
    end_time -. start_time )

let dumb_enumerate_ratios (result_oc : out_channel) (gen : unit -> 'a) score_func
    goal print_all =
  let extra_oc = if print_all then Some result_oc else None in
  print_header extra_oc;

  (* let ratios_list = [ 1.; 0.9; 0.8; 0.7; 0.6; 0.5; 0.4; 0.2; 0.1; 0. ] in *)
  (* let ratios_list = [ 1.; 0.75; 0.5; 0.25; 0. ] in *)
  (* these are strictly the ratios that appear 1 or more times from weights *)
  (* let ratios_list = [ 0.; 0.12; 0.14; 0.17; 0.29; 0.3; 0.5; 0.53; 1.0; ] in  *)
  (* above plus a bonus *)
  let ratios_list = [ 0.; 0.15; 0.3; 0.5; 0.9; 1.0; ] in 

  let weights_list =
    [| 1; 100; 200; 300; 400; 500; 600; 700; 800; 900; 1000 |]
  in

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

  let buffer = Array.make n_bool 0 in

  let rec iterate_list depth best =
    if depth >= n_bool then
      let score_b, dist_b, chi_buckets_b = sample_weights buffer in
      let w = Array.map (fun r -> r) buffer in
      min_score best (score_b, dist_b, chi_buckets_b, w)
    else
      List.fold_left
        (fun acc r ->
          if not (depth + 1 = n_bool) then (
            let w1 = Int.of_float (r *. 100.) in
            let w2 = Int.of_float ((1. -. r) *. 100.) in

            buffer.(depth) <- w1;
            buffer.(depth + 1) <- w2;
            let result = iterate_list (depth + 2) acc in
            result)
          else
            let w1 = Int.of_float (r *. 100.) in

            let i = List.find_index (fun x -> x = r) ratios_list in
            let i =
              match i with Some i' -> i' | None -> failwith "index error"
            in
            buffer.(depth) <- weights_list.(i);
            let result = iterate_list (depth + 1) acc in
            result)
        best ratios_list
  in

  let start_time = Unix.gettimeofday () in
  let best_score, best_dist, best_chi_buckets, best_weights =
    iterate_list 0 (max_float, 0., [], [| 0 |])
  in
  let end_time = Unix.gettimeofday () in

  let _ = print_solutions extra_oc best_weights best_score in

  ( best_weights,
    best_score,
    (best_dist, best_chi_buckets),
    end_time -. start_time )

