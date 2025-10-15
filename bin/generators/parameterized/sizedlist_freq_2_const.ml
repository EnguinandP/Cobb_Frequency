open Combinators
open Frequency_combinators
let rec sized_list_gen = fun s ->
  let (x_0) = sizecheck s in
  match x_0 with
  | true -> []
  | false ->
      let (m_b) = get_weight_idx 0 in
      let (m_rec) = get_weight_idx 1 in
      let (c_b) = get_weight_idx 2 in
      let (c_rec) = get_weight_idx 3 in
      (* let (w_base) = m_b * s + c_b in
      let () = set_w_idx 0 w_base in
      let (w_recursive) = m_rec * s + c_rec in
      let () = set_w_idx 1 w_recursive in
      let (base_case) = frequency_gen_list (w_base, (fun _ -> [])) in *)
      let (base_case) = freq_para_2_gen s c_b c_rec (m_b, (fun _ -> [])) in
      let (recursive_case) =
        base_case
          (m_b,
            (fun _ ->
               let (x_2) = subs s in
               let (x_3) = sized_list_gen x_2 in
               let (x_4) = int_gen () in x_4 :: x_3)) in
      recursive_case