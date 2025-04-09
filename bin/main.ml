module Env = Zzenv
let () = print_endline "Hello, World!"

(* $ ./main.exe print-coverage-types meta-config.json data/benchmark/quickchick/sizedlist/_under.ml *)


let process meta_config_file source_file () =
  let () = Env.load_meta meta_config_file in
  let code = Commands.Cre.preprocess source_file () in
  code

let code =
  (* let argc = Array.length Sys.argv in *)

  let config = Array.get Sys.argv 1 in
  let source = Array.get Sys.argv 2 in

  process config source ()



(* ../Cobb/underapproximation_type/meta-config.json  *)
(* ../Cobb/underapproximation_type/data/benchmark/quickchick/sizedlist/_under.ml *)