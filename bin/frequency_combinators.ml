let frequency_gen () =
  let f = QCheck.Gen.float (QCheck_runner.random_state ()) in
  if f < 0.25 then
    true
  else
    false