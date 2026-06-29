let () =
  let chunk_out = String.make (4096 * 2) 'a' in
  let chunk_err = String.make (4096 * 2) 'b' in
  for _ = 1 to 32 do
    print_string chunk_out;
    Printf.eprintf "%s" chunk_err
  done
