module G = QCheck.Gen

let test =
  let gen = G.(
    range_subset ~size:5 33 94
    >|= Array.to_seq
    >|= List.of_seq
    >>= shuffle_l
    >|= List.mapi (fun f c -> (f + 1, Char.chr c))
    >>= fun l ->
      let bs_gen = bytes_size ~gen:(frequencyl l) (int_range 1 50) in
      pure (l, generate1 bs_gen))
  in
  let gen = QCheck.make @@ gen in
  QCheck.Test.make ~name:"Huffman" gen @@ fun (freqs, bs) ->
    let t = Huffman.create freqs in
    let x = Option.get @@ Huffman.encode t bs in
    let nbs = Option.get @@ Huffman.decode t x in
    Bytes.equal bs nbs

let () =
  exit @@ QCheck_base_runner.run_tests ~verbose:true [test]
