module Set = Set.Make (Char)

type cell =
  | Node of t * t
  | Leaf

and t = { cell: cell; set: Set.t }

let dummy = { cell = Leaf; set = Set.empty }

let create freqs =
  if List.is_empty freqs then invalid_arg "Huffman.create"
  else
    let freqs = Array.of_list freqs in
    let sz = Array.length freqs in
    let hp = Heap.mk ~dummy sz in
    Array.iter
      (fun (f, c) ->
        Heap.insert hp { cell = Leaf; set = Set.singleton c } f
      ) freqs;
    while Heap.size hp > 1 do
      let t1, w1 = Heap.delete_min hp
      and t2, w2 = Heap.delete_min hp in
      let t = { cell = Node (t1, t2); set = Set.union t1.set t2.set } in
      let w = w1 + w2 in
      Heap.insert hp t w
    done;
    Heap.delete_min hp |> fst

let rec encode_char { cell; _ } acc c =
  match cell with
  | Leaf -> acc
  | Node (left, right) ->
    match Set.find c left.set with
    | exception Not_found ->
      begin match Set.find c right.set with
      | exception Not_found -> raise_notrace Exit
      | _ -> encode_char right (true :: acc) c
      end
    | _ -> encode_char left (false :: acc) c

let encode t bs =
  try
    Bytes.fold_left (encode_char t) [] bs
    |> List.rev
    |> Option.some
  with Exit -> None

let decode t bs =
  let rec loop acc { cell; set } bs =
    match bs with
    | [] -> Set.choose set :: acc
    | b :: bs ->
      match cell with
      | Leaf ->
        loop (Set.choose set :: acc) t (b :: bs)
      | Node (left, right) ->
        if b then loop acc right bs
        else loop acc left bs
  in
  loop [] t bs
  |> List.rev |> List.to_seq |> Bytes.of_seq |> Option.some
