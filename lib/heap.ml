type 'a t = {
  tree: int array;
  data: 'a array;
  mutable sz: int;
}

let[@inline always] size { sz; _ } = sz

exception Empty

let[@inline always] priority { tree; _ } i =
  Array.unsafe_get tree i

let[@inline always] get { tree; data; _ } i =
  Array.unsafe_get data i, Array.unsafe_get tree i

let[@inline always] set { tree; data; _ } i (v, p) =
  Array.unsafe_set data i v;
  Array.unsafe_set tree i p

let[@inline always] mk ~dummy cap = {
  tree = Array.init cap (fun _ -> 0);
  data = Array.init cap (fun _ -> dummy);
  sz = 0;
}

let[@inline always] full { tree; sz; _ } = Array.length tree = sz
let[@inline always] empty { sz; _ } = sz = 0
let[@inline always] left i = 2 * i + 1
let[@inline always] right i = 2 * i + 2
let[@inline always] parent i = (i - 1) / 2

let swap t i p =
  let tmp = get t i in
  set t i (get t p);
  set t p tmp

let rec percolate_up t i =
  let p = parent i in
  if priority t i < priority t p then (
    swap t i p;
    percolate_up t p
  )

let rec percolate_down t i =
  let l = left i and r = right i in
  let min = if l < t.sz && priority t l < priority t i then l else i in
  let min = if r < t.sz && priority t r < priority t min then r else min in
  if i <> min then (
    swap t i min;
    percolate_down t min
  )

let insert t v p =
  if full t then invalid_arg "Heap.insert"
  else (
    let i = t.sz in
    set t i (v, p);
    t.sz <- i + 1;
    percolate_up t i;
  )

let delete_min t =
  if empty t then raise Empty
  else
    let r = get t 0 in
    swap t 0 (t.sz - 1);
    t.sz <- t.sz - 1;
    percolate_down t 0;
    r
