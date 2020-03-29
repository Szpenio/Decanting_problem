(** PROJECT: Przelewanka **)
(** Author: Antoni Koszowski **)

(** checks whether the preconditions are met for the given set of data;
    all final volumes must be divisible by gcd of the mug volumes in general;
    there must be at least one full mug or with final volume equal 0         **)
let check vol final =
  let sat = ref false in
  Array.iter2 (fun x y -> if x = y || y = 0 then sat := true) vol final;
  if not !sat then false
  else
    let rec gcd a b =
      if b = 0 then a else gcd b (a mod b) in
    let d = ref vol.(0) in
    Array.iter (fun x -> d := gcd !d x) vol;
    Array.iter (fun y -> if y <> 0 && y mod !d <> 0 then sat := false) final;
    !sat

(** for the given array of pairs [|(x1,y1); (x2,y2); ...; (xn, yn)|],
    where xi is volume and yi expected state of each mug, using given operations,
    determines the minimal number of these operations
    needed to achieve the expected state or returns -1 if it is not possible **)
let przelewanka arr =
  let n = Array.length arr in
  let vol = Array.init n (fun id -> fst arr.(id)) in
  let final = Array.init n (fun id -> snd arr.(id)) in
  if n = 0 || (Array.make n 0) = final then 0
  else if not (check vol final) then -1
  else
    let steps = ref 0 in
    let stop = ref false in
    let q = Queue.create() in
    let ht = Hashtbl.create 1000000 in
    let next copy step =
      if not (Hashtbl.mem ht copy || !stop) then(
        if copy = final then(
          steps := step + 1;
          stop := true
        )
        else(
          Queue.add copy q;
          Hashtbl.add ht copy (step + 1)
        )
      ) in
    Queue.push (Array.make n 0) q;
    Hashtbl.add ht (Array.make n 0) 0;
    while not (Queue.is_empty q || !stop) do
      let cur = Queue.take q in
      let step = Hashtbl.find ht cur in
      for i = 0 to (n - 1) do
        let copy = Array.copy cur in
        copy.(i) <- vol.(i);
        next copy step
      done;
      for i = 0 to (n - 1) do
        let copy = Array.copy cur in
        copy.(i) <- 0;
        next copy step
      done;
      for i = 0 to (n - 1) do
        for j = 0 to (n - 1) do
          let copy = Array.copy cur in
          if i <> j && copy.(i) <> 0 && copy.(j) <> vol.(j) then(
            let to_full = vol.(j) - copy.(j) in
            if copy.(i) <= to_full then(
              copy.(j) <- copy.(j) + copy.(i);
              copy.(i) <- 0
            )
            else(
              copy.(j) <- copy.(j) + to_full;
              copy.(i) <- copy.(i) - to_full
            )
          );
          next copy step
        done;
      done;
    done;
    if !stop then !steps else -1
