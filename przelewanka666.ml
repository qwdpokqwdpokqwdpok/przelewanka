let przelewanka a =
  let n = Array.length a in
  if n = 0 then 0 else
  let start = Array.make n 0 in
  let koniec = Array.init n (fun i -> snd a.(i)) in
  if start = koniec then 0 else
  let warunek_konieczny =
    try
      begin
        Array.iter (fun (x, y) -> if y = 0 || x = y then raise Exit else ()) a;
        false;
      end;
    with Exit ->
    let rec nwd a b =
      if b = 0 then a else nwd b (a mod b) in
    let nwd_pojemnosci =
      Array.fold_left (fun acc (x, _) -> nwd x acc) 0 a in
    try
      begin
        Array.iter (fun (_, y) -> if y mod nwd_pojemnosci <> 0 then raise Exit else ()) a;
        true;
      end;
    with Exit -> false; in
  if not warunek_konieczny then -1 else
  let wynik = ref (-1) in
  let q = Queue.create () in
  let h = Hashtbl.create 666 in
  let dodaj (stan, ruchy) =
    if !wynik = -1 && not (Hashtbl.mem h stan) then (
      if stan = koniec then (
        wynik := ruchy;
        Queue.clear q;)
      else (
      Hashtbl.add h (Array.copy stan) true;
      (* czemu musze dodawac drugi argument do tablicy hashy *)
      Queue.add ((Array.copy stan), ruchy) q;)) in
  begin
    dodaj (start, 0);
    while not (Queue.is_empty q) do
      let (s, r) = Queue.take q in
        for i = 0 to n - 1 do
          let ns = Array.copy s in
          ns.(i) <- fst a.(i);
          dodaj (ns, r + 1);
          ns.(i) <- 0;
          dodaj (ns, r + 1);
          ns.(i) <- s.(i);
            for j = 0 to n - 1 do
              if i <> j then
                let ile = min (fst a.(j) - s.(j)) s.(i) in
                ns.(i) <- ns.(i) - ile;
                ns.(j) <- ns.(j) + ile;
                dodaj (ns, r + 1);
                ns.(i) <- s.(i);
                ns.(j) <- s.(j);
            done;
        done;
    done;
  end;
  !wynik
