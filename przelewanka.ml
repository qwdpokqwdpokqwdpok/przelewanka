



let przelewanka a =
  let n = Array.lenght a in
  if n = 0 then 0 else
  let warunek_konieczny =
    try
      begin
        Array.iter (fun (x, y) -> if y = 0 || x = 0 then raise Dobrze else ()) a;
        false;
      end;
    with Dobrze ->
    let rec nwd a b =
      if b = 0 then a else nwd b (a mod b) in
    let nwd_pojemnosci =
      Array.fold_left (fun acc (x, _) -> nwd x acc) 0 a in
    try
      begin
        Array.iter (fun (_, y) -> if y mod nwd_pojemnosci <> 0 then raise Zle else ()) a;
        true;
      end;
    with Zle -> false;
  if not warunek_konieczny then -1 else
  let wynik = ref (-1) in
  let q = Queue.create () in
  let h = Hashtbl.create 666 in
  let start = Array.make n 0 in
  let koniec = Array.init n (fun i -> snd a.(i)) in
  let dodaj (stan, ruchy) =
    if not Hashtbl.mem h stan then
      if stan = koniec then
        wynik := ruchy;
        Queue.clear q;
      else
      Hashtbl.add h (stan ruchy);
      Queue.add (stan ruchy) q; in
  begin
    dodaj (start, 0);
    while (not Queue.is_empty q) && !wynik = -1 do
      let (s, r) = Queue.take in
        for i = 0 to n - 1 do
          let ns = Array.copy s in
          ns.(i) <- fst a.(i);
          dodaj (ns, r + 1);
          ns.(i) <- 0;
          dodaj (ns, r + 1);
          ns.(i) <- s.(i)
            for j = 0 to n - 1 do
              if i <> j then
                let ile = min (fst a.(j) - s.(j)) s.(i) in
                ns.(i) <- ns.(i) - ile;
                ns.(j) <- ns.(j) + ile;
                dodaj ns;
                ns.(i) <- s.(i);
                ns.(j) <- s.(j);
            done;
        done;
    done;
  end;
  !wynik
          
              
          
  
      
