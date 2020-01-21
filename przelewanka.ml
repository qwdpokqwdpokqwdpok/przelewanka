



let przelewanka a =
  let warunek_konieczny =
    try
      begin
        Array.iter (fun (x, y) -> if y = 0 || x = 0 then raise Dobrze else ()) a;
        false
      end
    with Dobrze ->
    let rec nwd a b =
      if b = 0 then a else nwd b (a mod b) in
    let nwd_pojemnosci =
      Array.fold_left (fun acc (x, _) -> nwd x acc) 0 a in
    try
      begin
        Array.iter (fun (_, y) -> if y mod nwd_pojemnosci <> 0 then raise Zle else ()) a;
        true
      end
    with Zle -> false

