exception Warning of Location.t * string

let warnings : exn list ref option ref = ref None

let raise_warning exn =
  match !warnings with
  | None -> raise exn
  | Some l -> l := exn :: !l

let prerr_warning loc w =
  match !warnings with
  | None -> Location.print_warning loc Format.err_formatter w
  | Some l ->
    let ppf, to_string = Misc.ppf_to_string () in
    Location.print_warning loc ppf w;
    match to_string () with
      | "" -> ()
      | s ->  l := Warning (loc,s) :: !l
                       
let () = 
  Location.prerr_warning_ref := prerr_warning

let catch_warnings f =
  let caught = ref [] in
  let previous = !warnings in
  warnings := Some caught;
  let result =
    try Misc.Inr (f())
    with e -> Misc.Inl e
  in
  warnings := previous;
  !caught, result

let compare_pos pos loc =
  let open Location in
  let pos = Misc.split_pos pos in
  if pos < Misc.split_pos loc.loc_start
  then -1
  else if pos > Misc.split_pos loc.loc_end
  then 1
  else 0

let union a b = 
  let open Location in
  match a,b with
  | a, { loc_ghost = true } -> a
  | { loc_ghost = true }, b -> b
  | a,b ->
    let loc_start =
      if Misc.split_pos a.loc_start <= Misc.split_pos b.loc_start
      then a.loc_start
      else b.loc_start
    and loc_end =
      if Misc.split_pos a.loc_end <= Misc.split_pos b.loc_end
      then b.loc_end
      else a.loc_end
    in
    { loc_start ; loc_end ; loc_ghost = a.loc_ghost && b.loc_ghost }

