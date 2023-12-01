let explode s = List.init (String.length s) (String.get s);;

let extract_integers2 str =
  let char_list = explode str in
  let rec extract acc str = 
   match str with
    | [] -> List.rev acc
    | '1'::t -> extract (1::acc) t
    | '2'::t -> extract (2::acc) t
    | '3'::t -> extract (3::acc) t
    | '4'::t -> extract (4::acc) t
    | '5'::t -> extract (5::acc) t
    | '6'::t -> extract (6::acc) t
    | '7'::t -> extract (7::acc) t
    | '8'::t -> extract (8::acc) t
    | '9'::t -> extract (9::acc) t
    | 'o'::'n'::'e'::_ -> extract (1::acc) (List.tl str)
    | 't'::'w'::'o'::_ -> extract (2::acc) (List.tl str) 
    | 't'::'h'::'r'::'e'::'e'::_ -> extract (3::acc) (List.tl str)
    | 'f'::'o'::'u'::'r'::_ -> extract (4::acc) (List.tl str)
    | 'f'::'i'::'v'::'e'::_ -> extract (5::acc) (List.tl str)
    | 's'::'i'::'x'::_ -> extract (6::acc) (List.tl str)
    | 's'::'e'::'v'::'e'::'n'::_ -> extract (7::acc) (List.tl str)
    | 'e'::'i'::'g'::'h'::'t'::_ -> extract (8::acc) (List.tl str)
    | 'n'::'i'::'n'::'e'::_ -> extract (9::acc) (List.tl str)
    | _::t -> extract acc t
  in
  extract [] char_list
;;

let extract_integers str =
  let len = String.length str in
  let rec extract acc start =
    if start < len then
      match str.[start] with
      | '0'..'9' as digit ->
        let value = int_of_char digit - int_of_char '0' in
        extract (value :: acc) (start + 1)
      | _ -> extract acc (start + 1)
    else
      List.rev acc
  in
  extract [] 0
;;

let rec first_last = function
    | [] -> failwith "empty list"
    | [e] -> (e, e)
    | [e1;e2] -> (e1,e2) 
    | e1 :: _ :: r -> first_last (e1::r)
;;

let to_num l = let (a,b) = first_last l in 10 * a + b;;

let read_file filename =
  let file = open_in filename in
  let rec read_lines acc =
    try
      let line = input_line file in
      read_lines (line :: acc)
    with
      | End_of_file -> acc
  in
  let lines = read_lines [] in
  close_in file;
  lines
;;

let solve1 = List.fold_left (fun acc line -> acc + (to_num (extract_integers line))) 0;;
let solve2 = List.fold_left (fun acc line -> acc + (to_num (extract_integers2 line))) 0;;

read_file "inputs/day1" |> solve1 |> print_int;
print_newline ();
read_file "inputs/day1" |> solve2 |> print_int;
print_newline ();