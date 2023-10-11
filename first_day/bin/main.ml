(* A custom type that defines types of ranges *)
type range =
    | Ascending
    | Descending
    | Invalid

(* Parse arguments *)
let parse_arguments a b =
    let int_a = int_of_string_opt a in
    let int_b = int_of_string_opt b in
    match int_a, int_b with
    | None, Some _int_b -> raise (Invalid_argument a)
    | Some _int_a, None -> raise (Invalid_argument b)
    | None, None -> raise (Invalid_argument "Both arguments are invalid")
    | Some int_a, Some int_b -> [|int_a; int_b|]

(* Functions for generating ascending and descending ranges *)
let rec range_des a b =
    if a < b then []
        else a :: range_des (a - 1) b

let rec range_asc a b =
    if a > b then []
        else a :: range_asc (a + 1) b

(* Takes in a tuple as a parameter and matches to a range type*)
let determine_range_type a b =
    match (a, b) with
    | a, b when a < b -> Ascending
    | a, b when a > b -> Descending
    | _ -> Invalid
    
(* The main function of the program *)
let generate_range a b =
    match determine_range_type a b with
    | Ascending -> range_asc a b
    | Descending -> range_des a b
    | Invalid -> [a]

(* Parse arguments *)
let parsed_args = parse_arguments Sys.argv.(1) Sys.argv.(2)

(* Value to print *)
let rangey = generate_range parsed_args.(0) parsed_args.(1)

let main =
    List.iter print_int rangey

let () = main 
