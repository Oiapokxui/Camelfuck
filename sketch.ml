type expr = 
    | Mv_ptr_r 
    | Mv_ptr_l 
    | Incr_val 
    | Decr_val 
    | Beg_while 
    | End_while 
    | Print_char
    | Save_char
    | Nothing

type state = 
    | None
    | Some of (state_rec ref) and state_rec = {
        mem_ptr:int;
        on_loop:bool;
        start_loop:int;
        end_loop:int;
        nested_state:state;
    } 

type token = 
    | Expr of expr 
    | TokenTuple of (expr * int)

let match_char chr = 
    match chr with
    | '<' -> Mv_ptr_l
    | '>' -> Mv_ptr_r
    | '.' -> Print_char   
    | ',' -> Save_char
    | '+' -> Incr_val
    | '-' -> Decr_val
    | '[' -> Beg_while
    | ']' -> End_while
    | _ -> Nothing

let action (expression:expr) =
    match expression with
    | Mv_ptr_r -> print_endline "moving right"
    | Mv_ptr_l -> print_endline "moving left"
    | Incr_val -> print_endline "incr val"
    | Decr_val -> print_endline "decr val "
    | Beg_while -> print_endline "beg while"
    | End_while -> print_endline "end while"
    | Print_char -> print_endline "print char"
    | Save_char -> print_endline "save char"
    | Nothing -> print_endline "nothing"


let unbox_expr (tkn:token) : expr =
    match tkn with
    | TokenTuple (exp, ind) -> exp
    | Expr exp -> exp

let unbox_index (tkn:token) : (int option) = 
    match tkn with
    | TokenTuple (exp, ind) -> Some ind
    | Expr exp -> None

let match_expr ((to_token:char), (i:int)) = TokenTuple (match_char to_token, i)

let increase mem index = (Bytes.get_uint8 mem index + 1) |> Bytes.set_uint8 mem index

let decrease mem index = (Bytes.get_uint8 mem index - 1) |> Bytes.set_uint8 mem index

let print_mem mem index = print_char (Bytes.get mem index) 

let save_char mem index = (Bytes.set mem index (input_char stdin))

let move_ptr_right (this_state: state) = function
    | None -> ()
    | Some this_record ->
            let index = !this_record.mem_ptr in
            this_record := {
                !this_record with mem_ptr =
                    if (index = 30_000)
                    then 0 
                    else (index + 1)
            }

let move_ptr_left (this_state: state) = function
    | None -> ()
    | Some this_record ->
            let index = (!this_record.mem_ptr) in
            this_record := {
                !this_record with mem_ptr =
                    if (index = 0)
                    then 30_000 else (index - 1)
            }

let change_loop_state (this_state: state) = function
    | None -> ()
    | Some this_record ->
            let truth_val = (!this_record.on_loop) in 
            let index = 
                if not truth_val 
                then (!this_record.mem_ptr) 
                else (!this_record.start_loop) 
            in
            this_record := { 
                !this_record with 
                on_loop = not truth_val ; 
                start_loop = index 
            }

let tuple_tokenize parsed_list : token list = List.map (fun el -> match_expr el) parsed_list

let tokenize parsed_list = List.map (fun el -> match_char el) parsed_list

let rec parse file lst = 
    try 
        (parse file ((input_char file)::lst)) 
    with
    | _ -> lst

let rec parse_enumerate file index lst = 
    try 
        parse_enumerate file (index + 1) (((input_char file), index)::lst)
    with
    | _ -> lst

let parse_enumerate_filename filename = parse_enumerate (open_in filename) 0 []

let list_length lst = List.fold_right (fun elem accum -> accum + 1) lst 0

let filter_char (token_list:token list) (f_expr:expr) : token list = List.filter (fun tkn -> unbox_expr tkn |> (fun expr -> expr = f_expr)) token_list  

let filter_opening_brackets (token_list:token list) : token list = filter_char token_list Beg_while

let filter_closing_brackets (token_list:token list) : token list = filter_char token_list End_while

let check_syntax (token_list:token list) = 
    let bool_val =  list_length (filter_opening_brackets token_list) = list_length (filter_closing_brackets token_list)  in
    match bool_val with
    | true -> ()
    | false -> raise (Failure "Number of Opening and Closing Brackets do not match")

let main = 
    let name = "helloworld.bf" in

    let mem = Bytes.create 30_000 in 

    let tokens = parse_enumerate_filename name |> List.rev |> tuple_tokenize in

    let _ = check_syntax tokens in 
     
    let this_state = Some (ref {
        mem_ptr      = 0;
        on_loop      = false;
        start_loop   = 0;
        end_loop     = 0;
        nested_state = None;
    }) in
    
    let fuck tkn = action (unbox_expr tkn) in
    List.map (fun tkn -> fuck tkn) tokens
    ()
