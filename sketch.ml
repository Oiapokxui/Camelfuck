type state = 
    | None
    | Some of (state_rec ref) and state_rec = {
        mem_ptr: int;
        start_loop: int option;
        end_loop: int option;
        on_loop: bool
    } 

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

type token = (expr * int)

type statement =
    | Sequence of statement list
    | While of statement
    | Expression of token

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

let get_state_index (stt:state) =
    match stt with
    | None -> raise (Failure "get_state_index: State should not be none")
    | Some a -> !a.mem_ptr

let get_state_index (stt:state) =
    match stt with
    | None -> raise (Failure "get_state_index: State should not be none")
    | Some record -> !record.mem_ptr

let create_state (u:unit) = Some (ref {
        mem_ptr      = 0;
        on_loop      = false;
        start_loop   = Some 0;
        end_loop     = Some 0;
    }) 

let unbox_expr (tkn:token) : expr =
    match tkn with
    | (exp, ind) -> exp

let unbox_index (tkn:token) : int = 
    match tkn with
    | (exp, ind) -> ind

let match_expr ((to_token:char), (i:int)) = (match_char to_token, i)

let increase mem index = (Bytes.get_uint8 mem index + 1) |> Bytes.set_uint8 mem index

let decrease mem index = (Bytes.get_uint8 mem index - 1) |> Bytes.set_uint8 mem index

let print_mem mem index = print_char( Bytes.get mem index)

let save_char mem index = (Bytes.set mem index (input_char stdin))

let move_ptr_right (this_state: state) =
    match this_state with
    | None -> ()
    | Some this_record ->
            let index = !this_record.mem_ptr in
            this_record := {
                !this_record with 
                mem_ptr =
                    if (index = 30_000)
                    then 0 
                    else (index + 1)
            }

let move_ptr_left (this_state: state) = 
    match this_state with
    | None -> ()
    | Some this_record ->
            let index = (!this_record.mem_ptr) in
            this_record := {
                !this_record with 
                mem_ptr =
                    if (index = 0)
                    then 30_000 else (index - 1)
            }

let is_mem_ptr_zero (this_state: state) (mem: bytes) =
    match this_state with
    | None -> raise (Failure ("is_mem_ptr_zero : cannot access None state"))
    | Some s -> (Bytes.get_uint8 mem (!s.mem_ptr) = 0)

let change_loop_state (this_state: state) =
    match this_state with
    | None -> ()
    | Some this_record ->
            let truth_val = (!this_record.on_loop) in 
            let index = 
                if not truth_val 
                then (!this_record.mem_ptr) 
                else (get_state_index this_state) 
            in
            this_record := { 
                !this_record with 
                on_loop = not truth_val ; 
                start_loop = Some index 
            }

let begin_loop_state (this_state: state) (end_while_pos: int) =
    match this_state with
    | None -> None
    | Some this_record ->
            let index = (!this_record.start_loop) in
            Some (ref { 
                !this_record with 
                on_loop = true ; 
                start_loop = index ;
                end_loop = Some end_while_pos
            })

let end_loop_if_possible (states: state list) (mem: bytes) = 
    match states with
    | [] -> []
    | loop_state::states_tail ->
            if (is_mem_ptr_zero loop_state mem)
            then states_tail
            else states

let interpret (states_stack:state list) (mem:bytes) (expression:expr) : state list =
    match states_stack with
    | [] -> raise (Failure "interpret: Cannot interpret without states")
    | this_state :: stack_tail ->
        let this_index = get_state_index this_state in
        match expression with
        | Mv_ptr_r -> 
                move_ptr_right this_state ; states_stack
        | Mv_ptr_l -> move_ptr_left this_state ; states_stack
        | Incr_val -> increase mem this_index ; states_stack
        | Decr_val -> decrease mem this_index ; states_stack
        | Beg_while -> print_endline "beg while" ; states_stack
        | End_while -> print_endline "end while" ; states_stack 
        | Print_char -> print_mem mem this_index ; states_stack
        | Save_char -> save_char mem this_index ; states_stack
        | Nothing -> print_endline "nothing" ; states_stack

let rec interpret_from_list 
    (states_queue:state list) 
    (mem:bytes) 
    (expression_list: token list) =
        match expression_list with
        | [] -> ()
        | token::expr_tail -> 
                let new_states_queue = interpret states_queue mem (unbox_expr token) in
                interpret_from_list new_states_queue mem expr_tail

let tokenize parsed_list : token list = List.map (fun el -> match_expr el) parsed_list

let list_length lst = List.fold_right (fun elem accum -> accum + 1) lst 0

let push_to_queue (token_list: token list) = 
    let rec push_to_queue_helper (tkn_lst: token list) (queue: token Queue.t) =
       match tkn_lst with
       | [] -> ()
       | tkn :: list_tail -> 
               Queue.add tkn queue ; 
               push_to_queue_helper list_tail queue 
    in
    let token_queue = Queue.create () in
    push_to_queue_helper token_list token_queue;
    token_queue

let rec statement_to_list (stmt: statement) : statement list =
    match stmt with
    | Expression expr -> [stmt]
    | Sequence seq -> seq
    | While while_stmt -> statement_to_list while_stmt 

let add_to_first_stmt (stmt_to_add:statement) (stmt_list:statement list) : statement list=
    match stmt_list with
    | [] ->  [stmt_to_add]
    | first :: stmt_list_tail -> 
            match first with
            | While _ -> stmt_to_add :: stmt_list
            | _ ->
                    let first_as_list = statement_to_list first in 
                    (Sequence (first_as_list @ [stmt_to_add]) ) :: stmt_list_tail

let sequenciate_statements_list (stmts: statement list) : statement =
    if list_length stmts = 1
    then List.hd stmts
    else Sequence stmts

let rec parse_syntax_helper (token_queue: token Queue.t) (stmts: statement list) : statement = 
    if Queue.is_empty token_queue
    then sequenciate_statements_list (List.rev  stmts)
    else 
        let tkn :token = Queue.take token_queue in
        match (unbox_expr tkn) with
        | End_while -> sequenciate_statements_list (List.rev  stmts)
        | Beg_while -> 
                let while_stmt = 
                    While (parse_syntax_helper token_queue [])
                in
                parse_syntax_helper token_queue (while_stmt::stmts)
        | Nothing -> parse_syntax_helper token_queue stmts
        | _ -> parse_syntax_helper token_queue (add_to_first_stmt (Expression tkn) stmts)

let parse_syntax (token_list: token list) = 
    let token_queue : token Queue.t = push_to_queue token_list in
    parse_syntax_helper token_queue []

let rec parse file lst = 
    try 
        (parse file ((input_char file)::lst)) 
    with
    | _ -> lst

let rec enumerate file index lst = 
    try 
        let next_tuple = ((input_char file), index) in
        enumerate file (index + 1) (next_tuple::lst)
    with
    | _ -> lst

let enumerate_file filename = enumerate (open_in filename) 0 []

let filter_expr (token_list:token list) (f_expr:expr) : token list = 
    List.filter (fun tkn -> unbox_expr tkn |> (fun expr -> expr = f_expr)) token_list  

let filter_opening_brackets (token_list:token list) : token list = filter_expr token_list Beg_while

let filter_closing_brackets (token_list:token list) : token list = filter_expr token_list End_while

let check_syntax (token_list:token list) = 
    match
        list_length (filter_opening_brackets token_list) = list_length (filter_closing_brackets token_list)  
    with
    | true -> ()
    | false -> raise (Failure "Number of Opening and Closing Brackets do not match")

let rec pair_loop_expressions token_list beg_while_stack pair_list =
    match token_list with
    | [] -> pair_list
    | tkn::list_tail -> 
            let expression = unbox_expr tkn in
            let index = string_of_int (unbox_index tkn) in

            if expression = Beg_while
            then (pair_loop_expressions list_tail (tkn::beg_while_stack) pair_list)

            else if expression = End_while
            then
                match beg_while_stack with
                | [] -> raise (
                            Failure ("Dangling End_while expression at position: " ^ index)
                        )
                | matching_token::stack_tail -> 
                        pair_loop_expressions 
                            list_tail 
                            stack_tail 
                            ((matching_token, tkn)::pair_list)

            else pair_loop_expressions list_tail beg_while_stack pair_list

let get_while_pair_expressions (token_list:token list) = pair_loop_expressions token_list [] [] 

let rec bora_caralho (states_stack : state) (mem: bytes) (a_stmt:statement) =
    match a_stmt with
    | Expression exp_to_execute -> 
            ignore(interpret [states_stack] mem (unbox_expr exp_to_execute)); ()
    | Sequence seq -> 
            ignore (List.map (fun a_seq -> bora_caralho states_stack mem a_seq) seq) ; ()
    | While while_seq -> 
            let while_state = begin_loop_state states_stack (get_state_index states_stack) in
            while not (is_mem_ptr_zero states_stack mem)
            do 
                ignore(bora_caralho while_state mem while_seq)
            done 

let main = 
    let name = "helloworld.bf" in

    let mem = Bytes.create 30_000 in 

    let tokens = enumerate_file name |> List.rev |> tokenize in

    let _ = get_while_pair_expressions tokens in

    let _ = check_syntax tokens in 

    let this_state = create_state () in

    let program = parse_syntax tokens in
    bora_caralho this_state mem program;
    print_endline ""
