type state = (state_rec ref) and state_rec = {
        mem_ptr: int;
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

let get_state_index (stt:state) = !stt.mem_ptr

let create_state (u:unit) = ref {
        mem_ptr      = 0;
        on_loop      = false;
    }

let get_token_expr (tkn:token) : expr =
    match tkn with
    | (exp, ind) -> exp

let get_token_index (tkn:token) : int = 
    match tkn with
    | (exp, ind) -> ind

let match_expr (to_token, i) = (match_char to_token, i)

let increase mem index = (Bytes.get_uint8 mem index + 1) |> Bytes.set_uint8 mem index

let decrease mem index = (Bytes.get_uint8 mem index - 1) |> Bytes.set_uint8 mem index

let print_mem mem index = print_char( Bytes.get mem index)

let save_char mem index = (Bytes.set mem index (input_char stdin))

let move_ptr_right this_state =
    let index = !this_state.mem_ptr in
    this_state := {
        !this_state with 
        mem_ptr =
            if (index = 30_000)
            then 0 
            else (index + 1)
    }

let move_ptr_left this_state = 
    let index = (!this_state.mem_ptr) in
    this_state := {
        !this_state with 
        mem_ptr =
            if (index = 0)
            then 30_000 else (index - 1)
    }

let is_mem_of_ptr_zero this_state mem =
    let index = !this_state.mem_ptr in
    if index >= 0
    then (Bytes.get_uint8 mem index = 0)
    else raise (Failure ("is_mem_of_ptr_zero : cannot access negative index"))

let begin_loop_state this_state =
    ref { 
        !this_state with 
        on_loop = true ; 
    }

let execute_expression states_stack mem expression =
    match states_stack with
    | [] -> raise (Failure "execute_expression: Cannot execute and expression without states")
    | this_state :: stack_tail ->
        let this_index = get_state_index this_state in
        match expression with
        | Mv_ptr_r -> move_ptr_right this_state 
        | Mv_ptr_l -> move_ptr_left this_state 
        | Incr_val -> increase mem this_index 
        | Decr_val -> decrease mem this_index 
        | Print_char -> print_mem mem this_index 
        | Save_char -> save_char mem this_index 
        | Beg_while -> ()
        | End_while -> ()
        | Nothing ->  ()

let tokenize_parsed_list lst = List.map (fun el -> match_expr el) lst

let list_length lst = List.fold_right (fun elem accum -> accum + 1) lst 0

let create_queue_from_token_list token_list = 
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

let rec statement_to_list stmt =
    match stmt with
    | Expression expr -> [stmt]
    | Sequence seq -> seq
    | While while_stmt -> statement_to_list while_stmt 

let add_to_first_stmt stmt_to_add stmt_list =
    match stmt_list with
    | [] ->  [stmt_to_add]
    | first :: stmt_list_tail -> 
            match first with
            | While _ -> stmt_to_add :: stmt_list
            | _ ->
                    let first_as_list = statement_to_list first in 
                    (Sequence (first_as_list @ [stmt_to_add]) ) :: stmt_list_tail

let sequenciate_statements_list statements =
    if list_length statements = 1
    then List.hd statements
    else Sequence statements

let rec parse_syntax_helper token_queue statements = 
    if Queue.is_empty token_queue
    then sequenciate_statements_list (List.rev  statements)
    else 
        let tkn :token = Queue.take token_queue in
        match (get_token_expr tkn) with
        | End_while -> sequenciate_statements_list (List.rev  statements)
        | Beg_while -> 
                let while_stmt = 
                    While (parse_syntax_helper token_queue [])
                in
                parse_syntax_helper token_queue (while_stmt::statements)
        | Nothing -> parse_syntax_helper token_queue statements
        | _ -> parse_syntax_helper token_queue (add_to_first_stmt (Expression tkn) statements)

let parse_syntax (token_list: token list) = 
    let token_queue : token Queue.t = create_queue_from_token_list token_list in
    parse_syntax_helper token_queue []

let rec read_file file lst = 
    try 
        (read_file file ((input_char file)::lst)) 
    with
    | _ -> lst

let rec enumerate file index char_pos_tuple_list = 
    try 
        let next_tuple = ((input_char file), index) in
        enumerate file (index + 1) (next_tuple::char_pos_tuple_list)
    with
    | _ -> char_pos_tuple_list

let read_enumerate_file filename = enumerate (open_in filename) 0 []

let filter_expr token_list f_expr = 
    List.filter (fun tkn -> get_token_expr tkn |> (fun expr -> expr = f_expr)) token_list  

let filter_opening_brackets token_list = filter_expr token_list Beg_while

let filter_closing_brackets token_list = filter_expr token_list End_while

let check_syntax token_list = 
    match
        list_length (filter_opening_brackets token_list) = list_length (filter_closing_brackets token_list)  
    with
    | true -> ()
    | false -> raise (Failure "Number of Opening and Closing Brackets do not match")

let rec pair_loop_expressions token_list beg_while_stack pair_list =
    match token_list with
    | [] -> (
            match beg_while_stack with
            | [] -> pair_list
            | head::tail -> 
                    let index = string_of_int (get_token_index head) in
                    raise (
                        Failure ("Dangling [ at position " ^ index)
                    )
    )
    | tkn::list_tail -> 
            let expression = get_token_expr tkn in
            let index = string_of_int (get_token_index tkn) in

            if expression = Beg_while
            then (pair_loop_expressions list_tail (tkn::beg_while_stack) pair_list)

            else if expression = End_while
            then
                match beg_while_stack with
                | [] -> raise (
                            Failure ("Dangling ] at position " ^ index)
                        )
                | matching_token::stack_tail -> 
                        pair_loop_expressions 
                            list_tail 
                            stack_tail 
                            ((matching_token, tkn)::pair_list)

            else pair_loop_expressions list_tail beg_while_stack pair_list

let get_matching_while_tokens token_list = pair_loop_expressions token_list [] [] 

let rec interpret (states_stack : state) (mem: bytes) (a_stmt:statement) =
    match a_stmt with
    | Expression exp_to_execute -> 
            ignore (execute_expression [states_stack] mem (get_token_expr exp_to_execute)) ; ()
    | Sequence seq -> 
            ignore (List.map (fun a_seq -> interpret states_stack mem a_seq) seq) ; ()
    | While while_seq -> 
            let while_state = begin_loop_state states_stack in
            while not (is_mem_of_ptr_zero states_stack mem)
            do 
                ignore (interpret while_state mem while_seq)
            done 

let main = 
    let name = "bloody_stupid_testing.bf" in

    let mem = Bytes.create 30_000 in 

    let tokens = read_enumerate_file name |> List.rev |> tokenize_parsed_list in

    let _ = get_matching_while_tokens tokens in

    let _ = check_syntax tokens in 

    let this_state = create_state () in

    let program = parse_syntax tokens in

    interpret this_state mem program;

    print_endline ""
