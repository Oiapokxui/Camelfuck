val match_char : char -> expr

val get_state_index : state -> int

val create_state : unit -> state

val get_token_expr : token -> expr

val get_token_index : token -> int

val match_expr : char * int -> expr * int

val increase : bytes -> int -> ()

val decrease : bytes -> int -> ()

val print_mem : bytes -> int -> ()

val save_char : bytes -> int -> ()

val move_ptr_right : state -> ()

val move_ptr_left : state -> ()

val is_mem_of_ptr_zero : state -> -> bytes -> bool

val change_loop_state : state -> ()

val begin_loop_state : state -> state

val execute_expression : state list -> bytes -> expr -> ()

val statement_to_list : statement -> statement list 

val add_to_first_stmt : statement -> statement list -> statement list

val sequenciate_statements_list : statement list -> statement

val parse_syntax_helper : token Queue.t -> statement list -> statement

val create_queue_from_token_list : token list -> token Queue.t

val parse_syntax : token list -> statement

val read_file :  in_channel -> int -> char list

val enumerate : in_channel -> int -> (char * int) list

val read_enumerate_file : string -> (char * int) list

val tokenize_parsed_list : (char * int) list -> token list

val filter_expr : token list -> expr -> token list

val filter_opening_brackets : token list -> token list

val filter_closing_brackets : token list -> token list

val check_syntax : token list -> ()

val pair_loop_expressions : token list -> token list -> (token * token) list -> (token * token) list 

val get_matching_while_tokens : token list -> (token * token) list

val list_length : 'a list -> int

val interpret : state -> bytes -> statement -> ()
