val match_char : char -> expr

val action : expr -> ()

val get_state_index : state -> int

val unbox_expr : token -> expr

val unbox_index : token -> int

val match_expr : char -> int -> expr * int

val increase : bytes -> int -> ()

val decrease : bytes -> int -> ()

val print_mem : bytes -> int -> ()

val save_char : bytes -> int -> ()

val move_ptr_right : state -> ()

val move_ptr_left : state -> ()

val is_mem_ptr_zero : state -> bool

val change_loop_state : state -> ()

val begin_loop_state : state -> int -> state

val end_loop_if_possible : state list -> bytes -> state list

val statement_to_list : statement -> statement list 

val add_to_first_stmt : statement -> statement list -> statement list

val parse_syntax_helper : token Queue.t -> statement list -> statement

val push_to_queue : token list -> token Queue.t

val parse_syntax : token list -> statement

val parse :  in_channel -> int -> char list

val enumerate : in_channel -> int -> (char * int) list

val enumerate_file : string -> (char * int) list

val tokenize : (char * int) list -> token list

val filter_expr : token list -> expr -> token list

val filter_opening_brackets : token list -> token list

val filter_closing_brackets : token list -> token list

val check_syntax : token list -> ()

val pair_loop_expressions : token list -> token list -> (token * token) list -> (token * token) list 

val get_while_pair_expressions : token list -> (token * token) list

val list_length : 'a list -> int

val interpret : state list -> bytes -> expr -> state list

val interpret_from_list : state list -> bytes -> token list -> ()
