open Printf

exception Invalid_argument;;
exception No_arguments;;
exception Out_of_range;;

let get_list_from_command_line = 
	let rec _parse_list_command_line lst cur_index =
		if cur_index > 0 then
			_parse_list_command_line ((int_of_string Sys.argv.(cur_index))::lst) (cur_index - 1)
		else lst
	in
	let lst_size = Array.length Sys.argv - 2 in
	if lst_size <= 0 then
		raise No_arguments
	else
		_parse_list_command_line [] lst_size
;;

let get_indx_from_command_line =
	let argc = Array.length Sys.argv in
	let indx = (int_of_string Sys.argv.(argc - 1)) in
	if indx < 0 then
		raise Invalid_argument
	else
		indx;
;;

let my_nth lst indx =
    let rec _my_nth _lst _indx _cur_indx =
		match _lst with
		 	[] -> raise Out_of_range
		 	| head :: tail ->
		 		if _indx = _cur_indx then
					head
		 		else
		 			_my_nth tail _indx (_cur_indx + 1)
	in
	_my_nth lst indx 0
;;

let main =
	let lst = get_list_from_command_line in
	let indx = get_indx_from_command_line in
	let nth = my_nth lst indx in
	printf "\nYour list: ";
	List.iter (printf "%i ") lst;
	printf "\nYour index to find: %i" indx;
	printf "\nYour nth: %i" nth;
	printf "\n\n";
;;