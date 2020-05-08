open Printf
;;

type super_list = Int of int | List of super_list list
;;

let concat_numbers number_one number_two =
	match (number_one, number_two) with
		| (Int n1, List l2) -> List ((Int n1) :: l2)
		| (List l1, List l2) -> List ((List l1) :: l2)
		| (Int n1, Int n2) -> failwith "Not implemented"
		| (List l1, Int n2) -> failwith "Not implemented"
;;

let rec reverse_super_list_to_new_list lst new_lst = 
			match lst with
				| Int i -> Int i
				| List [] -> new_lst
				| List (head :: tail) -> 
					reverse_super_list_to_new_list 
						(List tail)
						(concat_numbers (reverse_super_list_to_new_list head (List [])) new_lst)
;;

 let rec reverse_super_list lst = 
		 match lst with
			 | Int i -> failwith "Failed to reverse the Int"
			 | List l -> reverse_super_list_to_new_list (List l) (List [])
 ;;

let rec print_super_list slst =
		match slst with
			| Int i -> printf "%i" i
			| List l -> printf "[ "; print_list slst; printf "]\n"
 	and print_list lst =
		match lst with
			| Int i -> printf "%i " i
			| List [] -> printf ""
			| List (head :: tail) ->
					if is_list head then printf "[ ";
					print_list head;
					if is_list head then printf "] ";
					print_list (List tail)
	and is_list slst =
		match slst with
			| Int i -> false
			| List l -> true
;;


let get_list = 
	let rec get_next_bracket_index indx number_of_inner_brackets =
		if Sys.argv.(indx) = "[" then
			if number_of_inner_brackets > 0 then get_next_bracket_index (indx - 1) (number_of_inner_brackets - 1)
			else indx
		else if Sys.argv.(indx) = "]" then get_next_bracket_index (indx - 1) (number_of_inner_brackets + 1)
		else get_next_bracket_index (indx - 1) number_of_inner_brackets
	in
	let rec get_list_from_command_line lst indx =
		if indx > 0 then
			let command_line_argument = Sys.argv.(indx) in
			if command_line_argument = "[" then
				lst
			else if command_line_argument = "]" then
				let sub_list = (List (get_list_from_command_line [] (indx - 1))) :: lst in 
				get_list_from_command_line sub_list ((get_next_bracket_index (indx - 1) 0) - 1)
			else
				get_list_from_command_line ((Int (int_of_string command_line_argument)) :: lst) (indx - 1)
		else lst
	in
	if (Array.length Sys.argv) = 1 then
		[Int 0; List [Int 1; Int 2]; Int 3; Int 4; List [List [Int 5; Int 6]; Int 7]] (* test example *)
	else
		get_list_from_command_line [] (Array.length Sys.argv - 1)
;;

printf "Your list:\t\t";;
print_super_list (List (get_list));;
printf "Your list reversed:\t";;
print_super_list (reverse_super_list (List (get_list)));;