open Printf

let read_file filename = 
	let lines = ref [] in
	let chan = open_in filename in
	try
		while true; do
	    	lines := input_line chan :: !lines
	  	done; !lines
	with End_of_file ->
	  	close_in chan;
  		List.rev !lines
;;

let rec split_str_list str_list =
	_split_str_list str_list []
	and _split_str_list str_list result =
		match str_list with
		| [] -> List.rev result
		| head :: tail -> _split_str_list tail ((los_to_lof (Str.split (Str.regexp " ") head) []) :: result)
	and
		los_to_lof str_list result =
			match str_list with
			| [] -> List.rev result
			| head :: tail -> los_to_lof tail ((float_of_string head) :: result)
;;

let lists_to_arrays lists = Array.of_list (List.map Array.of_list lists);;

let arrays_to_system arrays =
	let size = Array.length arrays in
	let system = Syst.create_syst size in
	for i = 0 to size - 1 do
		for j = 0 to size - 1 do
			Mat.mod_mat system.Syst.m_system_matrix i j arrays.(i).(j)
		done;
		Vec.mod_vec system.Syst.m_system_vector i arrays.(i).(size)
	done;
	system
;;

let main =
	if (Array.length Sys.argv) < 2 then
		printf "Failed to find a file name\n"
	else
		let filename = Sys.argv.(1) in
		let system = arrays_to_system (lists_to_arrays (split_str_list (read_file filename))) in
		Syst.solve_by_gauss_elimination system;
;;