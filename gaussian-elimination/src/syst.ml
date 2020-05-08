open Printf

type syst = { m_system_matrix: Mat.mat; m_system_vector: Vec.vec };;

let create_syst size = { m_system_matrix = Mat.create_mat size; m_system_vector = Vec.create_vec size };;

let array_swap arr first_index second_index =
	let temp = arr.(first_index) in
	arr.(first_index) <- arr.(second_index);
	arr.(second_index) <- temp;
;;

let arrays_index_swap first_arr second_arr index =
	let temp = first_arr.(index) in
	first_arr.(index) <- second_arr.(index);
	second_arr.(index) <- temp;
;;

let system_rows_swap system first_row_index second_row_index =
	let size = system.m_system_matrix.Mat.m_matrix_size in
	let first_row_array = system.m_system_matrix.Mat.m_matrix.(first_row_index) in
	let second_row_array = system.m_system_matrix.Mat.m_matrix.(second_row_index) in
	for i = 0 to size - 1 do
		arrays_index_swap first_row_array second_row_array i
	done;
	array_swap system.m_system_vector.Vec.m_vector first_row_index second_row_index
;;

let div_syst_row_by_pivot system i =
	let size = system.m_system_matrix.Mat.m_matrix_size in
	let divider = 1. /. (Mat.access_mat system.m_system_matrix i i) in
	let system_row = { Vec.m_vector_size = size; Vec.m_vector = system.m_system_matrix.Mat.m_matrix.(i) } in (* by reference *)
	Mat.mod_mat_row system.m_system_matrix i (Vec.mult_vec_by_scalar system_row divider);
	Vec.mod_vec system.m_system_vector i ((Vec.access_vec system.m_system_vector i) *. divider)
;;

let diagonalize_system system =
	let size = system.m_system_matrix.Mat.m_matrix_size in
	for i = 0 to size - 1 do
		div_syst_row_by_pivot system i;
		let primary_mat_row = Vec.{ Vec.m_vector_size = size; Vec.m_vector = system.m_system_matrix.Mat.m_matrix.(i) } in (* by reference *)
		let primary_vec_value = system.m_system_vector.Vec.m_vector.(i) in
		for j = i + 1 to size - 1 do
			let secondary_mat_row = { Vec.m_vector_size = size; Vec.m_vector = system.m_system_matrix.Mat.m_matrix.(j) } in (* by reference *)
			let secondary_vec_value = system.m_system_vector.Vec.m_vector.(j) in
			let scalar = (-1.) *. (Vec.access_vec secondary_mat_row i) in

			let temp_row_to_add = Vec.mult_vec_by_scalar primary_mat_row scalar in
			let result_row = Vec.add_vec temp_row_to_add secondary_mat_row in
			Mat.mod_mat_row system.m_system_matrix j result_row;

			Vec.mod_vec system.m_system_vector j (primary_vec_value *. scalar +. secondary_vec_value)
	    done;
	done
;;

let solution_from_diagonalized_system system =
	let size = system.m_system_matrix.Mat.m_matrix_size in
	let solution = Vec.create_vec size in
	for i = size - 1 downto 0 do
		let sum = ref 0. in
		for j = i + 1 to size - 1 do
			sum := !sum +. ((Mat.access_mat system.m_system_matrix i j) *. (Vec.access_vec solution j)) 
		done;
		let dividend = (Vec.access_vec system.m_system_vector i) -. !sum in
		Vec.mod_vec solution i (dividend /. (Mat.access_mat system.m_system_matrix i i))
	done;
	solution
;;

let print_system system =
	let size = system.m_system_matrix.Mat.m_matrix_size in
	for i = 0 to size - 1 do
		printf "( ";
		for j = 0 to size - 1 do
			printf "%5.2f " (Mat.access_mat system.m_system_matrix i j)
		done;
		printf "| %5.2f )\n" (Vec.access_vec system.m_system_vector i)
	done
;;

let solve_by_gauss_elimination system =
	printf "System:\n";
	print_system system;

	diagonalize_system system;
	printf "\nDialonalized system:\n";
	print_system system;

	let system_solution = solution_from_diagonalized_system system in
	printf "\nSolution:\n";
	Vec.print_vector system_solution;
;;