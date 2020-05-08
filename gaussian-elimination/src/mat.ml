type mat = { m_matrix_size: int; m_matrix: float array array };;

let create_mat size = { m_matrix_size = size; m_matrix = Array.make_matrix size size 0. };;
let access_mat matrix i j = matrix.m_matrix.(i).(j);;
let mod_mat matrix i j new_value = matrix.m_matrix.(i).(j) <- new_value;;
let mod_mat_row matrix i new_vector = matrix.m_matrix.(i) <- new_vector.Vec.m_vector;;

let mult_mat first_mat second_mat =
	if first_mat.m_matrix_size != second_mat.m_matrix_size then
		failwith "Failed to multiply matrices: dimensions incompatible"
	else
		let mat_size = first_mat.m_matrix_size in
		let result_mat = create_mat mat_size in
		for i = 0 to mat_size - 1 do
			for j = 0 to mat_size - 1 do
				let result_value = ref 0. in
				for k = 0 to mat_size - 1 do
					result_value := !result_value +. ((access_mat first_mat i k) *. (access_mat second_mat k j))
				done;
				mod_mat result_mat i j !result_value
			done
		done;
	result_mat
;;

let mult_mat_vec matrix vector =
	if matrix.m_matrix_size != vector.Vec.m_vector_size then
		failwith "Failed to multiply a matrix by vector: dimensions incompatible"
	else
		let size = matrix.m_matrix_size in
		let result_vec = Vec.create_vec size in
		for i = 0 to size - 1 do
			let result_value = ref 0. in
			for j = 0 to size - 1 do
				result_value := !result_value +. ((access_mat matrix i j) *. (Vec.access_vec vector j))
			done;
			Vec.mod_vec result_vec i !result_value
		done;
	result_vec
;;