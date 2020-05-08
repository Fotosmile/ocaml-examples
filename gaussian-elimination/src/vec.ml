open Printf

type vec = { m_vector_size: int; m_vector: float array };;

let create_vec size = { m_vector_size = size; m_vector = Array.make size 0. };;
let access_vec vector i = vector.m_vector.(i);;
let mod_vec vector i new_value = vector.m_vector.(i) <- new_value;;

let add_vec first_vec second_vec =
	if first_vec.m_vector_size != second_vec.m_vector_size then
		failwith "Failed to add vectors: dimensions incompatible"
	else
		let vec_size = first_vec.m_vector_size in
		let result_vec = create_vec vec_size in
		for i = 0 to vec_size - 1 do
			mod_vec result_vec i ((access_vec first_vec i) +. (access_vec second_vec i))
		done;
	result_vec
;;

let mult_vec_by_scalar vector scalar =
	let vec_size = vector.m_vector_size in
	let result_vec = create_vec vec_size in
	for i = 0 to vec_size - 1 do
		mod_vec result_vec i ((access_vec vector i) *. scalar)
	done;
	result_vec
;;

let print_vector vector =
	let size = vector.m_vector_size in
	printf "( ";
	for i = 0 to size - 1 do
		printf "%5.2f " (access_vec vector i)
	done;
	printf " )\n";
;;