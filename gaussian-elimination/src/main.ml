open Printf

let get_first_example_system =
	let mat_ex = Mat.create_mat 4 in
	let vec_ex = Vec.create_vec 4 in

	mat_ex.Mat.m_matrix.(0) <- [| 10.;  7.;  8.;  7.|];
	mat_ex.Mat.m_matrix.(1) <- [|  7.;  5.;  6.;  5.|];
	mat_ex.Mat.m_matrix.(2) <- [|  8.;  6.; 10.;  9.|];
	mat_ex.Mat.m_matrix.(3) <- [|  7.;  5.;  9.; 10.|];

	Vec.mod_vec vec_ex 0 32.;
	Vec.mod_vec vec_ex 1 23.;
	Vec.mod_vec vec_ex 2 33.;
	Vec.mod_vec vec_ex 3 31.;

	{ Syst.m_system_matrix = mat_ex; Syst.m_system_vector = vec_ex }
;;

let get_second_example_system =
	let mat_ex = Mat.create_mat 4 in
	let vec_ex = Vec.create_vec 4 in

	mat_ex.Mat.m_matrix.(0) <- [| 10.;  7.;  8.;  7.|];
	mat_ex.Mat.m_matrix.(1) <- [|  7.;  5.;  6.;  5.|];
	mat_ex.Mat.m_matrix.(2) <- [|  8.;  6.; 10.;  9.|];
	mat_ex.Mat.m_matrix.(3) <- [|  7.;  5.;  9.; 10.|];

	Vec.mod_vec vec_ex 0 32.1;
	Vec.mod_vec vec_ex 1 23.9;
	Vec.mod_vec vec_ex 2 33.1;
	Vec.mod_vec vec_ex 3 31.9;

	{ Syst.m_system_matrix = mat_ex; Syst.m_system_vector = vec_ex }
;;

let get_third_example_system =
	let mat_ex = Mat.create_mat 4 in
	let vec_ex = Vec.create_vec 4 in

	mat_ex.Mat.m_matrix.(0) <- [| 10.;    7.;    8.1;  7.2|];
	mat_ex.Mat.m_matrix.(1) <- [|  7.08;  5.04;  6.;   5.|];
	mat_ex.Mat.m_matrix.(2) <- [|  8.;    5.98;  9.89; 9.|];
	mat_ex.Mat.m_matrix.(3) <- [|  6.99;  4.99;  9.;   9.98|];

	Vec.mod_vec vec_ex 0 32.;
	Vec.mod_vec vec_ex 1 23.;
	Vec.mod_vec vec_ex 2 33.;
	Vec.mod_vec vec_ex 3 31.;

	{ Syst.m_system_matrix = mat_ex; Syst.m_system_vector = vec_ex }
;;

let main =
	printf "\n\n-----First example-----\n\n";
	Syst.solve_by_gauss_elimination get_first_example_system;

	printf "\n\n-----Second example-----\n\n";
	Syst.solve_by_gauss_elimination get_second_example_system;

	printf "\n\n-----Third example-----\n\n";
	Syst.solve_by_gauss_elimination get_third_example_system;
;;