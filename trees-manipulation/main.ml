type 'a bin_tree = EmptyTreeNode | TreeNode of 'a bin_tree * 'a * 'a bin_tree;;

type 'a tree_node = Empty | Node of 'a;;

let rec log2n n =
	if n > 1 then 
		1 + log2n (n / 2)
	else
		0
;;

let pow value power =
	int_of_float ((float_of_int value) ** (float_of_int power))
;;

let print_node node =
	match node with
	| Empty -> print_string "E"
	| Node value -> print_int value
;;

let create_leaf height =
	Array.make ((pow 2 (height + 1)) - 1) Empty
;;

let create_tree label left_tree right_tree =
	Array.concat [(Array.make 1 (Node label)); left_tree; right_tree]
;;

let rec get_height btree =
			get_height_with_start_height btree 0
	and
		get_height_with_start_height btree height =
			match btree with
			| EmptyTreeNode -> height
			| TreeNode (lb, r, rb) ->
				max (get_height_with_start_height lb (height + 1))
					(get_height_with_start_height rb (height + 1))
;;

let rec array_of_bin_tree btree = 
			array_of_bin_tree_with_height btree (get_height btree) 
	and
		array_of_bin_tree_with_height btree height =
			match btree with
			| EmptyTreeNode -> create_leaf height				
			| TreeNode (lb, r, rb) -> 
				create_tree r 
							(array_of_bin_tree_with_height lb (height - 1)) 
							(array_of_bin_tree_with_height rb (height - 1))
;;

let rec fill_array_with_bin_tree btree arr =
			fill_array btree arr (get_height btree) 0 (((Array.length arr) + 1) / 2)
	and 
		fill_array btree arr height cur_pos middle_of_subarray = 
			match btree with
			| EmptyTreeNode -> 
				let leaf = create_leaf height in
				Array.blit leaf 0 arr cur_pos (Array.length leaf)
			| TreeNode (lb, r, rb) -> 
				arr.(cur_pos) <- Node r;
				fill_array lb arr (height - 1) (cur_pos + 1) (middle_of_subarray / 2);
				fill_array rb arr (height - 1) (cur_pos + middle_of_subarray) (middle_of_subarray / 2)
;;

let rec infix_traversal tree func_to_run =
			let size = Array.length tree in
			let log_res = log2n (size + 1) in
			let height = log_res - 1 in
			print_tree_with_level tree (height - 1) func_to_run
	and
		print_tree_with_level tree level_of_deep func_to_run =
			let size = Array.length tree in
			let sub_length = size / 2 in

			if size > 1 then
			(
				print_tree_with_level (Array.sub tree 1 sub_length) (level_of_deep - 1) func_to_run;
				func_to_run tree.(0) level_of_deep;
				print_tree_with_level (Array.sub tree (sub_length + 1) sub_length) (level_of_deep - 1) func_to_run;
			)
;;

let print_tree tree tree_name =
	print_string tree_name;
	print_string "\n\n\n";
	infix_traversal 
		tree 
		(
			fun node level_of_deep -> 
				if node <> Empty then
				(
					for i = 0 to level_of_deep - 1 do
						print_string "\t"
					done;
					print_node node;
					print_string "\n";
				)
		);	
	print_string "\n\n\n";
;;


let example_balanced = 
	TreeNode (
		TreeNode (
			TreeNode (
				EmptyTreeNode,
				1,
				EmptyTreeNode
			),
			2,
			TreeNode (
				EmptyTreeNode,
				3,
				EmptyTreeNode
			)
		),
		4,
		TreeNode (
			TreeNode (
				EmptyTreeNode,
				5,
				EmptyTreeNode
			),
			6,
			TreeNode (
				EmptyTreeNode,
				7,
				EmptyTreeNode
			)
		)
	)
;;

let example_disbalanced =
	TreeNode (
		TreeNode (
			TreeNode (
				EmptyTreeNode,
				1,
				EmptyTreeNode
			),
			2,
			EmptyTreeNode
		),
		4,
		TreeNode (
			TreeNode (
				TreeNode (
					EmptyTreeNode,
					5,
					EmptyTreeNode
				),
				6,
				EmptyTreeNode
			),
			7,
			TreeNode (
				EmptyTreeNode,
				8,
				EmptyTreeNode
			)
		)
	)
;;

print_tree (array_of_bin_tree example_balanced) "balanced example";;
print_tree (array_of_bin_tree example_disbalanced) "disbalanced example";;