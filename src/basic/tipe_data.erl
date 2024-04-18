-module (tipe_data).

-export ([start/0]).



start() ->
% integer
		Int = 4,
		io:format("integer : ~p ~n",[Int]),
% float
		Float = 2.31,
		io:format("float : ~p ~n",[Float]),
% atom
		% atom (diawali dengan huruf kecil)
		io:format("atom : ~p ~n",[atom]),
% boolean
		io:fwrite("boolean : ~p ~n",[ 2 =< 3]),
% bit string
		Bin1 = <<10,20>>,
   		X = binary_to_list(Bin1),
		io:fwrite("bit string: ~p ~n",[ X ]),
% tuple

		io:fwrite("tuple : ~p ~n",[ {3,5,"oke",3.14}]),

% Map
		Yodha = #{name=>"yodha",kelas=>2},
		io:format("map : ~p ~n",[Yodha]),

% list

		io:format("list : ~p ~n",[[3,6]]).




