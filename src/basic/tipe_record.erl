-module (tipe_record).

-export ([start/0]).

-record (person, {nama,umur}).


start() ->
		Person = #person{nama="yodha",umur = 15},

		io:format("Record : ~p~n",[Person#person.nama]).