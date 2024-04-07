-module(complex1).
-export([start/1, stop/0, init/1]).
-export([foo/1, bar/1]).

start(ExtPrg) ->
    spawn(?MODULE, init, [ExtPrg]).
stop() ->
    complex ! stop.

foo(X) ->
    call_port({foo, X}).
bar(Y) ->
    call_port({bar, Y}).

call_port(Msg) ->
    complex ! {call, self(), Msg},
    receive
	{complex, Result} ->
	    Result
    end.

init(ExtPrg) ->
    register(complex, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, [{packet, 2}]),
    loop(Port).

loop(Port) ->
    receive
	{call, Caller, Msg} ->
	    Port ! {self(), {command, encode(Msg)}},
	    receive
		{Port, {data, Data}} ->
		    Caller ! {complex, decode(Data)}
	    end,
	    loop(Port);
	stop ->
	    Port ! {self(), close},
	    receive
		{Port, closed} ->
		    exit(normal)
	    end;
	{'EXIT', Port, Reason} ->
	    exit(port_terminated)
    end.

encode({foo, X}) -> [1, X];
encode({bar, Y}) -> [2, Y].

decode([Int]) -> Int.



% Running the Example

% Step 1. Compile the C code:

% unix> gcc -o extprg complex.c erl_comm.c port.c

% Step 2. Start Erlang and compile the Erlang code:

% unix> erl
% Erlang (BEAM) emulator version 4.9.1.2

% Eshell V4.9.1.2 (abort with ^G)
% 1> c(complex1).
% {ok,complex1}

% Step 3. Run the example:

% 2> complex1:start("./extprg").
% <0.34.0>
% 3> complex1:foo(3).
% 4
% 4> complex1:bar(5).
% 10
% 5> complex1:stop().
% stop

