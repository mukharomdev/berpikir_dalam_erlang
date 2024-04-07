-module(fault_tolerance).
-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([worker/1, supervisor_spec/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 5, 10}, supervisor_spec()}}.

supervisor_spec() ->
    {ok, {{simple_one_for_one, 3, 3600}, [
        {worker, {fault_tolerance, worker}, transient, 1000, worker, [arg]}
    ]}}.

worker(Arg) ->
    io:format("Worker ~p starting~n", [Arg]),
    % Simulate a potential crash
    case Arg of
        crash -> exit(crash);
        _ -> ok
    end,
    loop(Arg).

loop(Arg) ->
    io:format("Worker ~p running~n", [Arg]),
    receive
        {stop, From} ->
            io:format("Worker ~p stopping~n", [Arg]),
            From ! stopped,
            ok;
        {restart, From} ->
            io:format("Worker ~p restarting~n", [Arg]),
            From ! restarted,
            loop(Arg);
        _ ->
            loop(Arg)
    end.
