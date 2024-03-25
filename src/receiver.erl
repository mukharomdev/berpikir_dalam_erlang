-module(receiver).
-export([start/0, loop/0]).

start() ->
    register(receiver, spawn(fun loop/0)).

loop() ->
    receive
        {From, Message} ->
            io:format("Received: ~p~n", [Message]),
            From ! {thanks, "Thanks for the message!"}
    end,
    loop().
