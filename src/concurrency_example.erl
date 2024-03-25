-module(concurrency_example).
-export([start/0, sender/1, receiver/0]).

start() ->
    ReceiverPid = spawn(concurrency_example, receiver, []),
    spawn(concurrency_example, sender, [ReceiverPid]).

receiver() ->
    receive
        {From, Message} ->
            io:format("Pesan diterima dari ~p: ~s~n", [From, Message]),
            receiver() % Loop untuk menerima pesan lebih lanjut
    end.

sender(ReceiverPid) ->
    Message = "Halo dari sender!",
    ReceiverPid ! {self(), Message},
    ok.


