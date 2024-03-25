-module(sender).
-export([send/1]).

send(ReceiverNode) ->
    %% Pastikan node terhubung
    net_adm:ping(ReceiverNode),
    %% Kirim pesan ke proses receiver di node lain
    {receiver, ReceiverNode} ! {self(), "Hello from sender"},
    receive
        Reply -> io:format("Received reply: ~p~n", [Reply])
    after 5000 ->
        io:format("No reply received~n")
    end.
