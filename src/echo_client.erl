-module(echo_client).
-export([send/1]).

send(Msg) ->
    {ok, Pid} = echo_server:start_link(),
    gen_server:call(Pid, {send_echo, Msg}).
