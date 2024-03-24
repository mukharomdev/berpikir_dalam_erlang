%%%-------------------------------------------------------------------
%% @doc berpikir_dalam_erlang public API
%% @end
%%%-------------------------------------------------------------------

-module(berpikir_dalam_erlang_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    berpikir_dalam_erlang_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
