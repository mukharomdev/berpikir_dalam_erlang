# Membangun Sistem 

Erlang adalah bahasa pemrograman yang dirancang untuk membangun sistem yang bersifat concurrent, fault-tolerant, dan scalable. Erlang sering digunakan dalam industri telekomunikasi, finansial, dan sistem pesan real-time karena kemampuannya yang unik dalam menangani ratusan ribu proses bersamaan dengan overhead yang rendah.

Berikut adalah contoh sederhana dari sebuah sistem Erlang. Contoh ini melibatkan pembuatan server sederhana yang dapat menangani permintaan dari klien. Server ini akan menerima pesan, mencetak pesan tersebut ke console, dan mengirimkan balasan ke klien.

1. Definisi Modul Server

```erlang

-module(echo_server).
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #{}}. % state is just an empty map

handle_call({send_echo, Msg}, _From, State) ->
    io:format("Received message: ~p~n", [Msg]),
    {reply, {echo, Msg}, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
```
2. Definisi Modul Klien

```erlang

-module(echo_client).
-export([send/1]).

send(Msg) ->
    {ok, Pid} = echo_server:start_link(),
    gen_server:call(Pid, {send_echo, Msg}).
```
Cara Menjalankan:

    Kompilasi Kode: Pertama, Anda perlu mengkompilasi kedua modul tersebut dengan menggunakan perintah c(Module). di shell Erlang.

```shell

1> c(echo_server).
2> c(echo_client).
```
Menggunakan Klien: Setelah kedua modul berhasil dikompilasi, Anda dapat menggunakan fungsi send/1 dari modul echo_client untuk mengirim pesan ke server.

```shell

    3> echo_client:send("Hello, Erlang!").
```
    Di sisi server, Anda akan melihat pesan "Received message: 'Hello, Erlang!'" dicetak ke console, dan klien akan menerima balasan {echo, "Hello, Erlang!"}.

Contoh di atas hanya permulaan dari apa yang bisa dilakukan dengan Erlang dalam membangun sistem yang bersifat concurrent dan fault-tolerant. Erlang memiliki ekosistem yang luas dengan berbagai library dan tools yang mendukung pembangunan sistem yang kompleks.
