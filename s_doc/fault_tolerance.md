Fault tolerance adalah salah satu fitur utama dari Erlang, yang memungkinkan sistem Erlang untuk tetap beroperasi bahkan saat terjadi kesalahan (error). Hal ini dicapai dengan menggunakan konsep pemrograman yang disebut "Let it crash", di mana sistem Erlang membiarkan proses yang mengalami kesalahan untuk gagal dan kemudian memulihkan diri atau di-restart jika perlu, sementara proses lain dalam sistem terus berjalan.

Berikut adalah contoh sederhana sebuah program Erlang yang menunjukkan bagaimana Erlang menangani fault tolerance dengan menggunakan supervisor:

```erlang

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
```
Dalam contoh ini, terdapat dua komponen utama: supervisor (fault_tolerance) dan worker (fault_tolerance_worker). Supervisor adalah proses yang bertanggung jawab untuk mengelola proses worker. Supervisor menggunakan strategi "one_for_one", yang berarti jika salah satu dari proses di bawah pengawasnya gagal, hanya proses tersebut yang akan di-restart.

Worker adalah proses yang bekerja di bawah pengawas. Dalam contoh ini, worker melakukan tugas sederhana yang diwakili oleh pemanggilan ke fungsi loop/1. Jika worker menerima pesan crash, itu akan menghasilkan kesalahan dan kemudian di-restart oleh supervisor.

Untuk menjalankan contoh ini, Anda bisa menggunakan shell Erlang dan memanggil fault_tolerance:start_link().. Kemudian, Anda dapat mengirim pesan ke worker untuk melihat bagaimana supervisor menangani kesalahan:

```
{ok, Pid} = fault_tolerance_worker:start_link(arg),
Pid ! crash.
```
Anda akan melihat bahwa meskipun worker mengalami kesalahan, supervisor berhasil memulihkannya dengan meng-restart proses tersebut. Ini adalah contoh sederhana dari bagaimana Erlang memungkinkan untuk sistem yang fault tolerant dan dapat terus beroperasi bahkan saat terjadi kesalahan.