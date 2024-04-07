# Sistem Terdistribusi

Distribusi di Erlang memungkinkan aplikasi berjalan di lebih dari satu node Erlang yang dapat berkomunikasi satu sama lain melalui jaringan. Ini sangat berguna untuk membangun sistem terdistribusi yang skalabel dan dapat diandalkan. Sebagai contoh, mari kita buat aplikasi sederhana yang terdiri dari dua node: satu node mengirim pesan ke node lainnya.
Persiapan

Sebelum kita memulai, pastikan Erlang sudah terinstal di sistem Anda. Untuk menjalankan sistem terdistribusi Erlang, Anda perlu memulai Erlang dengan nama node yang diberikan dan pastikan kedua node tersebut bisa saling berkomunikasi.

Buka dua terminal, satu untuk setiap node Erlang. Jalankan Erlang dengan nama node yang berbeda pada setiap terminal:

Terminal 1:


```
erl -name node1@localhost -setcookie abc
```
Terminal 2:

```

erl -name node2@localhost -setcookie abc
```

Kedua node menggunakan cookie yang sama (abc) untuk autentikasi.
Langkah 1: Definisikan Modul dan Fungsi

Pada node pengirim, kita akan mendefinisikan sebuah modul sender dengan fungsi untuk mengirim pesan ke node penerima.
Pada Node Pengirim (Terminal 1)
```erlang

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
```
Pada Node Penerima (Terminal 2)

Di node penerima, kita mendefinisikan modul receiver yang memulai proses yang menunggu pesan dan membalasnya.

```erlang

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
```
Langkah 2: Kompilasi dan Jalankan Kode

    Pada Node Penerima (Terminal 2)

    Kompilasi receiver dan jalankan fungsi start/0:

    erlang

c(receiver).
receiver:start().

Pada Node Pengirim (Terminal 1)

Kompilasi sender dan kirim pesan ke receiver di node lain menggunakan fungsi send/1. Gantikan NodeName dengan nama node penerima yang tepat (dalam kasus ini node2@localhost):

erlang

    c(sender).
    sender:send('node2@localhost').

Anda seharusnya melihat pesan yang dikirim dari node pengirim ke penerima, dan penerima membalas pesan tersebut. Demonstrasi ini menunjukkan dasar-dasar komunikasi antar-node dalam aplikasi Erlang yang terdistribusi. Erlang menjadikan pengembangan sistem terdistribusi lebih mudah dengan model pemrograman berbasis pesan yang kuat dan kemampuan untuk transparan mengelola proses di berbagai node.