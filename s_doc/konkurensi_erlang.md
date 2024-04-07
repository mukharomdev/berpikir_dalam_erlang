# Konkurensi 

Konkurensi merupakan salah satu fitur utama dan kekuatan Erlang, di mana setiap proses berjalan secara independen dan dapat berkomunikasi melalui pengiriman pesan. Di bawah ini adalah contoh sederhana yang menggambarkan bagaimana konkurensi bekerja di Erlang. Kita akan membuat dua proses: satu proses mengirim pesan kepada yang lain, dan proses penerima akan mencetak pesan tersebut ke konsol.
Langkah 1: Definisikan Modul dan Fungsi Ekspor

Pertama, kita buat file dengan nama concurrency_example.erl dan mendefinisikan modul serta fungsi-fungsi yang perlu diekspor.

```erlang

-module(concurrency_example).
-export([start/0, sender/1, receiver/0]).
```
Langkah 2: Fungsi Receiver

Fungsi receiver/0 menunggu pesan dan mencetak pesan yang diterima. Penggunaan receive memungkinkan proses untuk menunggu pesan.

```erlang

receiver() ->
    receive
        {From, Message} ->
            io:format("Pesan diterima dari ~p: ~s~n", [From, Message]),
            receiver() % Loop untuk menerima pesan lebih lanjut
    end.
```
Langkah 3: Fungsi Sender

Fungsi sender/1 mengirim pesan ke proses yang ditentukan. Parameter adalah PID (Process Identifier) dari proses penerima.

```erlang

sender(ReceiverPid) ->
    Message = "Halo dari sender!",
    ReceiverPid ! {self(), Message},
    ok.
```
Langkah 4: Fungsi Start

Fungsi start/0 menginisialisasi proses-proses. Ini membuat proses receiver, mendapatkan PID-nya, dan kemudian memulai proses sender dengan mengirimkan PID receiver sebagai argumen.

```erlang

start() ->
    ReceiverPid = spawn(concurrency_example, receiver, []),
    spawn(concurrency_example, sender, [ReceiverPid]).
```
Langkah 5: Kompilasi dan Jalankan

    Kompilasi Kode

    Buka terminal, navigasikan ke direktori tempat file concurrency_example.erl berada, dan kompilasi menggunakan erlc concurrency_example.erl.

    Jalankan di Erlang Shell

    Buka Erlang shell dengan mengetik erl di terminal, lalu jalankan fungsi start/0 dengan:

```erlang

    c(concurrency_example).
    concurrency_example:start().
```
Setelah menjalankan fungsi start/0, Anda akan melihat bahwa proses receiver mencetak pesan yang dikirim oleh proses sender. Ini menunjukkan bagaimana dua proses dapat berkomunikasi secara konkuren dalam Erlang melalui mekanisme pengiriman pesan.

Contoh ini adalah ilustrasi dasar dari konkurensi di Erlang, menunjukkan bagaimana mudahnya untuk membuat dan mengelola proses yang berjalan secara paralel dan berkomunikasi satu sama lain. Fitur konkurensi ini membuat Erlang menjadi pilihan yang sangat baik untuk sistem real-time, aplikasi distribusi, dan layanan yang membutuhkan skalabilitas tinggi dan keandalan.