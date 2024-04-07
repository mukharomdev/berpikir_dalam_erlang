# Membuat Aplikasi Konsole di Erlang

Mari kita buat sebuah contoh aplikasi konsol sederhana di Erlang yang berfungsi sebagai penghitung kata. Aplikasi ini akan meminta pengguna untuk memasukkan sebuah string melalui konsol, menghitung jumlah kata dalam string tersebut, dan kemudian menampilkan hasilnya.

Berikut adalah langkah-langkah dan kode untuk membuat aplikasi penghitung kata:

1. Definisikan Modul dan Fungsi Utama

Pertama, buat sebuah file dengan nama word_counter.erl. Di dalam file ini, kita akan mendefinisikan modul dan fungsi-fungsi yang diperlukan.

```erlang 

-module(word_counter).
-export([start/0, read_input/0, count_words/1, display_result/1]).
```
2. Fungsi untuk Memulai Aplikasi

Fungsi start/0 akan memulai aplikasi kita. Fungsi ini akan memanggil fungsi read_input/0 untuk meminta masukan dari pengguna.

```erlang

start() ->
    io:format("Masukkan sebuah kalimat: "),
    Input = read_input(),
    Words = count_words(Input),
    display_result(Words).
```
3. Fungsi untuk Membaca Masukan

Fungsi read_input/0 menggunakan io:get_line/1 untuk membaca masukan dari pengguna melalui konsol.

```erlang

read_input() ->
    io:get_line("").
```
4. Fungsi untuk Menghitung Kata

Fungsi count_words/1 menerima string masukan, memisahkannya menjadi daftar kata berdasarkan spasi, dan menghitung jumlah kata.

```erlang

count_words(Input) ->
    Words = string:split(string:trim(Input), " ", all),
    length(Words).
```
5. Fungsi untuk Menampilkan Hasil

Fungsi display_result/1 menampilkan jumlah kata yang telah dihitung.

```erlang

    display_result(Words) ->
        io:format("Jumlah kata: ~p~n", [Words]).
```
    Menjalankan Aplikasi

    Untuk menjalankan aplikasi ini, buka terminal atau command prompt dan lakukan langkah-langkah berikut:

    a. Compile file word_counter.erl dengan menggunakan perintah erlc word_counter.erl.

    b. Buka Erlang shell dengan mengetik erl.

    c. Jalankan aplikasi dengan mengetik word_counter:start(). di Erlang shell.

    d. Masukkan kalimat yang ingin Anda hitung kata-katanya ketika diminta, lalu tekan enter.

Contoh aplikasi ini menggambarkan bagaimana Erlang dapat digunakan untuk membuat aplikasi konsol sederhana yang interaktif. Aplikasi ini bisa dijadikan sebagai dasar untuk membuat aplikasi konsol yang lebih kompleks dengan Erlang.