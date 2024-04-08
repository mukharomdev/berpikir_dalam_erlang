#Hot Code Swapping

Hot Code Swapping adalah kemampuan dalam Erlang untuk mengganti kode program yang sedang berjalan tanpa mempengaruhi proses yang sedang berjalan atau menghentikan sistem secara keseluruhan. Ini adalah salah satu fitur utama dari Erlang yang memungkinkan aplikasi untuk diperbarui secara dinamis tanpa waktu henti yang signifikan.

Berikut ini adalah contoh sederhana penggunaan Hot Code Swapping di Erlang:

Kita akan membuat sebuah module sederhana yang memiliki sebuah fungsi bernama hello_world/0:

erlang

-module(hello_world).
-export([hello_world/0]).

hello_world() ->
    io:format("Hello, World!~n").

Kemudian kita akan meng-compile modul ini dan memulainya dalam shell Erlang:

erlang

c(hello_world).
hello_world:hello_world().

Setelah itu, kita akan memperbarui modul hello_world dengan versi baru yang memiliki perubahan. Mari kita tambahkan pesan sapaan yang baru:

erlang

-module(hello_world).
-export([hello_world/0]).

hello_world() ->
    io:format("Hello, Erlang!~n").

Kemudian, kita akan meng-compile modul ini dan melakukan hot code swapping di shell Erlang:

erlang

c(hello_world).
l(hello_world).
hello_world:hello_world().

Dengan melakukan itu, modul hello_world telah diperbarui tanpa perlu menghentikan atau memulai kembali shell Erlang atau proses apa pun yang sedang berjalan di dalamnya. Hal ini memungkinkan sistem yang berjalan secara real-time untuk diperbarui dengan cepat dan mudah tanpa mengganggu ketersediaan atau performa aplikasi.