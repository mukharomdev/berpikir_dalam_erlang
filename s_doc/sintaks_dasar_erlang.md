# Sintaks Dasar Erlang

Erlang memiliki sintaks yang cukup unik dan ringkas yang mungkin terlihat asing bagi mereka yang terbiasa dengan bahasa pemrograman lain. Berikut adalah beberapa sintaks dasar dan konsep penting dalam Erlang untuk membantu Anda memulai:

1. Variabel

- Variabel di Erlang harus diawali dengan huruf kapital.

- Variabel bersifat immutable, artinya sekali nilai telah di-assign, nilai tersebut tidak bisa diubah.

```erlang

Name = "Erlang".
Count = 10.
```
2. Atom

Atom adalah konstanta yang namanya merupakan nilainya sendiri. Atom diawali dengan huruf kecil atau diapit dengan tanda petik ' jika mengandung spasi atau karakter khusus.

```erlang

atom.
'another atom'.
```

3. Tuples

Tuple digunakan untuk menyimpan sejumlah tetap elemen. Elemen-elemen dalam tuple dapat berupa tipe data apapun. Tuple ditulis dengan menggunakan kurung kurawal {}.

```erlang

{ok, 200, "OK"}.
{error, "NotFound"}.
```

4. Lists

    List digunakan untuk menyimpan sejumlah variabel elemen. List ditulis dengan menggunakan kurung siku [].

```erlang

[1, 2, 3, 4, 5].
["erlang", "elixir", "otp"].
```

5. String

String di Erlang dinyatakan sebagai list dari karakter-karakter.

```erlang

"Hello, World!".
```

6. Fungsi

Fungsi didefinisikan dalam modul. Setiap fungsi diawali dengan kata kunci -export jika ingin diakses dari luar modul.

```erlang

-module(example).
-export([say_hello/0]).

say_hello() ->
    io:format("Hello, World!~n").
```
7. Percabangan

Erlang menggunakan case...of untuk percabangan dan juga guard clauses untuk kondisi lebih kompleks.

```erlang

case Variable of
    pattern1 when Guard1 -> Expression1;
    pattern2 -> Expression2;
    _ -> DefaultExpression
end.
```
8. Pengulangan

Erlang tidak memiliki konstruksi pengulangan tradisional seperti for atau while. Sebagai gantinya, pengulangan dicapai melalui rekursi.

```erlang

loop(0) -> ok;
loop(N) ->
    io:format("Loop iteration: ~p~n", [N]),
    loop(N-1).
```

9. Concurrent Programming

Erlang sangat baik dalam menangani pemrograman konkuren. Untuk membuat proses, Anda menggunakan fungsi spawn.

```erlang

spawn(module, function_name, [Arg1, Arg2, ...]).
```

10. Komentar

Komentar di Erlang diawali dengan tanda %.

```erlang

% Ini adalah komentar
```
Ini adalah pengenalan dasar terhadap sintaks Erlang. Erlang memiliki banyak fitur lanjutan, terutama terkait dengan pemrograman fungsional dan konkuren, yang membuatnya sangat cocok untuk sistem berskala besar dan aplikasi real-time.