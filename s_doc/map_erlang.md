# Map

Di Erlang, tipe data map adalah struktur data yang sangat fleksibel dan kuat untuk menyimpan pasangan kunci-nilai, di mana setiap kunci unik terkait dengan sebuah nilai. Maps diperkenalkan di Erlang versi 17, memberikan cara yang lebih dinamis dan ekspresif untuk bekerja dengan data terstruktur dibandingkan dengan record yang lebih statis dan harus didefinisikan terlebih dahulu.
Membuat Map

Untuk membuat sebuah map, Anda menggunakan sintaks {} dengan elemen-elemen di dalamnya, di mana setiap elemen adalah sebuah pasangan kunci => nilai. Kunci dan nilai dapat berupa tipe data apa pun.

```erlang

Map1 = #{key1 => value1, key2 => value2}.
```
Contoh

Berikut adalah beberapa contoh sederhana penggunaan map di Erlang:

1. Membuat dan Mengakses Map

```erlang

% Membuat map
Map = #{name => "John Doe", age => 30, city => "New York"}.

% Mengakses nilai menggunakan kunci
Name = Map#{name => _}.
Age = Map#{age => _}.

io:format("Name: ~p, Age: ~p~n", [Name, Age]).
```
2. Menambah atau Memperbarui Nilai

Untuk menambah atau memperbarui nilai dalam map, Anda bisa menggunakan sintaks maps:put/3 atau Map#{Key => Value} untuk pembaruan.

```erlang

% Menambah/memperbarui nilai
Map2 = Map#{city => "Los Angeles"}.

% Menambahkan nilai baru
Map3 = Map2#{country => "USA"}.

io:format("~p~n", [Map3]).
```
3. Menghapus Kunci

Untuk menghapus kunci (dan nilai terkait) dari map, gunakan maps:remove/2.

```erlang

% Menghapus kunci
Map4 = maps:remove(city, Map3).

io:format("~p~n", [Map4]).
```
4. Iterasi Atas Map

Anda dapat menggunakan maps:fold/3 untuk mengiterasi atas map, melakukan operasi pada setiap pasangan kunci-nilai.

Makeup.Styles.HTML.StyleMap.abap_style()
```erlang

% Iterasi atas map dan cetak setiap kunci-nilai
maps:fold(fun(Key, Value, Acc) ->
              io:format("Key: ~p, Value: ~p~n", [Key, Value]),
              Acc
          end, ok, Map).
```
Maps sangat berguna untuk kasus-kasus di mana Anda perlu struktur data yang mudah diubah dan diakses secara dinamis dengan kunci yang berbeda-beda. Erlang menyediakan banyak fungsi bawaan dalam modul maps untuk bekerja dengan struktur data ini, termasuk pencarian, penambahan, pembaruan, dan penghapusan elemen, serta iterasi dan transformasi.