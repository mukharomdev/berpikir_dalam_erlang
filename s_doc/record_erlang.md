# Record 

Di Erlang, record adalah cara untuk mendefinisikan struktur data yang memiliki sejumlah field dengan nama. Record merupakan abstraksi di atas tuple, di mana posisi pertama dalam tuple digunakan untuk menyimpan nama record, yang memungkinkan Erlang untuk membedakan antara berbagai jenis record. Penggunaan record membuat kode lebih mudah dibaca dan ditulis dibandingkan dengan menggunakan tuple secara langsung, karena Anda dapat merujuk ke field berdasarkan nama bukan posisi.
Mendefinisikan Record

Record didefinisikan dengan direktif -record(Nama, {Field1, Field2, ..., FieldN}). di dalam module Erlang. Direktif ini biasanya ditempatkan di bagian atas file.

```erlang

-record(person, {name, age, city}).
```
## Membuat Instance Record

Untuk membuat instance dari sebuah record, Anda menggunakan sintaks #NamaRecord{field1=Value1, ...}.

```erlang

John = #person{name="John Doe", age=30, city="New York"}.
```
## Mengakses dan Memperbarui Field Record

Anda bisa mengakses field dalam record menggunakan sintaks Record#RecordName.FieldName.

```erlang

Name = John#person.name.
```
Untuk memperbarui field, Anda bisa menggunakan sintaks yang mirip dengan pembuatan instance, tapi kali ini Anda menggunakannya pada sebuah variable record yang sudah ada.

```erlang

JohnUpdated = John#person{city="Los Angeles"}.
```
Contoh Penggunaan Record

Berikut adalah contoh penggunaan record yang lengkap dalam sebuah module Erlang.

```erlang

-module(person_module).
-export([create_person/3, update_city/2, get_info/1]).

-record(person, {name, age, city}).

create_person(Name, Age, City) ->
    #person{name=Name, age=Age, city=City}.

update_city(Person, NewCity) ->
    Person#person{city=NewCity}.

get_info(Person) ->
    io:format("Name: ~p, Age: ~p, City: ~p~n", [Person#person.name, Person#person.age, Person#person.city]).
```

Dalam module ini, create_person/3 digunakan untuk membuat record baru, update_city/2 untuk memperbarui kota dalam record, dan get_info/1 untuk mencetak informasi tentang orang tersebut.
Kelebihan dan Kekurangan Record

Kelebihan:

    Membuat kode lebih mudah dibaca dan ditulis dengan merujuk pada field dengan nama.
    Struktur data yang baik untuk data statis yang strukturnya tidak berubah setelah didefinisikan.

Kekurangan:

    Tidak fleksibel seperti maps; struktur record harus didefinisikan di awal dan tidak bisa diubah pada runtime.
    Karena record adalah sintaks gula di atas tuple, operasi pada record lebih lambat dibandingkan dengan operasi langsung pada tuple, meskipun perbedaannya sangat kecil dan seringkali tidak signifikan.

Record adalah fitur yang sangat berguna dalam Erlang, terutama untuk mendefinisikan dan bekerja dengan data terstruktur dalam cara yang lebih terorganisir dan mudah dibaca.
