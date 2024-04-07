Di Erlang, tag -opaque adalah sebuah direktif yang digunakan dalam module untuk mendeklarasikan tipe data yang tidak seharusnya diinspeksi atau dimanipulasi secara langsung dari luar module tempat tipe data tersebut didefinisikan. Dengan kata lain, -opaque menandakan bahwa struktur internal dari tipe data tersebut adalah "tersembunyi" atau tidak transparent, dan operasi atas tipe data tersebut harus dilakukan melalui interface yang disediakan oleh module tersebut.

Menggunakan -opaque membantu dalam pemrograman yang lebih aman dari segi tipe, karena memastikan bahwa detail implementasi suatu tipe data tidak bocor keluar dari batas module-nya. Hal ini memungkinkan pengembang untuk mengubah representasi internal tipe data tanpa merusak kode yang mengandalkan module tersebut, selama interface publik module tidak berubah.

Contoh penggunaan -opaque:

```erlang

-module(some_module).
-export([make_some_data/1, operate_on_data/1]).

-opaque some_data_type() :: {some_internal_data, term()}.

-spec make_some_data(term()) -> some_data_type().
make_some_data(X) -> {some_internal_data, X}.

-spec operate_on_data(some_data_type()) -> term().
operate_on_data({some_internal_data, X}) -> %% melakukan sesuatu dengan X
```
Dalam contoh ini, some_data_type() dideklarasikan sebagai tipe data yang opaque dengan -opaque. Ini berarti bahwa detail dari some_data_type(), yaitu bahwa ia merupakan sebuah tuple dengan atom some_internal_data sebagai elemen pertamanya, tidak seharusnya diketahui atau diandalkan oleh module lain yang menggunakan some_module. Module lain harus menggunakan fungsi make_some_data/1 untuk membuat data tipe ini dan operate_on_data/1 untuk beroperasi atas data tersebut.

Ini membantu menjaga abstraksi dan memudahkan refactoring, karena implementasi internal dari some_data_type() dapat diubah tanpa mempengaruhi kode yang menggunakannya, asalkan interface fungsi make_some_data/1 dan operate_on_data/1 tetap konsisten.