# Shell Erlang

Shell Erlang merupakan lingkungan interaktif (REPL - Read-Eval-Print Loop) yang memungkinkan Anda untuk mengeksekusi perintah Erlang secara langsung, membuat prototipe cepat, dan bereksperimen dengan kode tanpa perlu mengkompilasi modul terlebih dahulu. Ini sangat berguna untuk pembelajaran, pengujian fungsi, dan debug. Berikut adalah beberapa dasar penggunaan shell Erlang.
Memulai Shell Erlang

Untuk memulai shell Erlang, cukup ketik `erl` di terminal atau command prompt. Anda akan melihat output seperti ini:

```shell

Erlang/OTP 24 [erts-11.1] [source] [64-bit] [smp:8:8] ...

Eshell V11.1  (abort with ^G)
1>
```

## Dasar-Dasar Perintah Shell

- **Ekspresi**. Anda dapat mengetikkan ekspresi Erlang apa pun dan menekan Enter untuk mengevaluasinya. Contoh:

```erlang

1> 3 + 4.
7
```
- Variabel. Anda dapat mengikat nilai ke variabel (ingat bahwa variabel harus diawali dengan huruf besar). Variabel di shell dapat direset dengan perintah f().

```erlang

1> X = 10.
10
2> X.
10
3> f(X).
ok
4> X.
* 1: variable 'X' is unbound
```
- Fungsi. Anda dapat mendefinisikan fungsi sederhana langsung dalam shell, meski dengan sintaks yang agak terbatas dibandingkan dengan mendefinisikan fungsi dalam modul.

```erlang

1> F = fun(X) -> X * 2 end.
#Fun<erl_eval.44.40011524>
2> F(10).
20
```
- Modul. Untuk menggunakan fungsi dari modul, Anda harus memastikan bahwa modul tersebut sudah terkompilasi dan berada di path yang dapat ditemukan oleh Erlang.

```erlang

1> c(my_module).
{ok,my_module}
2> my_module:my_function().
```

Keluar dari Shell. Untuk keluar dari shell Erlang, tekan Ctrl+C dua kali atau gunakan perintah q()..

```erlang

    1> q().
    ok
    2> 
```
Fitur Lanjutan

- Menggunakan Bantuan. Erlang shell tidak memiliki sistem bantuan built-in seperti beberapa REPL lain, tetapi dokumentasi Erlang secara online sangat lengkap.

- History dan Autocomplete. Versi terbaru dari shell Erlang mendukung history command (menggunakan panah atas dan bawah) dan autocomplete fungsi dan modul dengan menekan Tab.

- Mengelola Proses. Anda dapat memulai dan memantau proses Erlang langsung dari shell, menggunakan fungsi seperti spawn dan self.

Shell Erlang adalah alat yang sangat kuat untuk eksplorasi dan pengembangan cepat. Mempraktikkan Erlang melalui shell adalah cara yang bagus untuk mempelajari bahasa dan ekosistemnya dengan interaktif.