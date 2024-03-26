Untuk menggunakan HTML markup dalam Erlang, biasanya kita bergantung pada library eksternal atau menulis fungsi sederhana yang menghasilkan string HTML berdasarkan input yang diberikan. Erlang sendiri tidak memiliki built-in library khusus untuk menghasilkan HTML, berbeda dengan bahasa web-centric seperti PHP atau JavaScript. Namun, Erlang sangat kuat dalam menangani concurrencies dan bisa sangat efektif digunakan sebagai backend server yang melayani data ke frontend web.

Salah satu cara untuk menghasilkan HTML dari Erlang adalah dengan menggunakan template engines seperti erlydtl, yang mirip dengan Django template engine untuk Python. ErlyDTL memungkinkan Anda menulis template HTML dan mengisinya dengan data dari Erlang.

Berikut adalah langkah-langkah dasar untuk menggunakan erlydtl:
1. Tambahkan erlydtl ke Proyek Anda

Jika Anda menggunakan rebar3 (sebuah tool build Erlang), tambahkan erlydtl ke dependencies di rebar.config:

erlang

{deps, [
    {erlydtl, ".*", {git, "https://github.com/erlydtl/erlydtl.git", {branch, "master"}}}
]}.

Setelah itu, jalankan rebar3 get-deps untuk mengunduh dan menginstal dependensi.
2. Membuat Template HTML

Buat file template HTML dengan ekstensi .dtl. Misalnya, example_template.dtl:

html

<!DOCTYPE html>
<html>
<head>
    <title>{{ title }}</title>
</head>
<body>
    <h1>{{ heading }}</h1>
    <p>{{ message }}</p>
</body>
</html>

3. Mengompilasi Template

Dalam kode Erlang, kompilasi template Anda menggunakan erlydtl:compile. Contoh:

erlang

-module(my_html_generator).
-export([generate_html/0]).

generate_html() ->
    {ok, Template} = erlydtl:compile("example_template.dtl", my_template),
    Data = [{title, "Test Page"}, {heading, "Hello, World!"}, {message, "This is a test of erlydtl."}],
    {ok, RenderedHtml} = erlydtl:render(Template, Data),
    RenderedHtml.

4. Menjalankan dan Menghasilkan HTML

Sekarang, Anda bisa menjalankan fungsi generate_html/0 untuk menghasilkan string HTML yang diisi dengan data yang Anda berikan.

erlang

1> my_html_generator:generate_html().

Hasilnya akan menjadi string HTML yang telah diisi dengan data dari variabel Data.

Perlu diperhatikan bahwa ini hanya contoh dasar. erlydtl dan template engines lainnya menawarkan fitur yang lebih kaya, seperti pengulangan, kondisional, dan filter, yang memungkinkan pembuatan template HTML yang lebih kompleks dan dinamis.

Jangan lupa untuk membaca dokumentasi erlydtl untuk informasi lebih detil tentang cara menggunakan dan fitur-fitur lanjutan yang ditawarkan.
