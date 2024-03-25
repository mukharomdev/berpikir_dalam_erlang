% contoh membuat aplikasi konsole di erlang

-module(word_counter).
-export([start/0, read_input/0, count_words/1, display_result/1]).


% fungsi utama yang akan memanggil fungsi lainnya

start() ->
    io:format("Masukkan sebuah kalimat: "),
    Input = read_input(),
    Words = count_words(Input),
    display_result(Words).


read_input() ->
    io:get_line("").

count_words(Input) ->
    Words = string:split(string:trim(Input), " ", all),
    length(Words).

display_result(Words) ->
    io:format("Jumlah kata: ~p~n", [Words]).
