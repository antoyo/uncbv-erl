-module(main).
-export([main/0, main/1]).

main() ->
    extract([]).

main(Args) ->
    extract(Args).

extract(Filenames) ->
    io:format("~p~n", [Filenames]),
    erlang:halt().
