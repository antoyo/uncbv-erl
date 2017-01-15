-module(main).
-export([main/0, main/1]).

main() ->
    extract([]).

main(Args) ->
    extract(Args).

% Extract the archive named Filename and exit.
extract([Filename]) ->
    cbv:uncbv(Filename),
    erlang:halt().
