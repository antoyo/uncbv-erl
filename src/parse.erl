-module(parse).
-export([count/3, many/2]).

% Apply the `Parser` `Count` times on the given `Input`.
count(Parser, Count, Input) ->
    Len = size(Input) div Count,
    Counts = lists:seq(0, Count - 1),
    lists:map(fun(Cnt) ->
        Parser(binary:part(Input, Cnt * Len, Len)) end, Counts).

% Apply the `Parser` as many times as possible on the given `Input`.
many(_, Input) when size(Input) =:= 0 ->
    ok;
many(Parser, Input) ->
    NewInput = Parser(Input),
    many(Parser, NewInput).
