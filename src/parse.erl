-module(parse).
-export([count/3]).

% Apply the `Parser` `Count` times on the given `Input`.
count(Parser, Count, Input) ->
    Len = size(Input) div Count,
    Fun = fun(In) ->
              Parser(In)
          end,
    Counts = lists:seq(0, Count - 1),
    lists:map(fun(Cnt) ->
        Fun(binary:part(Input, Cnt * Len, Len)) end, Counts).
