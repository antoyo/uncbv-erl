% Copyright (c) 2017 Boucher, Antoni <bouanto@zoho.com>
%
% Permission is hereby granted, free of charge, to any person obtaining a copy of
% this software and associated documentation files (the "Software"), to deal in
% the Software without restriction, including without limitation the rights to
% use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
% the Software, and to permit persons to whom the Software is furnished to do so,
% subject to the following conditions:
%
% The above copyright notice and this permission notice shall be included in all
% copies or substantial portions of the Software.
%
% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
% FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
% COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
% IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
% CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

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
