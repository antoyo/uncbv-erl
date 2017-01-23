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

-module(huffman).
-export([decode/3]).

-include("records.hrl").

% Decode the huffman-encoded Input using the Huffman Tree.
% The decoding ends when DecompressedSize is reached.
decode(Input, Tree, DecompressedSize) ->
    decode(Input, Tree, DecompressedSize, << >>).

decode(_, _, 0, Result) -> Result;
decode(Input, Tree, DecompressedSize, Result) ->
    {Byte, NewInput} = decode_byte(Input, Tree),
    decode(NewInput, Tree, DecompressedSize - 1, << Result/binary, Byte >>).

% Decode the next byte from Input using the Huffman Tree.
decode_byte(Input, #tree{value=Value, left=nil, right=nil}) ->
    {Value, Input};
decode_byte(<< 0:1, Rest/bitstring >>, #tree{left=Left}) ->
    decode_byte(Rest, Left);
decode_byte(<< 1:1, Rest/bitstring >>, #tree{right=Right}) ->
    decode_byte(Rest, Right).
