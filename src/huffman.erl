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
