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

-module(cbv).
-export([uncbv/1]).

-include("records.hrl").

-define(FILE_COUNT_LEN, 16).
-define(FILE_NAME_LEN, 132).
-define(FILE_NAME_LEN_LEN, 8).
-define(MAGIC_NUMBER_LEN, 16).
-define(HEADER_LEN, (?MAGIC_NUMBER_LEN + ?FILE_COUNT_LEN + ?FILE_NAME_LEN_LEN + ?UNKNOWN_LEN) div 8).
-define(TREE_SIZE, 256).
-define(UNKNOWN_LEN, 24).

-record(compression_flags, {compressed,
                            huffman_encoded}).

-record(file_metadata, {compressed_size,
                        decompressed_size,
                        filename}).

-record(header, {file_count,
                 filename_len
                }).

% Parse a compressed block.
block(FileMetadata,
      <<
        BlockSize     : 16 / little-unsigned-integer,
        _UnknownBytes : 16 / little-unsigned-integer,
        Rest / binary
      >>) ->
    CompressedBlock = binary:part(Rest, 0, BlockSize),
    extract_block(FileMetadata, CompressedBlock),
    Len = size(Rest) - BlockSize,
    binary:part(Rest, BlockSize, Len).

% Parse the compression flags.
compression_flags(2#00) -> #compression_flags{compressed=false, huffman_encoded=false};
compression_flags(2#01) -> #compression_flags{compressed=true, huffman_encoded=false};
compression_flags(2#10) -> #compression_flags{compressed=false, huffman_encoded=true};
compression_flags(2#11) -> #compression_flags{compressed=true, huffman_encoded=true}.

% Create a Huffman tree from an array.
create_huffman_tree(Array) ->
    Fun =
        fun(Value, Bits, Tree) ->
            case Bits of
                << >> -> Tree;
                _ -> tree_insert(Bits, Value, Tree)
            end
        end,
    array:foldl(Fun, nil, Array).

% Decode a binary c-string to an Erlang binary string.
decode_filename(Bin) ->
    hd(binary:split(Bin, <<0>>)).

% Decompress some bytes.
decompress_bytes(0, _, Rest, Result) -> {Result, Rest};
decompress_bytes(_, _, Bin, Result) when size(Bin) =:= 0 -> {Result, Bin};
decompress_bytes(Count, CodeBytes, Bin, Acc) ->
    NewCodeBytes = CodeBytes bsl 1,
    NewCount = Count - 1,
    if CodeBytes band 16#8000 =/= 0 ->
           % The current byte is a coded byte.
           <<
             High  : 4,
             Low   : 4,
             Byte1 : 8 / unsigned-integer,
             Rest / binary
           >> = Bin,
           case High of
               0 ->
                   % Run-length decoding.
                   Size = Low + 3,
                   Bytes = binary:copy(<< Byte1 >>, Size),
                   NewAcc = << Acc/binary, Bytes/binary >>,
                   decompress_bytes(NewCount, NewCodeBytes, Rest, NewAcc);
               1 ->
                   % Run-length decoding with bigger size.
                   Size = Low + (Byte1 bsl 4) + 16#13,
                   << Byte2:8, NewRest/binary >> = Rest,
                   Bytes = binary:copy(<< Byte2 >>, Size),
                   NewAcc = << Acc/binary, Bytes/binary >>,
                   decompress_bytes(NewCount, NewCodeBytes, NewRest, NewAcc);
               _ ->
                   % Copy content already seen in the file (backward reference).
                   % Get the offset and the length.
                   Offset = (Byte1 bsl 4) + Low + 3,
                   {Size, NewBin} =
                       case High of
                           2 ->
                               << Byte2:8, NewRest/binary >> = Rest,
                               {Byte2 + 16#10, NewRest};
                           _ -> {High, Rest}
                       end,
                   CurrentPosition = size(Acc),
                   Start = CurrentPosition - Offset,
                   BackwardReference = binary:part(Acc, Start, Size),
                   NewAcc = << Acc/binary, BackwardReference/binary >>,
                   decompress_bytes(NewCount, NewCodeBytes, NewBin, NewAcc)
           end;
       true ->
           << Byte : 8, Rest / binary >> = Bin,
           NewAcc = << Acc/binary, Byte >>,
           decompress_bytes(NewCount, NewCodeBytes, Rest, NewAcc)
    end.

% Decompress a block.
decompress_block(Bin) ->
    decompress_block(Bin, << >>).

decompress_block(Bin, Result) when size(Bin) =:= 0 ->
    Result;
decompress_block(
  <<
    CodeBytes : 16 / little-unsigned-integer,
    Rest / binary
  >>, Result) ->
    {Bytes, NewRest} = decompress_bytes(16, CodeBytes, Rest, Result),
    decompress_block(NewRest, Bytes).

% Extract, decode and decompress a block.
extract_block(FileMetadata, Bin) ->
    FlagsBin = binary:first(Bin),
    Flags = compression_flags(FlagsBin),
    Rest = binary:part(Bin, 1, size(Bin) - 1),
    NewInput =
        if Flags#compression_flags.huffman_encoded -> huffman(Rest);
           true -> Rest
        end,
    Result =
        if Flags#compression_flags.compressed -> decompress_block(NewInput);
           true -> NewInput
        end,
    {ok, File} = file:open(FileMetadata#file_metadata.filename, [append]),
    ok = file:write(File, Result),
    ok = file:close(File),
    ok.

% Extract a file from the archive.
extract_file(Filename, FileMetadata, Location) ->
    file:delete(FileMetadata#file_metadata.filename),
    {ok, File} = file:open(Filename, [read, binary]),
    {ok, _} = file:position(File, Location),
    % TODO: refactor to avoid reading a whole (compressed) file into memory.
    {ok, Bin} = file:read(File, FileMetadata#file_metadata.compressed_size),
    parse:many(fun(Input) -> block(FileMetadata, Input) end, Bin),
    file:close(File).

% Parse the file list.
file_list(Header, Bin) ->
    parse:count(fun(Input) -> file_metadata(Input) end, Header#header.file_count, Bin).

% Parse the file metadata (name and sizes).
file_metadata(
    <<
      Filename         : ?FILE_NAME_LEN / binary,
      CompressedSize   : 32 / little-unsigned-integer,
      DecompressedSize : 32 / little-unsigned-integer,
      _Rest / binary
    >>) ->
    DecodedFilename = decode_filename(Filename),
    #file_metadata{compressed_size=CompressedSize, decompressed_size=DecompressedSize, filename=DecodedFilename}.

% Parse a CBV file header.
header(<<
         8           : ?MAGIC_NUMBER_LEN  / little-unsigned-integer,
         FileCount   : ?FILE_COUNT_LEN    / little-unsigned-integer,
         FileNameLen : ?FILE_NAME_LEN_LEN / little-unsigned-integer,
         _Unknown    : ?UNKNOWN_LEN
       >>) ->
    #header{file_count=FileCount, filename_len=FileNameLen}.

% Decode a huffman-encoded block.
huffman(<<
          DecompressedSize : 16 / big-unsigned-integer,
          Rest / binary
        >>) ->
    {Tree, Input} = huffman_tree(Rest),
    huffman:decode(Input, Tree, DecompressedSize).

% Decode a huffman tree.
huffman_tree(Bin) ->
    InitArray = array:new(?TREE_SIZE),
    {Array, Input} = huffman_tree(0, Bin, InitArray),
    {create_huffman_tree(Array), Input}.

huffman_tree(256, Rest, Result) -> {Result, Rest};
huffman_tree(Count,
    <<
      Len  : 4   / little-unsigned-integer,
      Bits : Len / bitstring,
      Rest       / bitstring
    >>, Acc) ->
    NewAcc = array:set(Count, Bits, Acc),
    huffman_tree(Count + 1, Rest, NewAcc).

% Insert a Value from Bits into the Tree.
tree_insert(<< >>, Value, _) ->
    #tree{value=Value};
tree_insert(Bits, Value, nil) ->
    tree_insert(Bits, Value, #tree{});
tree_insert(<< 0:1, Bits/bitstring >>, Value, Tree) ->
    Tree#tree { left = tree_insert(Bits, Value, Tree#tree.left) };
tree_insert(<< 1:1, Bits/bitstring >>, Value, Tree) ->
    Tree#tree { right = tree_insert(Bits, Value, Tree#tree.right) }.

% Extract the archive named Filename.
uncbv(Filename) ->
    {ok, File} = file:open(Filename, [read, binary]),
    {ok, HeaderBin} = file:read(File, ?HEADER_LEN),
    Header = header(HeaderBin),
    FileListSize = Header#header.file_count * Header#header.filename_len,
    {ok, FileListBin} = file:read(File, FileListSize),
    FileList = file_list(Header, FileListBin),
    file:close(File),
    HeaderSize = ?HEADER_LEN + FileListSize,
    lists:foldl(
      fun(FileMetadata, Pos) ->
          extract_file(Filename, FileMetadata, Pos),
          Pos + FileMetadata#file_metadata.compressed_size
      end, HeaderSize, FileList).
