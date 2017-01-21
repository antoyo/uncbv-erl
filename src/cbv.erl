-module(cbv).
-export([uncbv/1]).

-define(FILE_COUNT_LEN, 16).
-define(FILE_NAME_LEN, 132).
-define(FILE_NAME_LEN_LEN, 8).
-define(MAGIC_NUMBER_LEN, 16).
-define(HEADER_LEN, (?MAGIC_NUMBER_LEN + ?FILE_COUNT_LEN + ?FILE_NAME_LEN_LEN + ?UNKNOWN_LEN) div 8).
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
block(FileMetadata, Bin) ->
    case Bin of
        <<
          BlockSize     : 16 / little-unsigned-integer,
          _UnknownBytes : 16 / little-unsigned-integer,
          Rest / binary
        >> ->
            CompressedBlock = binary:part(Rest, 0, BlockSize),
            extract_block(FileMetadata, CompressedBlock),
            Len = size(Rest) - BlockSize,
            binary:part(Rest, BlockSize, Len)
    end.

% Parse the compression flags.
compression_flags(2#00) -> #compression_flags{compressed=false, huffman_encoded=false};
compression_flags(2#01) -> #compression_flags{compressed=true, huffman_encoded=false};
compression_flags(2#10) -> #compression_flags{compressed=false, huffman_encoded=true};
compression_flags(2#11) -> #compression_flags{compressed=true, huffman_encoded=true}.

% Decode a binary c-string to an Erlang binary string.
decode_filename(Bin) ->
    hd(binary:split(Bin, <<0>>)).

% Decompress a block.
decompress_block(Bin) ->
    erlang:error(unimplemented),
    % TODO
    Bin.

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
    {ok, File} = file:open(Filename, [read, binary]),
    {ok, _} = file:position(File, Location),
    % TODO: refactor to avoid reading a whole (compressed) file into memory.
    {ok, Bin} = file:read(File, FileMetadata#file_metadata.compressed_size),
    parse:many(fun(Input) -> block(FileMetadata, Input) end, Bin),
    file:close(File).

% Parse the file list.
file_list(Header, Bin) ->
    parse:count(fun(Input) -> file_metadata(Header, Input) end, Header#header.file_count, Bin).

% Parse the file metadata (name and sizes).
file_metadata(Header, Bin) ->
    RestSize = Header#header.filename_len - 2 * 4 - ?FILE_NAME_LEN,
    case Bin of
        <<
          Filename         : ?FILE_NAME_LEN / binary,
          CompressedSize   : 32 / little-unsigned-integer,
          DecompressedSize : 32 / little-unsigned-integer,
          _Rest            : RestSize / binary
        >> ->
            DecodedFilename = decode_filename(Filename),
            #file_metadata{compressed_size=CompressedSize, decompressed_size=DecompressedSize, filename=DecodedFilename}
    end.

% Parse a CBV file header.
header(<<
         8           : ?MAGIC_NUMBER_LEN  / little-unsigned-integer,
         FileCount   : ?FILE_COUNT_LEN    / little-unsigned-integer,
         FileNameLen : ?FILE_NAME_LEN_LEN / little-unsigned-integer,
         _Unknown    : ?UNKNOWN_LEN
       >>) ->
    #header{file_count=FileCount, filename_len=FileNameLen}.

% Decode a huffman-encoded block.
huffman(Bin) ->
    erlang:error(unimplemented),
    % TODO
    Bin.

% Extract the archive named Filename.
uncbv(Filename) ->
    {ok, File} = file:open(Filename, [read, binary]),
    {ok, HeaderBin} = file:read(File, ?HEADER_LEN),
    Header = header(HeaderBin),
    FileListSize = Header#header.file_count * Header#header.filename_len,
    {ok, FileListBin} = file:read(File, FileListSize),
    FileList = file_list(Header, FileListBin),
    file:close(File),
    [FileMetadata|_] = FileList,
    HeaderSize = ?HEADER_LEN + FileListSize,
    extract_file(Filename, FileMetadata, HeaderSize),
    io:format("~p~n", [FileList]).
