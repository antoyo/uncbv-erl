-module(cbv).
-export([uncbv/1]).

-define(FILE_COUNT_LEN, 16).
-define(FILE_NAME_LEN, 132).
-define(FILE_NAME_LEN_LEN, 8).
-define(MAGIC_NUMBER_LEN, 16).
-define(HEADER_LEN, (?MAGIC_NUMBER_LEN + ?FILE_COUNT_LEN + ?FILE_NAME_LEN_LEN + ?UNKNOWN_LEN) div 8).
-define(UNKNOWN_LEN, 24).

-record(file_metadata, {compressed_size,
                        decompressed_size,
                        filename}).

-record(header, {file_count,
                 filename_len
                }).

% Decode a binary c-string to an Erlang binary string.
decode_filename(Bin) ->
    hd(binary:split(Bin, <<0>>)).

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

% Extract the archive named Filename.
uncbv(Filename) ->
    {ok, File} = file:open(Filename, [read, binary]),
    {ok, HeaderBin} = file:read(File, ?HEADER_LEN),
    Header = header(HeaderBin),
    {ok, FileListBin} = file:read(File, Header#header.file_count * Header#header.filename_len),
    FileList = file_list(Header, FileListBin),
    file:close(File),
    io:format("~p~n", [FileList]).
