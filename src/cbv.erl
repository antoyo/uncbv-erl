-module(cbv).
-export([uncbv/1]).

-define(FILE_COUNT_LEN, 16).
-define(FILE_NAME_LEN_LEN, 8).
-define(MAGIC_NUMBER_LEN, 16).
-define(HEADER_LEN, (?MAGIC_NUMBER_LEN + ?FILE_COUNT_LEN + ?FILE_NAME_LEN_LEN + ?UNKNOWN_LEN) div 8).
-define(UNKNOWN_LEN, 24).

-record(header, {file_count,
                 filename_len
                }).

% Parse a CBV file header.
header(<<
         8           : ?MAGIC_NUMBER_LEN  / little,
         FileCount   : ?FILE_COUNT_LEN    / little,
         FileNameLen : ?FILE_NAME_LEN_LEN / little,
         _Unknown    : ?UNKNOWN_LEN
       >>) ->
    #header{file_count=FileCount, filename_len=FileNameLen}.

% Extract the archive named Filename.
uncbv(Filename) ->
    {ok, File} = file:open(Filename, [read, binary]),
    {ok, HeaderBin} = file:read(File, ?HEADER_LEN),
    Header = header(HeaderBin),
    file:close(File),
    io:format("~p~n", [Header]).
