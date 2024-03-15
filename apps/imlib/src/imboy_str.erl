-module(imboy_str).

-export([startswith/2]).
-export([replace/3]).
-export([replace_single_quote/1]).

% imboy_str:replace_single_quote(<<"hello D'l">>).
replace_single_quote(Bin) when is_list(Bin) ->
    replace_single_quote(list_to_binary(Bin));
replace_single_quote(Bin) ->
    replace_single_quote(Bin, <<>>).

replace_single_quote(<<>>, Acc) ->
    Acc;
replace_single_quote(<<39, Rest/binary>>, Acc) ->
    % 39 is the ASCII code for the apostrophe (')
    % 92 is the ASCII code for the backslash (\)
    replace_single_quote(Rest, <<Acc/binary, 92, 39>>);
replace_single_quote(<<Byte, Rest/binary>>, Acc) ->
    replace_single_quote(Rest, <<Acc/binary, Byte>>).

% imboy_str:replace("Hello, old world!", "old", "new").
replace(Subject, Pattern, Replacement) ->
    % 不区分大小写
    % Pattern2 = "(?i)" ++ Pattern,
    re:replace(Subject, Pattern, Replacement, [{return, binary}, global]).

% imboy_str:startswith("abc", "abcdef").
startswith([], _) -> true;
startswith([Ch | Rest1], [Ch | Rest2]) ->
        startswith(Rest1, Rest2);
startswith(_, _) -> false.

