-module(imboy_str).

-export([startswith/2]).
-export([endswith/2]).
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
    % <<"Hello, new world!">>
% string:replace("Hello, old world!", "old", "new").
    % ["Hello, ","new"," world!"]
% string:replace(<<"Hello, old world!">>, "old", "new").
    % [<<"Hello, ">>,"new",<<" world!">>]
replace(Subject, Pattern, Replacement) ->
    % 不区分大小写
    % Pattern2 = "(?i)" ++ Pattern,
    re:replace(Subject, Pattern, Replacement, [{return, binary}, global]).

% [imboy_str:startswith("aabc", "abcdef"), imboy_str:startswith(<<"aabc">>, <<"abcdef">>),  imboy_str:startswith("abc", "abcdef"), imboy_str:startswith(<<"abc">>, <<"abcdef">>)].
% string:prefix("abcdef", "abc").
startswith(Sub, Bin) when is_binary(Sub), is_binary(Bin) ->
    case binary:part(Bin, {0, byte_size(Sub)}) of
        Sub -> true;
        _ -> false
    end;
startswith(Sub, Str) when is_list(Sub), is_list(Str) ->
    startswith(list_to_binary(Sub), list_to_binary(Str)).

% [imboy_str:endswith("end", "This is the end of the string"), imboy_str:endswith(<<"end">>, <<"This is the end of the binary">>), imboy_str:endswith("ing", "This is the end of the string"),imboy_str:endswith(<<"ary">>, <<"This is the end of the binary">>)].
endswith(Suffix, Val) when is_list(Suffix), is_list(Val) ->
    endswith(list_to_binary(Suffix), list_to_binary(Val));
endswith(Suffix, Val) when is_binary(Suffix), is_binary(Val) ->
    SuffixSize = byte_size(Suffix),
    ValSize = byte_size(Val),
    case ValSize >= SuffixSize of
        true ->
            BinaryPart = binary:part(Val, {ValSize - SuffixSize, SuffixSize}),
            Suffix =:= BinaryPart;
        false ->
            false
    end.
