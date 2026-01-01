-module(imboy_str).

-export([startswith/2]).
-export([endswith/2]).
-export([replace/3]).
-export([replace_single_quote/1]).
-export([trunc/2, trunc/3]).


%%====================================================================
%% @doc trunc/2
%% 使用默认后缀 "..." 截断
%% imboy_str:trunc("12345678", 7).
%% <<"1234...">>
%%
%% imboy_str:trunc(<<"12345678">>, 7, <<"**">>).
%% <<"12345**">>
%%
%% imboy_str:trunc("123", 7).
%% "123"
%%
%% imboy_str:trunc("12345678", 3).
%% <<"...">>
%%
%% imboy_str:trunc(<<"123">>, 2, <<"abcdefgh">>).
%%====================================================================
-spec trunc(binary() | string(), non_neg_integer()) -> binary() | string().
trunc(Input, MaxLen) when is_list(Input) ->
    trunc(Input, MaxLen, "...");
trunc(Input, MaxLen) when is_binary(Input) ->
    trunc(Input, MaxLen, <<"...">>).

%%====================================================================
%% @doc trunc/3
%% 截断 Input 到 MaxLen，末尾加 Suffix，返回长度 <= MaxLen
%%====================================================================
-spec trunc(binary() | string(), non_neg_integer(), binary() | string()) -> binary() | string().

%% ---------- binary 版本 ----------
trunc(Input, MaxLen, Suffix) when is_binary(Input), is_binary(Suffix) ->
    InputLen = byte_size(Input),
    SuffixLen = byte_size(Suffix),
    if
        InputLen =< MaxLen ->
            Input;
        SuffixLen == 0 ->
            <<Head:MaxLen/binary, _/binary>> = Input,
            Head;
        SuffixLen >= MaxLen ->
            %% 关键改动：保留 Input 前 MaxLen
            <<Head:MaxLen/binary, _/binary>> = Input,
            Head;
        true ->
            Keep = MaxLen - SuffixLen,
            <<Head:Keep/binary, _/binary>> = Input,
            <<Head/binary, Suffix/binary>>
    end;

%% ---------- list 版本 ----------
trunc(Input, MaxLen, Suffix) when is_list(Input), is_list(Suffix) ->
    InputLen = length(Input),
    SuffixLen = length(Suffix),
    if
        InputLen =< MaxLen ->
            Input;
        SuffixLen == 0 ->
            lists:sublist(Input, MaxLen);
        SuffixLen >= MaxLen ->
            %% 关键改动：不再返回 suffix，而是保留 input
            lists:sublist(Input, MaxLen);
        true ->
            Trunc = lists:sublist(Input, MaxLen - SuffixLen),
            Trunc ++ Suffix
    end.


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
