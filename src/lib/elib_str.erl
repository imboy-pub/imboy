-module(elib_str).

%%% @doc 字符串工具模块
%%% 提供字符串截断、替换、前缀/后缀检查等功能

-export([startswith/2]).
-export([endswith/2]).
-export([replace/3]).
-export([replace_single_quote/1]).
-export([trunc/2, trunc/3]).


%%====================================================================
%% @doc trunc/2
%% 使用默认后缀 "..." 截断
%% elib_str:trunc("12345678", 7).
%% <<"1234...">>
%%
%% elib_str:trunc(<<"12345678">>, 7, <<"**">>).
%% <<"12345**">>
%%
%% elib_str:trunc("123", 7).
%% "123"
%%
%% elib_str:trunc("12345678", 3).
%% <<"...">>
%%
%% elib_str:trunc(<<"123">>, 2, <<"abcdefgh">>).
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


%% @doc 替换单引号为转义的单引号
%% @param Bin 输入字符串（binary 或 list）
%% @returns 替换后的字符串（所有 ' 替换为 \'）
%% @example
%% elib_str:replace_single_quote(<<"hello D'l">>).
-spec replace_single_quote(binary() | list()) -> binary().
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


%% @doc 替换字符串中的所有匹配项（全局替换）
%% @param Subject 输入字符串
%% @param Pattern 要匹配的模式（正则表达式）
%% @param Replacement 替换字符串
%% @returns 替换后的字符串
%% @example
%% elib_str:replace("Hello, old world!", "old", "new").
%%   %% <<"Hello, new world!">>
-spec replace(binary() | list(), binary() | list(), binary() | list()) -> binary().
replace(Subject, Pattern, Replacement) ->
    % 不区分大小写
    % Pattern2 = "(?i)" ++ Pattern,
    re:replace(Subject, Pattern, Replacement, [{return, binary}, global]).


%% @doc 检查字符串是否以指定前缀开头
%% @param Sub 前缀字符串
%% @param Bin 要检查的字符串
%% @returns true | false
%% @example
%% elib_str:startswith(<<"abc">>, <<"abcdef">>).
%%   %% true
-spec startswith(binary() | list(), binary() | list()) -> boolean().
startswith(Sub, Bin) when is_binary(Sub), is_binary(Bin) ->
    case binary:part(Bin, {0, byte_size(Sub)}) of
        Sub -> true;
        _ -> false
    end;
startswith(Sub, Str) when is_list(Sub), is_list(Str) ->
    startswith(list_to_binary(Sub), list_to_binary(Str)).


%% @doc 检查字符串是否以指定后缀结尾
%% @param Suffix 后缀字符串
%% @param Val 要检查的字符串
%% @returns true | false
%% @example
%% elib_str:endswith(<<"ary">>, <<"This is the end of the binary">>).
%%   %% true
-spec endswith(binary() | list(), binary() | list()) -> boolean().
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
