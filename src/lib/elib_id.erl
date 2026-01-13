-module(elib_id).

%%% @doc ID 生成工具模块
%%% 提供唯一标识符生成功能，支持带前缀和不带前缀的 ID 生成

-export([gen/0, gen/1]).

%% @doc 生成唯一标识符（无前缀）
%% @returns UID 二进制字符串
%%
%% @example
%% Id = elib_id:gen().
%% <<"...binary...">>
-spec gen() -> binary().
gen() ->
    gen("").

%% @doc 生成带前缀的唯一标识符
%% @param Prefix 前缀（integer、list 或 binary）
%% @returns 带前缀的 UID 二进制字符串
%%
%% @example
%% MsgId = elib_id:gen(<<"msg_">>).
%% <<"msg_...binary...">>
%%
%% IntId = elib_id:gen(123).
%% <<"123...binary...">>
%%
%% ListId = elib_id:gen("user_").
%% <<"user_...binary...">>
-spec gen(integer() | list() | binary()) -> binary().
gen(Prefix) ->
    U1 = uid:encode64(uid:g()),
    iolist_to_binary([ec_cnv:to_binary(Prefix), U1]).
