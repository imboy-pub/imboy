%%% @doc 消息 ID 值对象 / Message ID Value Object（T0.2）
%%%
%%% 封装消息标识（Xid/TSID binary），提供构造期校验与按值相等，
%%% 替换全层裸 binary 穿透。opaque type 由 dialyze 守护封装性：
%%% 模块外不得解构，只能经 new/1 构造、value/1 取值。
-module(message_id_vo).

-export([new/1, value/1, equal/2]).
-export_type([t/0]).

-opaque t() :: {message_id, binary()}.

%% @doc 构造消息 ID 值对象，空或非 binary 拒绝。
-spec new(binary()) -> {ok, t()} | {error, invalid_message_id}.
new(Bin) when is_binary(Bin), byte_size(Bin) > 0 ->
    {ok, {message_id, Bin}};
new(_) ->
    {error, invalid_message_id}.

%% @doc 取底层 binary 值（用于持久化/序列化边界）。
-spec value(t()) -> binary().
value({message_id, Bin}) ->
    Bin.

%% @doc 按值相等。
-spec equal(t(), t()) -> boolean().
equal({message_id, A}, {message_id, B}) ->
    A =:= B.
