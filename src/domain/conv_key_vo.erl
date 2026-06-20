%%% @doc 会话键值对象 / Conversation Key Value Object
%%%
%%% DDD 充血改造地基（Phase 0 / T0.2）：封装会话唯一键的格式不变量，
%%% 杜绝散落各处的字符串拼接。权威格式（SOURCE: msg_archive_repo:conv_key/3
%%% 与 msg_c2s_logic:authorize_conv/2）：
%%%   C2C = "c2c:{min_uid}:{max_uid}"（两 uid 升序，保证交换律）
%%%   C2G = "c2g:{group_id}"
%%%
%%% Encapsulates the conversation-key format invariant in one opaque VO,
%%% so c2c keys are always normalized to min:max and parsing is centralized.
-module(conv_key_vo).

-export([c2c/2, c2g/1, parse/1, value/1, type/1, equal/2]).
-export([c2c_members/1]).

-export_type([t/0, conv_type/0]).

-type conv_type() :: c2c | c2g.

-record(conv_key, {type :: conv_type(), v :: binary()}).
-opaque t() :: #conv_key{}.

%% @doc 构造 C2C 会话键，两 uid 规范化为 min:max。
%% 同一用户（自会话）与非正整数均拒绝。
-spec c2c(integer(), integer()) -> {ok, t()} | {error, invalid_c2c_uids}.
c2c(A, B) when is_integer(A), is_integer(B), A > 0, B > 0, A =/= B ->
    Min = erlang:min(A, B),
    Max = erlang:max(A, B),
    Key = <<"c2c:", (integer_to_binary(Min))/binary, ":", (integer_to_binary(Max))/binary>>,
    {ok, #conv_key{type = c2c, v = Key}};
c2c(_, _) ->
    {error, invalid_c2c_uids}.

%% @doc 构造 C2G 会话键。
-spec c2g(integer()) -> {ok, t()} | {error, invalid_gid}.
c2g(Gid) when is_integer(Gid), Gid > 0 ->
    Key = <<"c2g:", (integer_to_binary(Gid))/binary>>,
    {ok, #conv_key{type = c2g, v = Key}};
c2g(_) ->
    {error, invalid_gid}.

%% @doc 从持久化字符串解析会话键，校验格式并规范化 c2c 排序。
-spec parse(binary()) -> {ok, t()} | {error, invalid_conv_key}.
parse(<<"c2c:", Rest/binary>>) ->
    case binary:split(Rest, <<":">>) of
        [ABin, BBin] ->
            case {to_pos_int(ABin), to_pos_int(BBin)} of
                {{ok, A}, {ok, B}} when A =/= B ->
                    c2c(A, B);
                _ ->
                    {error, invalid_conv_key}
            end;
        _ ->
            {error, invalid_conv_key}
    end;
parse(<<"c2g:", GidBin/binary>>) ->
    case to_pos_int(GidBin) of
        {ok, Gid} ->
            c2g(Gid);
        _ ->
            {error, invalid_conv_key}
    end;
parse(_) ->
    {error, invalid_conv_key}.

%% @doc 取回会话键字符串。
-spec value(t()) -> binary().
value(#conv_key{v = V}) ->
    V.

%% @doc 取回会话类型（c2c | c2g）。
-spec type(t()) -> conv_type().
type(#conv_key{type = T}) ->
    T.

%% @doc 解析 c2c 持久化键并取回两个会话方 uid（升序 min/max）。
%% 供资源读鉴权判定 "Uid 是否会话方" 复用；非合法 c2c 键一律拒绝。
-spec c2c_members(binary()) -> {ok, {pos_integer(), pos_integer()}} | {error, invalid_conv_key}.
c2c_members(Bin) ->
    case parse(Bin) of
        {ok, #conv_key{type = c2c, v = V}} ->
            <<"c2c:", Rest/binary>> = V,
            [MinBin, MaxBin] = binary:split(Rest, <<":">>),
            {ok, {binary_to_integer(MinBin), binary_to_integer(MaxBin)}};
        _ ->
            {error, invalid_conv_key}
    end.

%% @doc 等值比较（按规范化后的键值）。
-spec equal(t(), t()) -> boolean().
equal(#conv_key{v = A}, #conv_key{v = B}) ->
    A =:= B.

%% @doc 严格正整数解析：非全数字串拒绝。
-spec to_pos_int(binary()) -> {ok, pos_integer()} | error.
to_pos_int(Bin) ->
    try binary_to_integer(Bin) of
        N when N > 0 -> {ok, N};
        _ -> error
    catch
        error:badarg -> error
    end.
