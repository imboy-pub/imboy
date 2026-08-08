-module(conversation_logic).
-compile([nowarn_deprecated_catch]).
%%%
% conversation 业务逻辑模块
% conversation business logic module
% 会话业务逻辑层，处理会话相关的业务逻辑，包括删除、恢复等操作
%%%

-export([delete/3]).
-export([restore/3]).
-export([list/2]).
-export([is_deleted/3]).
-export([filter_deleted_conversations/2]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 获取服务端权威会话列表（c2c + c2g）
%% @param Uid 用户ID
%% @param Opts 选项 #{limit => integer(), last_server_ts => term()}
%% @return {ok, list(map())}
-spec list(integer(), map()) -> {ok, list(map())}.
list(Uid, Opts) ->
    Limit = normalize_limit(maps:get(limit, Opts, 1000)),
    LastServerTs = maps:get(last_server_ts, Opts, undefined),
    C2cMsgs = safe_read_c2c(Uid, Limit, LastServerTs),
    C2gMsgs = safe_read_c2g(Uid, Limit, LastServerTs),
    Unified = merge_latest_conversations(Uid, C2cMsgs, C2gMsgs),
    Visible = filter_deleted_conversations(Uid, Unified),
    {ok, sort_conversations(attach_pin_state(Uid, Visible))}.

%% @doc 删除会话（软删除）
%% @param Uid 用户ID
%% @param ConversationId 会话ID（单聊为对方UID，群聊为群ID）
%% @param Type 会话类型（c2c/c2g）
%% @return ok 成功 | {error, Reason} 失败
-spec delete(integer(), integer(), binary()) -> ok | {error, binary()}.
delete(_Uid, ConversationId, _Type) when not is_integer(ConversationId); ConversationId =< 0 ->
    {error, <<"会话ID无效"/utf8>>};
delete(_Uid, _ConversationId, <<>>) ->
    {error, <<"会话类型不能为空"/utf8>>};
delete(_Uid, _ConversationId, Type) when Type =/= <<"c2c">> andalso Type =/= <<"c2g">> ->
    {error, <<"无效的会话类型"/utf8>>};
delete(Uid, ConversationId, Type) ->
    % 检查是否已经删除
    case conversation_delete_ds:is_conversation_deleted(Uid, ConversationId, Type) of
        true ->
            % 已经删除，直接返回成功（幂等性）
            ok;
        false ->
            % 标记会话为已删除
            case conversation_delete_ds:delete_conversation(Uid, ConversationId, Type) of
                ok ->
                    notify_conversation_change(
                        Uid, ConversationId, Type, <<"conversation_deleted">>
                    ),
                    ok;
                {error, Reason} ->
                    ?LOG(error, "删除会话失败: ~p", [Reason]),
                    {error, <<"删除会话失败"/utf8>>}
            end
    end.

%% @doc 恢复已删除的会话
%% @param Uid 用户ID
%% @param ConversationId 会话ID
%% @param Type 会话类型（c2c/c2g）
%% @return ok 成功 | {error, Reason} 失败
-spec restore(integer(), integer(), binary()) -> ok | {error, binary()}.
restore(_Uid, ConversationId, _Type) when not is_integer(ConversationId); ConversationId =< 0 ->
    {error, <<"会话ID无效"/utf8>>};
restore(Uid, ConversationId, Type) ->
    conversation_delete_ds:restore_conversation(Uid, ConversationId, Type),
    notify_conversation_change(Uid, ConversationId, Type, <<"conversation_restored">>),
    ok.

%% @doc 检查会话是否已删除
%% @param Uid 用户ID
%% @param ConversationId 会话ID
%% @param Type 会话类型（c2c/c2g）
%% @return true 已删除 | false 未删除
-spec is_deleted(integer(), integer(), binary()) -> boolean().
is_deleted(Uid, ConversationId, Type) ->
    conversation_delete_ds:is_conversation_deleted(Uid, ConversationId, Type).

%% @doc 获取用户的已删除会话列表
%% @param Uid 用户ID
%% @return {ok, List} 查询成功返回已删除列表 | {error, Reason} 查询失败
-spec get_deleted_list(integer()) -> {ok, list(map())} | {error, term()}.
get_deleted_list(Uid) ->
    conversation_delete_ds:get_deleted_conversations(Uid).

%% @doc 过滤已删除的会话
%% @param Uid 用户ID
%% @param MsgList 消息列表（会话列表）
%% @return 过滤后的消息列表
-spec filter_deleted_conversations(integer(), list(map())) -> list(map()).
filter_deleted_conversations(Uid, MsgList) ->
    DeletedResult =
        try
            get_deleted_list(Uid)
        catch
            _:Reason ->
                ?LOG(error, "查询会话删除列表失败: ~p", [Reason]),
                {error, Reason}
        end,
    case DeletedResult of
        {ok, DeletedList} ->
            DeletedSet = sets:from_list([
                {maps:get(<<"conversation_id">>, Item), maps:get(<<"conversation_type">>, Item)}
             || Item <- DeletedList
            ]),
            lists:filter(
                fun(Item) ->
                    case resolve_conversation_key(Item) of
                        {ok, ConversationKey} ->
                            not sets:is_element(ConversationKey, DeletedSet);
                        error ->
                            true
                    end
                end,
                MsgList
            );
        {error, _Reason} ->
            MsgList
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 向用户的所有在线设备推送会话变更通知
%% @private
-spec notify_conversation_change(integer(), integer(), binary(), binary()) -> ok.
notify_conversation_change(Uid, ConversationId, Type, Action) ->
    Payload = #{
        <<"conversation_id">> => ConversationId,
        <<"conversation_type">> => Type
    },
    msg_s2c_ds:send(Uid, [Uid], Action, <<>>, null, Payload, no_save),
    ok.

-spec normalize_limit(term()) -> integer().
normalize_limit(Limit) when is_integer(Limit), Limit > 0 ->
    erlang:min(Limit, 2000);
normalize_limit(_) ->
    1000.

-spec safe_read_c2c(integer(), integer(), term()) -> list(map()).
safe_read_c2c(Uid, Limit, LastServerTs) ->
    try
        %% 注意：read_msg/3 只读 to_id = Uid 的消息（收件箱语义，被离线暂存等复用），
        %% 这里需要双向（自己发/自己收）才能聚合出仅由自己发起、对端未回的会话。
        msg_c2c_ds:read_msg_for_conversation(Uid, Limit, LastServerTs)
    catch
        _:Reason ->
            ?LOG(error, "读取 c2c 会话失败: ~p", [Reason]),
            []
    end.

-spec safe_read_c2g(integer(), integer(), term()) -> list(map()).
safe_read_c2g(Uid, Limit, LastServerTs) ->
    try
        msg_c2g_ds:read_msg(Uid, Limit, LastServerTs)
    catch
        _:Reason ->
            ?LOG(error, "读取 c2g 会话失败: ~p", [Reason]),
            []
    end.

-spec merge_latest_conversations(integer(), list(map()), list(map())) -> list(map()).
merge_latest_conversations(Uid, C2cMsgs, C2gMsgs) ->
    Entries =
        lists:filtermap(fun(Msg) -> normalize_c2c_conversation(Uid, Msg) end, C2cMsgs) ++
            lists:filtermap(fun normalize_c2g_conversation/1, C2gMsgs),
    ConversationsByKey = lists:foldl(
        fun(Entry, Acc) ->
            Key = {
                maps:get(<<"conversation_id">>, Entry, <<>>),
                maps:get(<<"conversation_type">>, Entry, <<"c2c">>)
            },
            case maps:get(Key, Acc, undefined) of
                undefined ->
                    maps:put(Key, Entry, Acc);
                Existing ->
                    ExistingTs = maps:get(<<"server_ts">>, Existing, 0),
                    EntryTs = maps:get(<<"server_ts">>, Entry, 0),
                    case EntryTs >= ExistingTs of
                        true -> maps:put(Key, Entry, Acc);
                        false -> Acc
                    end
            end
        end,
        #{},
        Entries
    ),
    maps:values(ConversationsByKey).

-spec normalize_c2c_conversation(integer(), map()) -> {true, map()} | false.
normalize_c2c_conversation(Uid, Msg) ->
    FromId = maps:get(<<"from_id">>, Msg, 0),
    ToId = maps:get(<<"to_id">>, Msg, 0),
    case peer_uid(Uid, FromId, ToId) of
        {ok, PeerId} ->
            Ts = extract_server_ts(Msg),
            LastMsg = normalize_payload(maps:get(<<"payload">>, Msg, #{})),
            {true, #{
                <<"conversation_id">> => PeerId,
                <<"conversation_type">> => <<"c2c">>,
                <<"server_ts">> => Ts,
                <<"last_msg_id">> => maps:get(<<"msg_id">>, Msg, <<>>),
                <<"last_msg">> => LastMsg
            }};
        error ->
            false
    end.

%% 根据当前 Uid 与消息的 from_id/to_id 推断对端 UID。
%% 双向查询后，同一个 C2C 会话可能同时出现「自己发的」和「自己收的」两类消息：
%%   - 自己发出的：from_id = Uid，对端 = to_id
%%   - 自己收到的：to_id   = Uid，对端 = from_id
%% 兜底过滤掉与当前用户无关的脏数据。
-spec peer_uid(integer(), integer(), integer()) -> {ok, integer()} | error.
peer_uid(Uid, FromId, ToId) when
    is_integer(Uid),
    is_integer(FromId),
    FromId > 0,
    is_integer(ToId),
    ToId > 0
->
    if
        Uid =:= ToId -> {ok, FromId};
        Uid =:= FromId -> {ok, ToId};
        true -> error
    end;
peer_uid(_, _, _) ->
    error.

-spec normalize_c2g_conversation(map()) -> {true, map()} | false.
normalize_c2g_conversation(Msg) ->
    case maps:get(<<"to_id">>, Msg, 0) of
        GroupId when is_integer(GroupId), GroupId > 0 ->
            ConversationId = GroupId,
            Ts = extract_server_ts(Msg),
            LastMsg = normalize_payload(maps:get(<<"payload">>, Msg, #{})),
            {true, #{
                <<"conversation_id">> => ConversationId,
                <<"conversation_type">> => <<"c2g">>,
                <<"server_ts">> => Ts,
                <<"last_msg_id">> => maps:get(<<"msg_id">>, Msg, <<>>),
                <<"last_msg">> => LastMsg
            }};
        _ ->
            false
    end.

-spec extract_server_ts(map()) -> integer().
extract_server_ts(Msg) ->
    Value = maps:get(<<"server_ts">>, Msg, maps:get(<<"created_at">>, Msg, 0)),
    to_millisecond(Value).

-spec to_millisecond(term()) -> integer().
to_millisecond(Value) when is_integer(Value), Value >= 0 ->
    Value;
to_millisecond(Value) when is_binary(Value); is_list(Value) ->
    ValueBin = elib_cnv:safe_to_binary(Value),
    Parsed =
        case catch elib_dt:rfc3339_to(ValueBin, millisecond) of
            Ts when is_integer(Ts), Ts >= 0 ->
                Ts;
            _ ->
                case catch binary_to_integer(ValueBin) of
                    Int when is_integer(Int), Int >= 0 -> Int;
                    _ -> 0
                end
        end,
    Parsed;
to_millisecond(_) ->
    0.

-spec normalize_payload(term()) -> map().
normalize_payload(Payload) when is_map(Payload) ->
    Payload;
normalize_payload(Payload) when is_binary(Payload) ->
    case catch jsone:decode(Payload, [{object_format, map}]) of
        Map when is_map(Map) -> Map;
        _ -> #{}
    end;
normalize_payload(_) ->
    #{}.

-spec resolve_conversation_key(map()) -> {ok, {binary(), binary()}} | error.
resolve_conversation_key(Item) ->
    case
        {
            maps:get(<<"conversation_id">>, Item, undefined),
            maps:get(<<"conversation_type">>, Item, undefined)
        }
    of
        {ConversationId, ConversationType} when
            is_binary(ConversationId),
            ConversationId =/= <<>>,
            is_binary(ConversationType),
            ConversationType =/= <<>>
        ->
            {ok, {ConversationId, ConversationType}};
        _ ->
            case maps:get(<<"from_id">>, Item, 0) of
                FromId when is_integer(FromId), FromId > 0 ->
                    {ok, {FromId, <<"c2c">>}};
                _ ->
                    error
            end
    end.

-spec attach_pin_state(integer(), list(map())) -> list(map()).
attach_pin_state(Uid, Conversations) ->
    [
        Conversation#{
            <<"is_pinned">> =>
                conversation_pin_logic:is_pinned(
                    Uid,
                    maps:get(<<"conversation_id">>, Conversation, <<>>),
                    maps:get(<<"conversation_type">>, Conversation, <<"c2c">>)
                )
        }
     || Conversation <- Conversations
    ].

-spec sort_conversations(list(map())) -> list(map()).
sort_conversations(Conversations) ->
    lists:sort(
        fun(A, B) ->
            PinA = maps:get(<<"is_pinned">>, A, false),
            PinB = maps:get(<<"is_pinned">>, B, false),
            TsA = maps:get(<<"server_ts">>, A, 0),
            TsB = maps:get(<<"server_ts">>, B, 0),
            case {PinA, PinB} of
                {true, false} ->
                    true;
                {false, true} ->
                    false;
                _ when TsA > TsB -> true;
                _ when TsA < TsB -> false;
                _ ->
                    IdA = maps:get(<<"conversation_id">>, A, <<>>),
                    IdB = maps:get(<<"conversation_id">>, B, <<>>),
                    IdA =< IdB
            end
        end,
        Conversations
    ).

%% ===================================================================
%% EUnit tests.
%% ===================================================================
