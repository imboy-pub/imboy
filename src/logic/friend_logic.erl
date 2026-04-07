-module(friend_logic).
%%%
%  friend 业务逻辑模块
%%%
-export([add_friend/4]).
-export([add_friend/5]).
-export([confirm_friend/4]).
-export([confirm_friend_resp/2]).
-export([delete_friend/2]).
-export([move_to_category/3]).
-export([information/2]).

-include("log.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 添加好友
%% 发送好友请求，存储消息并通知对方
%% @param CurrentUid 当前用户ID
%% @param To 对方用户ID（HashID格式）
%% @param Payload 消息内容映射
%% @param CreatedAt 创建时间（RFC3339格式或时间戳）
%% @return ok | {error, ErrorCode, ErrorMsg}
-spec add_friend(integer(),
                 binary(),
                 map(),
                 binary() | integer()) -> ok | {error, binary(), binary()}.
add_friend(_, undefined, _, _) ->
    {error, <<"Parameter error">>, <<"to">>};
add_friend(_, _, undefined, _) ->
    {error, <<"Parameter error">>, <<"payload">>};
add_friend(_, _, _, undefined) ->
    {error, <<"Parameter error">>, <<"created_at">>};
add_friend(CurrentUid, To, Payload, CreatedAt) ->
    %% 统一转换时间戳为 RFC3339 binary 格式
    CreatedAt2 = elib_dt:to_rfc3339(CreatedAt),
    do_add_friend(CurrentUid, To, Payload, CreatedAt2).

%% @doc 兼容旧入口：保留旧 MsgId 参数但复用当前实现
-spec add_friend(binary(), integer(), binary(), map(), binary() | integer()) ->
    ok | {error, binary(), binary()}.
add_friend(_MsgId, CurrentUid, To, Payload, CreatedAt) ->
    add_friend(CurrentUid, To, Payload, CreatedAt).


%% @doc 内部函数：实际执行添加好友操作
-spec do_add_friend(integer(), binary(), map(), binary()) -> ok.
do_add_friend(CurrentUid, To, Payload, CreatedAt) ->
    ToId = ec_cnv:to_integer(To),
    NowTs = elib_dt:now(),
    From = CurrentUid,
    MsgId = <<"af_", From/binary, "_", To/binary>>,
    % ?DEBUG_LOG([is_binary(Payload), Payload]),
    % v2.0: S2C 消息使用 action 字段，直接作为参数传递
    Action = <<"apply_friend">>,
    Payload2 = maps:without([<<"action">>], Payload),
    % 存储消息（v2.0: 使用 write_msg/8 API）
    _ = msg_s2c_ds:write_msg(CreatedAt, MsgId, Payload2, CurrentUid, ToId, NowTs, Action, <<>>),
    Msg = message_ds:assemble_msg(<<"S2C">>, From, To, Payload2, MsgId, <<>>, Action, null),
    % ?DEBUG_LOG(Msg),
    MsLi = elib_retry_config:intervals(<<"s2c">>),
    message_ds:send_next(ToId, MsgId, jsone:encode(Msg, [native_utf8]), MsLi),
    ok.


%% @doc 确认好友请求
%% 接受好友请求，建立双向好友关系
%% @param CurrentUid 当前用户ID
%% @param From 发起方用户ID（HashID格式）
%% @param To 接收方用户ID（HashID格式）
%% @param Payload 包含好友设置信息的映射
%% @return {ok, FromID, Remark2, Source} | {error, ErrorCode, ErrorMsg}
-spec confirm_friend(integer(), binary(), binary(), binary()) -> {ok, integer(), binary(), binary()} | {error, binary(), binary()}.
confirm_friend(_, undefined, _, _) ->
    {error, <<"Parameter error">>, <<"from">>};
confirm_friend(_, _, undefined, _) ->
    {error, <<"Parameter error">>, <<"to">>};
confirm_friend(_, _, _, undefined) ->
    {error, <<"Parameter error">>, <<"payload">>};
confirm_friend(_MsgId, CurrentUid, From, Payload)
    when is_integer(CurrentUid), is_binary(From), is_binary(Payload) ->
    To = CurrentUid,
    confirm_friend(CurrentUid, From, To, Payload);
confirm_friend(CurrentUid, From, To, Payload) ->
    FromID = ec_cnv:to_integer(From),
    ToID = ec_cnv:to_integer(To),
    NowTs = elib_dt:now(),
    Payload2 = jsone:decode(Payload, [{object_format, map}]),

    FromSetting = maps:get(<<"from">>, Payload2, #{}),
    % Remark1 为 from 对 to 定义的 remark
    Remark1 = maps:get(<<"remark">>, FromSetting, <<>>),
    % ToTag 为 from 对 to 定义的 tag
    ToTag = maps:get(<<"tag">>, FromSetting, <<>>),
    Source = maps:get(<<"source">>, FromSetting, <<>>),
    FromToIsFriend = friend_ds:is_friend(FromID, ToID),
    % 使用 DS 层接口写入好友关系
    friend_ds:confirm_friend(FromToIsFriend, FromID, ToID, Remark1, FromSetting#{<<"is_from">> => 1}, ToTag, NowTs),

    ToSetting = maps:get(<<"to">>, Payload2, #{}),
    ToFromIsFriend = friend_ds:is_friend(ToID, FromID),
    % Remark2 为 to 对 from 定义的 remark
    Remark2 = maps:get(<<"remark">>, ToSetting, <<>>),
    % FromTag 为 to 对 from 定义的 tag
    FromTag = maps:get(<<"tag">>, ToSetting, <<>>),
    % 使用 DS 层接口写入好友关系
    friend_ds:confirm_friend(ToFromIsFriend,
                               ToID,
                               FromID,
                               Remark2,
                               ToSetting#{<<"source">> => Source},
                               FromTag,
                               NowTs),

    % 因为是 ToID 通过API确认的，所以只需要给FromID 发送消息
    MsgId = <<"afc_", From/binary, "_", To/binary>>,
    %% v2.0: S2C 消息使用 action 字段，直接作为参数传递
    Action = <<"apply_friend_confirm">>,
    Payload6 = Payload2#{
        <<"is_from">> => 1,
        <<"source">> => Source
    },

    % 存储消息（v2.0: 使用 write_msg/8 API）
    _ = msg_s2c_ds:write_msg(NowTs, MsgId, Payload6, CurrentUid, FromID, NowTs, Action, <<>>),

    % 这里的From To 需要对调，离线消息需要对调
    Msg = message_ds:assemble_msg(<<"S2C">>, To, From, Payload6, MsgId, <<>>, Action, null),

    % ?DEBUG_LOG(Msg),
    MsLi = elib_retry_config:intervals(<<"s2c">>),
    message_ds:send_next(FromID, MsgId, jsone:encode(Msg, [native_utf8]), MsLi),

    if
        ToTag == <<>> ->
            ok;
        true ->
            ToTag2 = [ I || I <- binary:split(ToTag, <<",">>, [global]), I /= <<>> ],
            _ = user_tag_relation_logic:add(FromID, 2, ToID, ToTag2),
            ok
    end,
    if
        FromTag == <<>> ->
            ok;
        true ->
            FromTag2 = [ I || I <- binary:split(FromTag, <<",">>, [global]), I /= <<>> ],
            _ = user_tag_relation_logic:add(ToID, 2, FromID, FromTag2),
            ok
    end,
    % 兼容旧缓存键，同时清理当前 friend_ds 使用的缓存键
    _ = friend_ds:invalidate_cache(FromID, ToID),
    imboy_cache:flush({is_friend, FromID, ToID}),
    imboy_cache:flush({is_friend, ToID, FromID}),
    {ok, FromID, Remark2, Source}.


%% @doc 生成确认好友响应
-spec confirm_friend_resp(integer(), binary()) -> map().
confirm_friend_resp(Uid, Remark) ->
    Column = <<"id,account,nickname,avatar,gender,sign,region,status">>,
    User = user_logic:find_by_id(Uid, Column),
    User#{
        <<"id">> => Uid,
        <<"remark">> => Remark
    }.


%% @doc 删除好友
%% 删除好友关系，清理相关缓存
%% @param CurrentUid 当前用户ID
%% @param Uid 要删除的好友用户ID（HashID格式或原始ID）
%% @return ok
-spec delete_friend(integer(), binary() | integer()) -> ok.
delete_friend(CurrentUid, Uid) when is_binary(Uid) ->
    Uid2 = ec_cnv:to_integer(Uid),
    delete_friend(CurrentUid, Uid2);
delete_friend(CurrentUid, Uid) ->
    _ = friend_ds:delete(CurrentUid, Uid),
    % 为了简单，删除好友关系清理两个缓存
    imboy_cache:flush({is_friend, CurrentUid, Uid}),
    imboy_cache:flush({is_friend, Uid, CurrentUid}),
    ok.


%% @doc 移动好友到分组
-spec move_to_category(integer(), binary() | integer(), integer()) -> ok.
move_to_category(CurrentUid, Uid, CategoryId) when is_binary(Uid) ->
    Uid2 = ec_cnv:to_integer(Uid),
    move_to_category(CurrentUid, Uid2, CategoryId);
move_to_category(CurrentUid, Uid, CategoryId) ->
    _ = friend_ds:move_to_category(CurrentUid, Uid, CategoryId),
    ok.


%% @doc 获取好友详细信息
%% @param CurrentUid 当前用户ID
%% @param Uid 好友用户ID
%% @return map() 好友详细信息，包含 is_friend 字段
-spec information(integer(), integer()) -> map().
information(CurrentUid, Uid) ->
    % 使用 DS 层接口检查是否是好友关系
    case friend_ds:is_friend_fields(CurrentUid, Uid, [<<"id">>]) of
        {true, _} ->
            % 获取用户详细信息
            case user_logic:find_by_id(Uid) of
                UserMap when map_size(UserMap) > 0 ->
                    % 过滤敏感信息，只返回好友可见的字段
                    UserMap#{
                        <<"is_friend">> => true
                    };
                _ ->
                    % 用户不存在
                    #{}
            end;
        {false, _} ->
            % 不是好友关系
            #{}
    end.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
