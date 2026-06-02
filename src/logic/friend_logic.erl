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
%% @param To 对方用户ID（整数）
%% @param Payload 消息内容映射
%% @param CreatedAt 创建时间（RFC3339格式或时间戳）
%% @return ok | {error, ErrorCode, ErrorMsg}
-spec add_friend(
    integer(),
    binary(),
    binary() | map(),
    binary() | integer()
) -> ok | {error, binary(), binary()}.
add_friend(_, undefined, _, _) ->
    {error, <<"Parameter error">>, <<"to">>};
add_friend(_, _, undefined, _) ->
    {error, <<"Parameter error">>, <<"payload">>};
add_friend(_, _, _, undefined) ->
    {error, <<"Parameter error">>, <<"created_at">>};
add_friend(CurrentUid, To, Payload, CreatedAt) ->
    %% 统一转换 To 为 binary（JSON 解析可能返回 integer）
    ToBin = ec_cnv:to_binary(To),
    %% 统一转换时间戳为 RFC3339 binary 格式
    CreatedAt2 = elib_dt:to_rfc3339(CreatedAt),
    do_add_friend(CurrentUid, ToBin, Payload, CreatedAt2).

%% @doc 兼容旧入口：保留旧 MsgId 参数但复用当前实现
-spec add_friend(binary(), integer(), binary(), binary(), binary() | integer()) ->
    ok | {error, binary(), binary()}.
add_friend(_MsgId, CurrentUid, To, Payload, CreatedAt) ->
    add_friend(CurrentUid, To, Payload, CreatedAt).

%% @doc 内部函数：实际执行添加好友操作
%%
%% T3.4：委托 friend_agg 守护好友申请状态机不变量（state-gating）。
%% 现状为无状态消息流；接线后由领域聚合拒绝「对方已是好友」「您已拉黑对方」
%% 的重复申请。pending 态在现状为瞬态 S2C 消息（未持久化），故 already_requested
%% 不在本层判定——待 pending-store 落地后补 accept/reject 接线（见 T3.4 后续）。
-spec do_add_friend(integer(), binary(), binary(), binary()) ->
    ok | {error, binary(), binary()}.
do_add_friend(CurrentUid, To, Payload, CreatedAt) ->
    ToId = ec_cnv:to_integer(To),
    FromBin = ec_cnv:to_binary(CurrentUid),
    %% 由现状联合查询（好友 + 黑名单）派生 friend_agg 初始状态
    {IsFriend, InDenylist} = friend_ds:check_relationship(CurrentUid, ToId),
    Friendship = friend_agg:rehydrate(#{
        <<"from_user_id">> => FromBin,
        <<"to_user_id">> => To,
        <<"status">> => add_friend_status(IsFriend, InDenylist)
    }),
    case friend_agg:request(Friendship) of
        {error, already_friends} ->
            {error, <<"already_friends">>, <<"对方已是您的好友"/utf8>>};
        {error, blocked} ->
            {error, <<"blocked">>, <<"您已拉黑对方，无法发起好友申请"/utf8>>};
        {ok, _Friendship2, _Events} ->
            send_apply_friend(CurrentUid, To, ToId, FromBin, Payload, CreatedAt)
    end.

%% @doc 由现状联合查询派生 friend_agg 初始状态：拉黑优先于好友。
%% check_relationship/2 的 InDenylist 表达「from 已拉黑 to」，对应 from→to 关系 blocked。
-spec add_friend_status(boolean(), boolean()) -> friend_agg:status().
add_friend_status(_IsFriend, true) -> blocked;
add_friend_status(true, false) -> friends;
add_friend_status(false, false) -> none.

%% @doc 发送好友申请 S2C 消息（行为逐字保留自原 do_add_friend 尾部）。
-spec send_apply_friend(integer(), binary(), integer(), binary(), binary() | map(), binary()) ->
    ok.
send_apply_friend(CurrentUid, To, ToId, FromBin, Payload, CreatedAt) ->
    NowTs = elib_dt:now(),
    MsgId = <<"af_", FromBin/binary, "_", To/binary>>,
    % v2.0: S2C 消息使用 action 字段，直接作为参数传递
    Action = <<"apply_friend">>,
    PayloadMap =
        if
            is_map(Payload) -> Payload;
            true -> jsone:decode(Payload, [{object_format, map}])
        end,
    Payload2 = maps:without([<<"action">>], PayloadMap),
    % 存储消息（v2.0: 使用 write_msg/8 API）
    msg_s2c_ds:write_msg(CreatedAt, MsgId, Payload2, CurrentUid, ToId, NowTs, Action, <<>>),
    %% FIX: 使用 FromBin (binary) 而非 CurrentUid (integer)，确保客户端收到字符串类型的 from
    Msg = message_ds:assemble_msg(<<"S2C">>, FromBin, To, Payload2, MsgId, <<>>, Action, null),
    MsLi = elib_retry_config:intervals(<<"s2c">>),
    message_ds:send_next(ToId, MsgId, jsone:encode(Msg, [native_utf8]), MsLi),
    ok.

%% @doc 确认好友请求
%% 接受好友请求，建立双向好友关系
%% @param CurrentUid 当前用户ID
%% @param From 发起方用户ID（整数）
%% @param To 接收方用户ID（整数）
%% @param Payload 包含好友设置信息的映射
%% @return {ok, FromID, Remark2, Source} | {error, ErrorCode, ErrorMsg}
-spec confirm_friend(integer(), binary(), binary(), binary()) ->
    {ok, integer(), binary(), binary()} | {error, binary(), binary()}.
confirm_friend(_, undefined, _, _) ->
    {error, <<"Parameter error">>, <<"from">>};
confirm_friend(_, _, undefined, _) ->
    {error, <<"Parameter error">>, <<"to">>};
confirm_friend(_, _, _, undefined) ->
    {error, <<"Parameter error">>, <<"payload">>};
confirm_friend(_MsgId, CurrentUid, From, Payload) when
    is_integer(CurrentUid), is_binary(From), is_binary(Payload)
->
    To = CurrentUid,
    confirm_friend(CurrentUid, From, To, Payload);
confirm_friend(CurrentUid, From, To, Payload) ->
    FromBin = ec_cnv:to_binary(From),
    ToBin = ec_cnv:to_binary(To),
    FromID = ec_cnv:to_integer(From),
    ToID = ec_cnv:to_integer(To),
    NowTs = elib_dt:now(),
    Payload2 =
        if
            is_map(Payload) -> Payload;
            true -> jsone:decode(Payload, [{object_format, map}])
        end,

    FromSetting = maps:get(<<"from">>, Payload2, #{}),
    % Remark1 为 from 对 to 定义的 remark
    Remark1 = maps:get(<<"remark">>, FromSetting, <<>>),
    % ToTag 为 from 对 to 定义的 tag
    ToTag = maps:get(<<"tag">>, FromSetting, <<>>),
    Source = maps:get(<<"source">>, FromSetting, <<>>),
    FromToIsFriend = friend_ds:is_friend(FromID, ToID),
    % 使用 DS 层接口写入好友关系
    friend_ds:confirm_friend(
        FromToIsFriend, FromID, ToID, Remark1, FromSetting#{<<"is_from">> => 1}, ToTag, NowTs
    ),

    ToSetting = maps:get(<<"to">>, Payload2, #{}),
    ToFromIsFriend = friend_ds:is_friend(ToID, FromID),
    % Remark2 为 to 对 from 定义的 remark
    Remark2 = maps:get(<<"remark">>, ToSetting, <<>>),
    % FromTag 为 to 对 from 定义的 tag
    FromTag = maps:get(<<"tag">>, ToSetting, <<>>),
    % 使用 DS 层接口写入好友关系
    friend_ds:confirm_friend(
        ToFromIsFriend,
        ToID,
        FromID,
        Remark2,
        ToSetting#{<<"source">> => Source},
        FromTag,
        NowTs
    ),

    % 因为是 ToID 通过API确认的，所以只需要给FromID 发送消息
    MsgId = <<"afc_", FromBin/binary, "_", ToBin/binary>>,
    %% v2.0: S2C 消息使用 action 字段，直接作为参数传递
    Action = <<"apply_friend_confirm">>,
    Payload6 = Payload2#{
        <<"is_from">> => 1,
        <<"source">> => Source
    },

    % 存储消息（v2.0: 使用 write_msg/8 API）
    _ = msg_s2c_ds:write_msg(NowTs, MsgId, Payload6, CurrentUid, FromID, NowTs, Action, <<>>),

    % 这里的From To 需要对调，离线消息需要对调
    %% FIX: 将 To (integer) 转为 binary，确保客户端收到字符串类型
    ToBin = ec_cnv:to_binary(To),
    Msg = message_ds:assemble_msg(<<"S2C">>, ToBin, From, Payload6, MsgId, <<>>, Action, null),

    % ?DEBUG_LOG(Msg),
    MsLi = elib_retry_config:intervals(<<"s2c">>),
    message_ds:send_next(FromID, MsgId, jsone:encode(Msg, [native_utf8]), MsLi),

    if
        ToTag == <<>> ->
            ok;
        true ->
            ToTag2 = [I || I <- binary:split(ToTag, <<",">>, [global]), I /= <<>>],
            _ = user_tag_relation_logic:add(FromID, 2, ToID, ToTag2),
            ok
    end,
    if
        FromTag == <<>> ->
            ok;
        true ->
            FromTag2 = [I || I <- binary:split(FromTag, <<",">>, [global]), I /= <<>>],
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
%% @param Uid 要删除的好友用户ID（整数）
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
