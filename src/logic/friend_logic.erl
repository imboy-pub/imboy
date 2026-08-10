-module(friend_logic).
%%%
%  friend 业务逻辑模块
%%%
-export([add_friend/4]).
-export([add_friend/5]).
-export([confirm_friend/4]).
-export([confirm_friend_resp/2]).
-export([reject_friend/2]).
-export([delete_friend/2]).
-export([move_to_category/3]).
-export([information/2]).
-export([is_friend/3]).
-export([list_by_uid/1]).
-export([get_friend_information/2]).
-export([change_remark/3]).

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
    binary() | undefined,
    binary() | map() | undefined,
    binary() | integer() | undefined
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
%% T3.4（含余项 pending-store）：委托 friend_agg 守护好友申请状态机不变量。
%% friend_ds:pending_status/2 派生 none|pending|friends|blocked（拉黑>好友>申请中）。
%% 接线后拒绝「对方已是好友」「您已拉黑对方」「已发送过申请」的重复申请；
%% ok 路径持久化 pending 行（status=0）供后续 accept/reject gating。
-spec do_add_friend(integer(), binary(), binary(), binary()) ->
    ok | {error, binary(), binary()}.
do_add_friend(CurrentUid, To, Payload, CreatedAt) ->
    ToId = ec_cnv:to_integer(To),
    case check_add_way_allowed(ToId, Payload) of
        ok ->
            do_add_friend_checked(CurrentUid, To, ToId, Payload, CreatedAt);
        {error, Code, Msg} ->
            {error, Code, Msg}
    end.

%% @doc 读侧强制目标用户的加友方式隐私开关（QA #19）：
%% 申请 payload 的 from.source 标识加友途径（mobile/qrcode/...），
%% 目标用户关闭对应开关时拒绝申请。开关缺省 = 允许。
-spec check_add_way_allowed(integer(), binary() | map()) ->
    ok | {error, binary(), binary()}.
check_add_way_allowed(ToId, Payload) ->
    PayloadMap =
        if
            is_map(Payload) -> Payload;
            true -> jsone:decode(Payload, [{object_format, map}])
        end,
    FromSetting = maps:get(<<"from">>, PayloadMap, #{}),
    %% 仅 mobile/qrcode 两个受控途径才查设置，其余途径零开销放行
    case maps:get(<<"source">>, FromSetting, <<>>) of
        <<"mobile">> ->
            check_setting_allows(
                ToId, <<"allow_add_by_phone">>, <<"对方已关闭通过手机号添加好友"/utf8>>
            );
        <<"qrcode">> ->
            check_setting_allows(
                ToId, <<"allow_add_by_qr">>, <<"对方已关闭通过二维码添加好友"/utf8>>
            );
        _ ->
            ok
    end.

-spec check_setting_allows(integer(), binary(), binary()) ->
    ok | {error, binary(), binary()}.
check_setting_allows(ToId, Key, DeniedMsg) ->
    case maps:get(Key, user_setting_ds:find_by_uid(ToId), true) of
        false -> {error, <<"add_way_disabled">>, DeniedMsg};
        _ -> ok
    end.

-spec do_add_friend_checked(integer(), binary(), integer(), binary(), binary()) ->
    ok | {error, binary(), binary()}.
do_add_friend_checked(CurrentUid, To, ToId, Payload, CreatedAt) ->
    FromBin = ec_cnv:to_binary(CurrentUid),
    Friendship = friend_agg:rehydrate(#{
        <<"from_user_id">> => FromBin,
        <<"to_user_id">> => To,
        <<"status">> => friend_ds:pending_status(CurrentUid, ToId)
    }),
    case friend_agg:request(Friendship) of
        {error, already_requested} ->
            {error, <<"already_requested">>, <<"您已发送过好友申请，请等待对方确认"/utf8>>};
        {error, already_friends} ->
            {error, <<"already_friends">>, <<"对方已是您的好友"/utf8>>};
        {error, blocked} ->
            {error, <<"blocked">>, <<"您已拉黑对方，无法发起好友申请"/utf8>>};
        {ok, _Friendship2, _Events} ->
            %% 先持久化 pending 行（供 accept/reject gating + 去重），再发申请消息
            _ = friend_ds:insert_pending(CurrentUid, ToId, Payload, elib_dt:now()),
            send_apply_friend(CurrentUid, To, ToId, FromBin, Payload, CreatedAt)
    end.

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
    _ = msg_s2c_ds:write_msg(CreatedAt, MsgId, Payload2, CurrentUid, ToId, NowTs, Action, <<>>),
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
-spec confirm_friend(integer(), binary() | undefined, binary() | undefined, binary() | undefined) ->
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
    confirm_friend(CurrentUid, From, integer_to_binary(CurrentUid), Payload);
confirm_friend(CurrentUid, From, To, Payload) ->
    FromBin = ec_cnv:to_binary(From),
    ToBin = ec_cnv:to_binary(To),
    FromID = ec_cnv:to_integer(From),
    ToID = ec_cnv:to_integer(To),
    %% T3.4 余项：accept gating — 仅当存在 From→To pending(status=0) 申请时可确认；
    %% 否则拒 no_pending_request（落地「无 pending 申请时拒绝 accept」语义）。
    Pending = friend_agg:rehydrate(#{
        <<"from_user_id">> => FromBin,
        <<"to_user_id">> => ToBin,
        <<"status">> => friend_ds:pending_status(FromID, ToID)
    }),
    case friend_agg:accept(Pending) of
        {error, no_pending_request} ->
            {error, <<"no_pending_request">>, <<"无待确认的好友申请"/utf8>>};
        {ok, _AccF, _AccEv} ->
            confirm_friend_do(CurrentUid, From, To, Payload, FromBin, ToBin, FromID, ToID)
    end.

%% @doc confirm_friend 主体（accept gating 通过后执行）：写双向好友关系 +
%% 发确认消息 + tag + cache flush。From→To 行经 confirm_friend upsert 由
%% pending(0) promote 为 friends(1)；To→From 行新插 status=1。
-spec confirm_friend_do(
    integer(), binary(), binary(), binary(), binary(), binary(), integer(), integer()
) -> {ok, integer(), binary(), binary()}.
confirm_friend_do(CurrentUid, From, To, Payload, FromBin, ToBin, FromID, ToID) ->
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
%%
%% 历史问题：旧实现只返回 user 表基础列，缺少 is_friend / last_seen_at，
%% 且 `status` 是 user 表的静态列而非实时在线状态。前端 accept 流程
%% （confirm_new_friend_provider._storeContactInfo）依赖这些字段落库，
%% 字段缺失会导致 contact.is_friend 不被置 1 → 通讯录查不到新好友。
%%
%% 现补充：
%%   - is_friend = 1（accept 语义即已是好友，省去前端推断）
%%   - peerId = Uid（兼容前端 ContactModel.fromMap 同时支持 id/peer_id）
%%   - last_seen_at 由 batch_online_state 兜底为 <<>>（user 表无此列，
%%     该列属 user_friend；勿在 Column 中引用，SQL 会报错被吞成空 map 致崩溃）
%%   - status 经 batch_online_state 计算为实时 online/offline
-spec confirm_friend_resp(integer(), binary()) -> map().
confirm_friend_resp(Uid, Remark) ->
    Column = <<"id,account,nickname,avatar,gender,sign,region">>,
    User0 = user_logic:find_by_id(Uid, Column),
    %% 复用 batch_online_state 计算实时在线状态（与 friend/list 一致）
    [User1] = user_ds:batch_online_state([User0]),
    User1#{
        <<"id">> => Uid,
        <<"peerId">> => Uid,
        <<"remark">> => Remark,
        <<"is_friend">> => 1
    }.

%% @doc 拒绝好友申请（T3.4 余项）：仅当存在 From→CurrentUid 的 pending(status=0)
%% 申请时可拒绝，删除该 pending 行；否则拒 no_pending_request。
%% From 为申请发起方，CurrentUid 为接收方（拒绝者）。
-spec reject_friend(integer(), binary() | integer()) -> ok | {error, binary(), binary()}.
reject_friend(CurrentUid, From) ->
    FromID = ec_cnv:to_integer(From),
    Pending = friend_agg:rehydrate(#{
        <<"from_user_id">> => ec_cnv:to_binary(FromID),
        <<"to_user_id">> => ec_cnv:to_binary(CurrentUid),
        <<"status">> => friend_ds:pending_status(FromID, CurrentUid)
    }),
    case friend_agg:reject(Pending) of
        {error, no_pending_request} ->
            {error, <<"no_pending_request">>, <<"无待拒绝的好友申请"/utf8>>};
        {ok, _F, _Ev} ->
            friend_ds:delete_pending(FromID, CurrentUid)
    end.

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

%% @doc 查询好友关系及指定字段
%% Handler 层通过此接口查询好友状态，不直接访问 friend_ds
%% @param FromUid 发起方用户ID
%% @param ToUid 目标用户ID
%% @param Field 需要查询的字段（如 <<"remark">>）
%% @return {boolean(), binary()}
-spec is_friend(integer(), integer(), binary()) -> {boolean(), binary()}.
is_friend(FromUid, ToUid, Field) ->
    friend_ds:is_friend(FromUid, ToUid, Field).

%% @doc 查询当前用户的好友列表（分页）
-spec list_by_uid(integer()) -> [map()].
list_by_uid(Uid) ->
    friend_ds:page_by_uid(Uid).

%% @doc 获取目标用户的设置信息，用于好友详情页展示
%% CurrentUid 为当前用户ID（保留供后续权限校验扩展），Uid 为目标好友ID
-spec get_friend_information(integer(), binary() | integer()) -> map().
get_friend_information(_CurrentUid, Uid) ->
    user_setting_ds:find_by_uid(Uid).

%% @doc 修改当前用户对指定好友的备注名
-spec change_remark(integer(), integer(), binary()) -> {ok, integer()} | {error, any()}.
change_remark(FromUid, ToUid, Remark) ->
    friend_ds:change_remark(FromUid, ToUid, Remark).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
