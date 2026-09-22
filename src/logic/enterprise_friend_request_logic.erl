-module(enterprise_friend_request_logic).

%%%
% enterprise_friend_request_logic 是 EPGZ-03 INT-11「好友申请（只发起）」
% 的业务逻辑层：OA 以同 Org 已映射 active Human 的名义代发起好友申请。
%
% 复用现有申请流（checkpoint 冻结）：
%   * 状态机 friend_agg:request/1（none --request--> pending）；
%   * pending 持久化 = user_friend 表 status=0 行（uk_fromuid_touid 幂等）；
%   * 人工审批流零改动：接收方在现有 friend_logic:confirm_friend /
%     reject_friend 中手动 accept/reject（消费同一 pending 真源）。
%
% 【绝对边界】本模块只发起申请：
%   * 不实现、不暴露 accept/confirm/reject/delete/list-all-friends 任何形态
%     （测试以导出面清单断言负例）；
%   * 创建结果只可能是 pending 态（friend_agg:request 的唯一成功跃迁），
%     不存在任何自动接受/确认路径。
%
% 输入契约（handler 壳由 A0 W4 接线；sender/target 字段语义 = OA 侧
% external_user_id，与 INT-04/05/06 成员语义一致，详见 checkpoint handoff）：
%%%

-export([
    create_request_tx/3,
    notify_request/1
]).

-define(MAX_EXTERNAL_ID_LEN, 256).
-define(MAX_GREETING_LEN, 500).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc INT-11：代发起好友申请（只创建 pending，绝不自动接受）。
%% Input（map）：
%%   sender_user_id   必填 binary（external_user_id，本 app 映射的 active Human）
%%   target_user_id   必填 binary（同上，且 ≠ sender）
%%   greeting         可选 binary ≤500（申请留言，缺省空）
%% 错误映射：
%%   invalid_request           参数非法 / 自发申请 / already_friends /
%%                             already_requested / blocked
%%   identity_not_mapped       sender 或 target 未映射（Detail 指明哪侧）
%% 返回 {ok, #{request_status => pending, sender_user_id, target_user_id}}。
-spec create_request_tx(any(), map(), map()) -> {ok, map()} | {error, {binary(), term()}}.
create_request_tx(Conn, Ctx, Input) when is_map(Input) ->
    OrgId = maps:get(organization_id, Ctx),
    AppId = maps:get(application_id, Ctx),
    SenderExt = maps:get(sender_user_id, Input, undefined),
    TargetExt = maps:get(target_user_id, Input, undefined),
    Greeting = maps:get(greeting, Input, <<>>),
    case validate_input(SenderExt, TargetExt, Greeting) of
        ok ->
            resolve_and_create(Conn, OrgId, AppId, SenderExt, TargetExt, Greeting);
        {error, Detail} ->
            {error, {<<"invalid_request">>, Detail}}
    end;
create_request_tx(_Conn, _Ctx, _Other) ->
    {error, {<<"invalid_request">>, input_not_map}}.

%% @doc 提交后通知（best-effort，池化路径，由 handler 在事务提交之后调用；
%% 失败不影响申请结果）。复用现有申请消息形态 apply_friend（S2C），
%% source 标记 oa，目标端在现有客户端流程里看到并人工处理。
%% 参数：#{sender_uid => integer(), target_uid => integer(), greeting => binary()}
-spec notify_request(map()) -> ok.
notify_request(#{sender_uid := SenderUid, target_uid := TargetUid} = Info) ->
    Greeting = maps:get(greeting, Info, <<>>),
    FromBin = ec_cnv:to_binary(SenderUid),
    ToBin = ec_cnv:to_binary(TargetUid),
    NowTs = elib_dt:now(),
    MsgId = <<"af_", FromBin/binary, "_", ToBin/binary>>,
    Action = <<"apply_friend">>,
    Payload = #{
        <<"msg">> => Greeting,
        <<"from">> => #{<<"source">> => <<"oa">>}
    },
    _ = msg_s2c_ds:write_msg(
        NowTs, MsgId, Payload, SenderUid, TargetUid, NowTs, Action, <<>>
    ),
    Msg = message_ds:assemble_msg(<<"S2C">>, FromBin, ToBin, Payload, MsgId, <<>>, Action, null),
    MsLi = elib_retry_config:intervals(<<"s2c">>),
    message_ds:send_next(TargetUid, MsgId, jsone:encode(Msg, [native_utf8]), MsLi),
    ok.

%% ===================================================================
%% Internal Functions
%% ===================================================================

-spec validate_input(term(), term(), term()) -> ok | {error, term()}.
validate_input(SenderExt, TargetExt, Greeting) ->
    case valid_external(SenderExt) of
        false ->
            {error, invalid_sender_user_id};
        true ->
            case valid_external(TargetExt) of
                false ->
                    {error, invalid_target_user_id};
                true ->
                    case SenderExt =:= TargetExt of
                        true ->
                            {error, self_request};
                        false ->
                            case
                                is_binary(Greeting) andalso byte_size(Greeting) =< ?MAX_GREETING_LEN
                            of
                                true -> ok;
                                false -> {error, invalid_greeting}
                            end
                    end
            end
    end.

-spec valid_external(term()) -> boolean().
valid_external(E) ->
    is_binary(E) andalso byte_size(E) > 0 andalso byte_size(E) =< ?MAX_EXTERNAL_ID_LEN.

%% @doc 双侧解析（同 Org 本 app 映射的 active 行）+ 状态机 + pending 落库。
%% 解析失败精确指明 sender/target 哪一侧未映射（stable 码同一：identity_not_mapped）。
-spec resolve_and_create(any(), integer(), integer(), binary(), binary(), binary()) ->
    {ok, map()} | {error, {binary(), term()}}.
resolve_and_create(Conn, OrgId, AppId, SenderExt, TargetExt, Greeting) ->
    case resolve_one(Conn, OrgId, AppId, SenderExt) of
        {ok, SenderUid} ->
            case resolve_one(Conn, OrgId, AppId, TargetExt) of
                {ok, TargetUid} ->
                    gate_and_insert(Conn, SenderUid, TargetUid, SenderExt, TargetExt, Greeting);
                {error, not_mapped} ->
                    {error, {<<"identity_not_mapped">>, target_not_mapped}}
            end;
        {error, not_mapped} ->
            {error, {<<"identity_not_mapped">>, sender_not_mapped}}
    end.

-spec resolve_one(any(), integer(), integer(), binary()) ->
    {ok, integer()} | {error, not_mapped}.
resolve_one(Conn, OrgId, AppId, ExternalId) ->
    case enterprise_external_identity_repo:resolve_tx(Conn, OrgId, AppId, [ExternalId]) of
        {ok, [#{<<"user_id">> := Uid} | _]} ->
            {ok, Uid};
        {ok, []} ->
            {error, not_mapped};
        {error, _} ->
            {error, not_mapped}
    end.

%% @doc 既有申请流状态机（friend_agg:request，镜像 friend_logic:do_add_friend
%% 的 gating 顺序：blocked > friends > already_requested）+ pending 落库。
%% 成功只可能是 none -> pending（不存在自动接受跃迁）。
-spec gate_and_insert(any(), integer(), integer(), binary(), binary(), binary()) ->
    {ok, map()} | {error, {binary(), term()}}.
gate_and_insert(Conn, SenderUid, TargetUid, SenderExt, TargetExt, Greeting) ->
    FromBin = ec_cnv:to_binary(SenderUid),
    ToBin = ec_cnv:to_binary(TargetUid),
    Status = enterprise_friend_request_repo:pending_status_tx(Conn, SenderUid, TargetUid),
    Friendship = friend_agg:rehydrate(#{
        <<"from_user_id">> => FromBin,
        <<"to_user_id">> => ToBin,
        <<"status">> => Status
    }),
    case friend_agg:request(Friendship) of
        {error, already_requested} ->
            {error, {<<"invalid_request">>, already_requested}};
        {error, already_friends} ->
            {error, {<<"invalid_request">>, already_friends}};
        {error, blocked} ->
            {error, {<<"invalid_request">>, blocked}};
        {ok, _Friendship2, _Events} ->
            Setting = #{
                <<"msg">> => Greeting,
                <<"from">> => #{<<"source">> => <<"oa">>}
            },
            case
                enterprise_friend_request_repo:insert_pending_tx(
                    Conn, SenderUid, TargetUid, Setting, elib_dt:now()
                )
            of
                {ok, _} ->
                    {ok, #{
                        <<"request_status">> => <<"pending">>,
                        <<"sender_user_id">> => SenderExt,
                        <<"target_user_id">> => TargetExt
                    }};
                {error, Reason} ->
                    {error, {<<"internal_error">>, Reason}}
            end
    end.
