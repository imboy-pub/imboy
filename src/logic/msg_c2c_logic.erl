-module(msg_c2c_logic).
%%%
%  msg_c2c 业务逻辑模块
%%%

-export([c2c/3]).
-export([c2c_client_ack/3]).
-export([c2c_revoke/3]).
-export([c2c_revoke_ack/3]).
-export([c2c_edit/3]).
-export([c2c_edit_ack/3]).

-include("chat.hrl").
-include("log.hrl").

%% 抑制 Dialyzer 警告 - 内部辅助函数
-dialyzer({nowarn_function, [c2c/3, prepare_c2c_data/2, stage_and_send_c2c/10]}).

%% ===================================================================
%% API
%% ===================================================================

%% 单聊消息
-spec c2c(binary(), integer(), Data :: map()) -> ok | {reply, Msg :: map()}.
c2c(MsgId, CurrentUid, Data) ->
    To = maps:get(<<"to">>, Data),
    ToId = elib_hashids:decode(To),

    % 【优化】使用联合查询函数同时检查好友关系和黑名单状态
    {IsFriend, InDenylist} = friend_ds:check_relationship(ToId, CurrentUid),
    % elib_log:info([<<"msg_c2c_c2c">>, CurrentUid, ToId, IsFriend, InDenylist]),
    case {IsFriend, InDenylist} of
        {true, 0} ->
            {From, PayloadJson, MsgType, Action, E2EE, Timestamps} = prepare_c2c_data(CurrentUid, Data),
            stage_and_send_c2c(MsgId, To, ToId, From, PayloadJson, MsgType, Action, E2EE, Timestamps, CurrentUid);
        {_, InDenylist2} when InDenylist2 > 0 ->
            Msg = message_ds:assemble_s2c(MsgId, <<"in_denylist">>, To),
            {reply, Msg};
        {false, _InDenylist} ->
            Msg = message_ds:assemble_s2c(MsgId, <<"not_a_friend">>, To),
            % elib_log:info(Msg),
            {reply, Msg}
    end.

%% ===================================================================
%% Internal Functions
%% ===================================================================

%% @doc 准备单聊消息数据
%% @private
-spec prepare_c2c_data(integer(), map()) -> {binary(), binary(), binary(), binary(), map(), map()}.
prepare_c2c_data(CurrentUid, Data) ->
    NowTs = elib_dt:now(),
    NowMS = elib_dt:rfc3339_to(NowTs, millisecond),
    From = elib_hashids:encode(CurrentUid),
    Payload = maps:get(<<"payload">>, Data),
    CreatedAt = maps:get(<<"created_at">>, Data),
    CreatedAtRfc = elib_dt:to_rfc3339(CreatedAt),

    % v2.0: 从顶层提取字段
    MsgType = maps:get(<<"msg_type">>, Data, <<>>),
    Action = maps:get(<<"action">>, Data, <<>>),
    E2EE = maps:get(<<"e2ee">>, Data, null), % map() | null

    Timestamps = #{
        now_ts => NowTs,
        now_ms => NowMS,
        created_at_rfc => CreatedAtRfc
    },
    {From, Payload, MsgType, Action, E2EE, Timestamps}.

%% @doc 备份并发送单聊消息
%% @private
-spec stage_and_send_c2c(binary(), binary(), integer(), binary(), binary(), binary(), binary(), map(), map(), integer()) ->
          ok | {reply, map()}.
stage_and_send_c2c(MsgId, To, ToId, From, Payload, MsgType, Action, E2EE, Timestamps, CurrentUid) ->
    #{now_ts := NowTs, now_ms := NowMS, created_at_rfc := CreatedAtRfc} = Timestamps,

    % 【关键修复】先备份到 staging 表（同步，确保消息安全）
    StageResult = msg_store_ds:stage(
        <<"c2c">>, MsgId, MsgType, Action, E2EE, Payload,
        CurrentUid, ToId, CreatedAtRfc, NowTs),

    elib_log:info(["stage_and_send_c2c", StageResult]),
    case StageResult of
        ok ->
            % 立即响应和投递
            self() ! {reply, #{
                <<"id">> => MsgId,
                <<"type">> => <<"C2C_SERVER_ACK">>,
                <<"server_ts">> => NowMS
            }},

            % 异步处理：入队 + 投递消息（带重试）
            elib_async:async_retry(fun() ->
                % ① 先入队（异步，立即返回）
                msg_store_ds:enqueue(<<"c2c">>, MsgId, #{
                    payload => Payload,
                    from_id => CurrentUid,
                    to_id => ToId,
                    created_at => CreatedAtRfc,
                    server_ts => NowTs
                }),

                % ② 后投递（使用 MsgType/Action/E2EE 参数，不解析 Payload）
                Msg = message_ds:assemble_msg(<<"C2C">>, From, To, Payload, MsgId, MsgType, Action, E2EE),
                imboy_message_helper:encode_and_send(ToId, MsgId, Msg, <<"c2c">>)
            end, 3, 1000),
            ok;
        error ->
            % 备份失败，记录错误并返回失败
            ok = ?ERROR_LOG("[C2C_STAGE_FAILED] MsgId=~s, FromUid=~p, ToUid=~p~n",
                       [MsgId, CurrentUid, ToId]),
            {reply, message_ds:assemble_s2c(MsgId, <<"internal_error">>, To)}
    end.


%% 客户端确认C2C投递消息
-spec c2c_client_ack(binary(), integer(), binary()) -> ok.
c2c_client_ack(MsgId, CurrentUid, DID) ->
    msg_ack_logic:client_ack(<<"c2c">>, MsgId, CurrentUid, DID).


%% 客户端撤回消息 for c2c
-spec c2c_revoke(binary(), integer(), Data :: map()) -> ok | {reply, Msg :: map()}.
c2c_revoke(MsgId, CurrentUid, Data) ->
    To = maps:get(<<"to">>, Data),
    From = maps:get(<<"from">>, Data),
    Payload = maps:get(<<"payload">>, Data),
    OriginalMsgId = maps:get(<<"original_msg_id">>, Payload),
    ToId = elib_hashids:decode(To),
    FromId = elib_hashids:decode(From),
    % ?DEBUG_LOG([From, To, ToId, CurrentUid, Data]),

    %% 【权限验证】只能撤销自己发送的消息
    case CurrentUid =:= FromId of
        true ->
            %% 【新增】检查消息是否存在
            case msg_c2c_ds:find_msg_by_id(OriginalMsgId) of
                {ok, #{<<"from_id">> := FromId}} ->
                    NowMs = elib_dt:millisecond(),
                    NowTs = elib_dt:now(),

                    % 构建撤销确认消息（v2.0 格式）
                    %% msg_type 和 action 在顶层，不在 payload 中
                    RevokePayload = #{
                        <<"content">> => <<>>,
                        <<"original_msg_id">> => OriginalMsgId,
                        <<"revoked_at">> => NowMs
                    },

                    RevokeMsg = #{
                        <<"id">> => MsgId,
                        <<"type">> => <<"C2C">>,
                        <<"from">> => From,
                        <<"to">> => To,
                        <<"msg_type">> => <<"custom">>,
                        <<"action">> => <<"message_revoke_ack">>,
                        <<"payload">> => RevokePayload,
                        <<"server_ts">> => NowMs
                    },

                    % 判断对方是否在线
                    case user_logic:is_online(ToId) of
                        true ->
                            imboy_message_helper:encode_and_send(ToId, MsgId, RevokeMsg, <<"c2s">>),
                            ok;
                        false ->  % 对端离线处理

                            % v2.0: 使用 revoke_offline_msg/8 显式传递 msg_type 和 action
                            case msg_c2c_ds:revoke_offline_msg(RevokePayload, NowTs, MsgId, FromId, ToId, <<"custom">>, <<"message_revoke_ack">>, null) of
                                ok -> ok;
                                {error, _} -> ok
                            end
                    end,
                    {reply, RevokeMsg};
                {ok, _} ->
                    %% 消息不属于当前用户
                    ErrorMsg = message_ds:assemble_s2c(MsgId, <<"permission_denied">>, To),
                    {reply, ErrorMsg};
                {error, not_found} ->
                    %% 消息不存在
                    ErrorMsg = message_ds:assemble_s2c(MsgId, <<"msg_not_found">>, To),
                    {reply, ErrorMsg}
            end;
        false ->
            % 权限不足，返回错误
            ErrorMsg = message_ds:assemble_s2c(MsgId, <<"permission_denied">>, To),
            {reply, ErrorMsg}
    end.

%% 客户端撤回消息确认 for c2c
-spec c2c_revoke_ack(binary(), integer(), Data :: map()) -> ok.
c2c_revoke_ack(MsgId, CurrentUid, Data) ->
    Payload = maps:get(<<"payload">>, Data),
    OriginalMsgId = maps:get(<<"original_msg_id">>, Payload),
    ok = ?DEBUG_LOG([MsgId, CurrentUid, OriginalMsgId]),
    % 撤回确认已处理
    % 撤回消息的存储和通知在 c2c_revoke 中已完成
    % 此处仅为接收方确认收到撤回通知
    % 如需追踪撤回状态，可在消息中添加 is_revoked 字段
    ok.

%% 客户端编辑消息 for c2c
-spec c2c_edit(binary(), integer(), Data :: map()) -> ok | {reply, Msg :: map()}.
c2c_edit(MsgId, CurrentUid, Data) ->
    To = maps:get(<<"to">>, Data),
    From = maps:get(<<"from">>, Data),
    Payload = maps:get(<<"payload">>, Data),
    OriginalMsgId = maps:get(<<"original_msg_id">>, Payload),
    NewContent = maps:get(<<"content">>, Payload),
    MsgType = maps:get(<<"msg_type">>, Payload),
    ToId = elib_hashids:decode(To),
    FromId = elib_hashids:decode(From),
    ok = ?DEBUG_LOG([From, To, ToId, CurrentUid, Data]),
    
    % 验证权限：只能编辑自己发送的消息
    case CurrentUid =:= FromId of
        true ->
            NowTs = elib_dt:now(),
            NowMS = elib_dt:millisecond(),

            % 构建编辑确认消息（v2.0 格式）
            %% msg_type 和 action 提升到顶层
            EditPayload = #{
                <<"content">> => NewContent,
                <<"original_msg_id">> => OriginalMsgId,
                <<"edited_at">> => NowMS
            },

            EditMsg = #{
                <<"id">> => MsgId,
                <<"type">> => <<"C2C">>,
                <<"from">> => From,
                <<"to">> => To,
                <<"msg_type">> => MsgType,
                <<"action">> => <<"message_edit_ack">>,
                <<"payload">> => EditPayload,
                <<"server_ts">> => NowMS
            },
            
            % 判断对方是否在线
            case user_logic:is_online(ToId) of
                true ->
                    imboy_message_helper:encode_and_send(ToId, MsgId, EditMsg, <<"c2s">>),
                    ok;
                false ->  % 对端离线处理
                    EditPayloadJson = imboy_message_helper:encode_json(EditPayload),
                    case msg_c2c_ds:edit_offline_msg(EditPayloadJson, NowTs, MsgId, FromId, ToId) of
                        ok ->
                            ok;
                        {error, _Reason} ->
                            ok
                    end
            end,
            {reply, EditMsg};
        false ->
            % 权限不足，返回错误
            ErrorMsg = message_ds:assemble_s2c(MsgId, <<"permission_denied">>, To),
            {reply, ErrorMsg}
    end.

%% 客户端编辑消息确认 for c2c
-spec c2c_edit_ack(binary(), integer(), Data :: map()) -> ok.
c2c_edit_ack(MsgId, CurrentUid, Data) ->
    Payload = maps:get(<<"payload">>, Data),
    OriginalMsgId = maps:get(<<"original_msg_id">>, Payload),
    NewContent = maps:get(<<"content">>, Payload),
    EditedAt = maps:get(<<"edited_at">>, Payload),
    ok = ?DEBUG_LOG([MsgId, CurrentUid, OriginalMsgId, NewContent, EditedAt]),
    
    % 更新本地消息内容
    % 这里可以添加数据库更新逻辑
    ok.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
