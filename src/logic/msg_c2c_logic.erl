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

%% ===================================================================
%% API
%% ===================================================================


%% 单聊消息
-spec c2c(binary(), integer(), Data :: map()) -> ok | {reply, Msg :: map()}.
c2c(MsgId, CurrentUid, Data) ->
    StartTime = erlang:monotonic_time(millisecond),
    io:format("⏱️ [C2C_START] MsgId: ~s, Time: ~p~n", [MsgId, StartTime]),

    To = maps:get(<<"to">>, Data),
    ToId = imboy_hashids:decode(To),
    % CurrentUid = imboy_hashids:decode(From),
    % ?DEBUG_LOG([CurrentUid, ToId, Data]),

    io:format("⏱️ [C2C_1] Decode complete: +~pms~n", [erlang:monotonic_time(millisecond) - StartTime]),

    % 判断当前用户是否是 ToId 用户的朋友
    IsFriend = friend_ds:is_friend(ToId, CurrentUid),
    io:format("⏱️ [C2C_2] Friend check complete: +~pms, IsFriend: ~p~n", [erlang:monotonic_time(millisecond) - StartTime, IsFriend]),

    % 判断当前用户是否在 ToId 的黑名单里面
    InDenylist = user_denylist_logic:in_denylist(ToId, CurrentUid),
    io:format("⏱️ [C2C_3] Denylist check complete: +~pms, InDenylist: ~p~n", [erlang:monotonic_time(millisecond) - StartTime, InDenylist]),

    case {IsFriend, InDenylist} of
        {true, 0} ->
            NowTs = imboy_dt:now(),
            NowMS = imboy_dt:rfc3339_to(NowTs, millisecond),
            From = imboy_hashids:encode(CurrentUid),
            Payload = maps:get(<<"payload">>, Data),
            CreatedAt = maps:get(<<"created_at">>, Data),
            CreatedAtRfc = imboy_dt:to_rfc3339(CreatedAt),
            io:format("⏱️ [C2C_4] Timestamps ready: +~pms~n", [erlang:monotonic_time(millisecond) - StartTime]),

            % 准备数据
            PayloadJson = jsone:encode(Payload, [native_utf8]),

            % 【关键修复】先备份到 staging 表（同步，确保消息安全）
            io:format("⏱️ [C2C_5] Staging message: +~pms~n", [erlang:monotonic_time(millisecond) - StartTime]),
            StageResult = msg_store_ds:stage(
                <<"c2c">>, MsgId, PayloadJson, CurrentUid, ToId,
                CreatedAtRfc, NowTs),

            case StageResult of
                ok ->
                    % 备份成功，继续处理
                    io:format("⏱️ [C2C_6] Stage success: +~pms~n", [erlang:monotonic_time(millisecond) - StartTime]),

                    % 立即响应和投递
                    self() ! {reply, #{
                        <<"id">> => MsgId,
                        <<"type">> => <<"C2C_SERVER_ACK">>,
                        <<"server_ts">> => NowMS
                    }},
                    io:format("⏱️ [C2C_7] Reply sent: +~pms~n", [erlang:monotonic_time(millisecond) - StartTime]),

                    % 异步处理：入队 + 投递消息（带重试）
                    imboy_async:async_retry(fun() ->
                        % ① 先入队（异步，立即返回）
                        msg_store_ds:enqueue(c2c, MsgId, #{
                            payload => PayloadJson,
                            from_id => CurrentUid,
                            to_id => ToId,
                            created_at => CreatedAtRfc,
                            server_ts => NowTs
                        }),

                        % ② 后投递（快速返回，不阻塞）
                        Msg = #{
                            <<"id">> => MsgId,
                            <<"type">> => <<"C2C">>,
                            <<"from">> => From,
                            <<"to">> => To,
                            <<"payload">> => Payload,
                            <<"created_at">> => CreatedAtRfc,
                            <<"server_ts">> => NowMS
                        },
                        MsgJson = jsone:encode(Msg, [native_utf8]),
                        MsLi = imboy_retry_config:intervals(<<"c2c">>),
                        io:format("⏱️ [C2C_9] send_next called: +~pms~n", [erlang:monotonic_time(millisecond) - StartTime]),
                        message_ds:send_next(ToId, MsgId, MsgJson, MsLi),

                        io:format("⏱️ [C2C_END] Total time: +~pms~n", [erlang:monotonic_time(millisecond) - StartTime])
                    end, 3, 1000),
                    ok;
                error ->
                    % 备份失败，记录错误并返回失败
                    ok = ?ERROR_LOG("[C2C_STAGE_FAILED] MsgId=~s, FromUid=~p, ToUid=~p~n",
                               [MsgId, CurrentUid, ToId]),
                    {reply, message_ds:assemble_s2c(MsgId, <<"internal_error">>, To)}
            end;
        {_, InDenylist2} when InDenylist2 > 0 ->
            io:format("⏱️ [C2C_DENY] In denylist: +~pms~n", [erlang:monotonic_time(millisecond) - StartTime]),
            Msg = message_ds:assemble_s2c(MsgId, <<"in_denylist">>, To),
            {reply, Msg};
        {false, _InDenylist} ->
            io:format("⏱️ [C2C_NOT_FRIEND] Not friend: +~pms~n", [erlang:monotonic_time(millisecond) - StartTime]),
            Msg = message_ds:assemble_s2c(MsgId, <<"not_a_friend">>, To),
            {reply, Msg}
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
    ToId = imboy_hashids:decode(To),
    FromId = imboy_hashids:decode(From),
    % ?DEBUG_LOG([From, To, ToId, CurrentUid, Data]),
    
    % 验证权限：只能撤销自己发送的消息
    case CurrentUid =:= FromId of
        true ->
            NowTs = imboy_dt:now(),
            NowMS = imboy_dt:millisecond(),

            % 构建撤销确认消息
            RevokePayload = #{
                <<"msg_type">> => <<"custom">>,
                <<"action">> => <<"message_revoke_ack">>,
                <<"content">> => <<>>,
                <<"original_msg_id">> => OriginalMsgId,
                <<"revoked_at">> => NowMS
            },

            RevokeMsg = #{
                <<"id">> => MsgId,
                <<"type">> => <<"C2C">>,
                <<"from">> => From,
                <<"to">> => To,
                <<"payload">> => RevokePayload,
                <<"server_ts">> => NowMS
            },
            
            % 判断对方是否在线
            case user_logic:is_online(ToId) of
                true ->
                    RevokeMsgJson = jsone:encode(RevokeMsg, [native_utf8]),
                    MsLi = imboy_retry_config:intervals(<<"c2s">>),
                    message_ds:send_next(ToId, MsgId, RevokeMsgJson, MsLi),
                    ok;
                false ->  % 对端离线处理
                    RevokePayloadJson = jsone:encode(RevokePayload, [native_utf8]),
                    case msg_c2c_ds:revoke_offline_msg(RevokePayloadJson, NowTs, MsgId, FromId, ToId) of
                        ok -> ok;
                        {error, _} -> ok
                    end
            end,
            {reply, RevokeMsg};
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
    % TODO
    % 更新本地消息状态为已撤销
    % 这里可以添加数据库更新逻辑
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
    ToId = imboy_hashids:decode(To),
    FromId = imboy_hashids:decode(From),
    ok = ?DEBUG_LOG([From, To, ToId, CurrentUid, Data]),
    
    % 验证权限：只能编辑自己发送的消息
    case CurrentUid =:= FromId of
        true ->
            NowTs = imboy_dt:now(),
            NowMS = imboy_dt:millisecond(),

            % 构建编辑确认消息
            EditPayload = #{
                <<"msg_type">> => MsgType,
                <<"action">> => <<"message_edit_ack">>,
                <<"content">> => NewContent,
                <<"original_msg_id">> => OriginalMsgId,
                <<"edited_at">> => NowMS
            },

            EditMsg = #{
                <<"id">> => MsgId,
                <<"type">> => <<"C2C">>,
                <<"from">> => From,
                <<"to">> => To,
                <<"payload">> => EditPayload,
                <<"server_ts">> => NowMS
            },
            
            % 判断对方是否在线
            case user_logic:is_online(ToId) of
                true ->
                    EditMsgJson = jsone:encode(EditMsg, [native_utf8]),
                    MsLi = imboy_retry_config:intervals(<<"c2s">>),
                    message_ds:send_next(ToId, MsgId, EditMsgJson, MsLi),
                    ok;
                false ->  % 对端离线处理
                    EditPayloadJson = jsone:encode(EditPayload, [native_utf8]),
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
