-module(msg_c2g_logic).
%%%
% msg_c2g 业务逻辑模块
%%%
-export([c2g/3]).
-export([c2g_client_ack/3]).
-export([c2g_revoke/3]).
-export([c2g_revoke_ack/3]).
-export([c2g_edit/3]).
-export([c2g_edit_ack/3]).

-export([check_msg/3]).

-include_lib("imlib/include/chat.hrl").
-include_lib("imlib/include/log.hrl").


%% ===================================================================
%% API
%% ===================================================================

%% 群聊发送消息
-spec c2g(binary(), integer(), list()) -> ok | {reply, list()}.
c2g(MsgId, CurrentUid, Data) ->
    Gid = proplists:get_value(<<"to">>, Data),
    ToGID = imboy_hashids:decode(Gid),
    % TODO check is group member
    MemberUids = group_ds:member_uids(ToGID),
    % Uids.
    NowTs = imboy_dt:now(),
    NowMS = imboy_dt:rfc3339_to(NowTs, millisecond),
    CreatedAt = proplists:get_value(<<"created_at">>, Data),
    CreatedAtMs = case imboy_type:is_numeric(CreatedAt) of
        true ->
            CreatedAt;
        false when is_binary(CreatedAt) orelse is_list(CreatedAt) ->
            imboy_dt:rfc3339_to(CreatedAt, millisecond);
        false ->
            % 如果时间戳格式错误，让进程崩溃
            erlang:error({invalid_timestamp_format, CreatedAt})
    end,
    Msg = [{<<"id">>, MsgId},
           {<<"type">>, <<"C2G">>},
           {<<"from">>, imboy_hashids:encode(CurrentUid)},
           {<<"to">>, Gid},
           {<<"payload">>, proplists:get_value(<<"payload">>, Data)},
           {<<"created_at">>, CreatedAtMs},
           {<<"server_ts">>, NowMS}],
    % ?DEBUG_LOG(Msg),
    Msg2 = jsone:encode(Msg, [native_utf8]),
    MsLi = [0, 3500, 3500, 3000, 5000],
    [message_ds:send_next(Uid, MsgId, Msg2, MsLi) || Uid <- MemberUids, CurrentUid /= Uid],

    % 存储消息
    msg_c2g_ds:write_msg(NowTs, MsgId, Msg2, CurrentUid, MemberUids, ToGID),

    self() ! {reply, [{<<"id">>, MsgId}, {<<"type">>, <<"C2G_SERVER_ACK">>}, {<<"server_ts">>, NowMS}]},
    ok.

%% 客户端确认C2G投递消息
-spec c2g_client_ack(binary(), integer(), binary()) -> ok.
c2g_client_ack(MsgId, Uid, _DID) ->
    msg_c2g_timeline_repo:client_ack(Uid, MsgId),
    ok.

%% 客户端撤回消息 for c2g
-spec c2g_revoke(binary(), integer(), Data :: list()) -> ok | {reply, Msg :: list()}.
c2g_revoke(MsgId, CurrentUid, Data) ->
    To = proplists:get_value(<<"to">>, Data),
    From = proplists:get_value(<<"from">>, Data),
    Payload = proplists:get_value(<<"payload">>, Data),
    OriginalMsgId = proplists:get_value(<<"original_msg_id">>, Payload),
    ToGID = imboy_hashids:decode(To),
    FromId = imboy_hashids:decode(From),
    ?DEBUG_LOG([From, To, ToGID, CurrentUid, Data]),
    
    % 验证权限：只能撤销自己发送的消息，且必须是群成员
    case {CurrentUid =:= FromId, group_ds:is_member(ToGID, CurrentUid)} of
        {true, true} ->
            NowTs = imboy_dt:now(),
            NowMS = imboy_dt:millisecond(),
            
            % 获取群成员列表
            MemberUids = group_ds:member_uids(ToGID),
            
            % 构建撤销确认消息
            RevokePayload = [
                {<<"msg_type">>, <<"custom">>},
                {<<"action">>, <<"message_revoke_ack">>},
                {<<"content">>, <<>>},
                {<<"original_msg_id">>, OriginalMsgId},
                {<<"revoked_at">>, NowMS}
            ],
            
            RevokeMsg = [
                {<<"id">>, MsgId},
                {<<"type">>, <<"C2G">>},
                {<<"from">>, From},
                {<<"to">>, To},
                {<<"payload">>, RevokePayload},
                {<<"server_ts">>, NowMS}
            ],
            
            RevokeMsgJson = jsone:encode(RevokeMsg, [native_utf8]),
            MsLi = [0, 3500, 3500, 3000, 5000],
            % 发送给群组其他成员
            [message_ds:send_next(Uid, MsgId, RevokeMsgJson, MsLi) || Uid <- MemberUids, CurrentUid /= Uid],
            
            % 存储离线消息
            msg_c2g_ds:revoke_offline_msg(RevokeMsgJson, NowTs, MsgId, CurrentUid, MemberUids, ToGID),
            
            {reply, RevokeMsg};
        {false, _} ->
            % 权限不足，不是发送者
            ErrorMsg = message_ds:assemble_s2c(MsgId, <<"permission_denied">>, To),
            {reply, ErrorMsg};
        {_, false} ->
            % 不是群成员
            ErrorMsg = message_ds:assemble_s2c(MsgId, <<"not_group_member">>, To),
            {reply, ErrorMsg}
    end.

%% 客户端撤回消息确认 for c2g
-spec c2g_revoke_ack(binary(), integer(), Data :: list()) -> ok.
c2g_revoke_ack(MsgId, CurrentUid, Data) ->
    Payload = proplists:get_value(<<"payload">>, Data),
    OriginalMsgId = proplists:get_value(<<"original_msg_id">>, Payload),
    ?DEBUG_LOG([MsgId, CurrentUid, OriginalMsgId]),
    
    % 更新本地消息状态为已撤销
    % 这里可以添加数据库更新逻辑
    ok.

%% 客户端编辑消息 for c2g
-spec c2g_edit(binary(), integer(), Data :: list()) -> ok | {reply, Msg :: list()}.
c2g_edit(MsgId, CurrentUid, Data) ->
    To = proplists:get_value(<<"to">>, Data),
    From = proplists:get_value(<<"from">>, Data),
    Payload = proplists:get_value(<<"payload">>, Data),
    OriginalMsgId = proplists:get_value(<<"original_msg_id">>, Payload),
    NewContent = proplists:get_value(<<"content">>, Payload),
    MsgType = proplists:get_value(<<"msg_type">>, Payload),
    ToGID = imboy_hashids:decode(To),
    FromId = imboy_hashids:decode(From),
    ?DEBUG_LOG([From, To, ToGID, CurrentUid, Data]),
    
    % 验证权限：只能编辑自己发送的消息，且必须是群成员
    case {CurrentUid =:= FromId, group_ds:is_member(ToGID, CurrentUid)} of
        {true, true} ->
            NowTs = imboy_dt:now(),
            NowMS = imboy_dt:millisecond(),
            
            % 获取群成员列表
            MemberUids = group_ds:member_uids(ToGID),
            
            % 构建编辑确认消息
            EditPayload = [
                {<<"msg_type">>, MsgType},
                {<<"action">>, <<"message_edit_ack">>},
                {<<"content">>, NewContent},
                {<<"original_msg_id">>, OriginalMsgId},
                {<<"edited_at">>, NowMS}
            ],
            
            EditMsg = [
                {<<"id">>, MsgId},
                {<<"type">>, <<"C2G">>},
                {<<"from">>, From},
                {<<"to">>, To},
                {<<"payload">>, EditPayload},
                {<<"server_ts">>, NowMS}
            ],
            
            EditMsgJson = jsone:encode(EditMsg, [native_utf8]),
            MsLi = [0, 3500, 3500, 3000, 5000],
            % 发送给群组其他成员
            [message_ds:send_next(Uid, MsgId, EditMsgJson, MsLi) || Uid <- MemberUids, CurrentUid /= Uid],
            
            % 存储离线消息
            msg_c2g_ds:edit_offline_msg(EditMsgJson, NowTs, MsgId, CurrentUid, MemberUids, ToGID),
            
            {reply, EditMsg};
        {false, _} ->
            % 权限不足，不是发送者
            ErrorMsg = message_ds:assemble_s2c(MsgId, <<"permission_denied">>, To),
            {reply, ErrorMsg};
        {_, false} ->
            % 不是群成员
            ErrorMsg = message_ds:assemble_s2c(MsgId, <<"not_group_member">>, To),
            {reply, ErrorMsg}
    end.

%% 客户端编辑消息确认 for c2g
-spec c2g_edit_ack(binary(), integer(), Data :: list()) -> ok.
c2g_edit_ack(MsgId, CurrentUid, Data) ->
    Payload = proplists:get_value(<<"payload">>, Data),
    OriginalMsgId = proplists:get_value(<<"original_msg_id">>, Payload),
    NewContent = proplists:get_value(<<"content">>, Payload),
    EditedAt = proplists:get_value(<<"edited_at">>, Payload),
    ?DEBUG_LOG([MsgId, CurrentUid, OriginalMsgId, NewContent, EditedAt]),
    
    % 更新本地消息内容
    % 这里可以添加数据库更新逻辑
    ok.


check_msg(Uid, Pid, _DID) ->
    GMsgs = msg_c2g_ds:read_msg(Uid),
    sent_offline_msg(Uid, Pid, GMsgs, 0),
    ok.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================


sent_offline_msg(_Uid, _Pid, [], _Index) ->
    ok;
sent_offline_msg(Uid, Pid, [Row | Tail], Index) ->
    {<<"payload">>, Msg} = lists:keyfind(<<"payload">>, 1, Row),
    ?DEBUG_LOG([Uid, Pid, Index, Msg]),
    Delay = 100 + Index * 100,
    erlang:start_timer(Delay, Pid, Msg),
    sent_offline_msg(Uid, Pid, Tail, Index + 1).
