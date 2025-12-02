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

-include_lib("imlib/include/chat.hrl").
-include_lib("imlib/include/log.hrl").

%% ===================================================================
%% API
%% ===================================================================


%% 单聊消息
-spec c2c(binary(), integer(), Data :: list()) -> ok | {reply, Msg :: list()}.
c2c(MsgId, CurrentUid, Data) ->
    To = proplists:get_value(<<"to">>, Data),
    ToId = imboy_hashids:decode(To),
    % CurrentUid = imboy_hashids:decode(From),
    ?DEBUG_LOG([CurrentUid, ToId, Data]),
    % 判断当前用户是否是 ToId 用户的朋友
    IsFriend = friend_ds:is_friend(ToId, CurrentUid),
    % 判断当前用户是否在 ToId 的黑名单里面
    InDenylist = user_denylist_logic:in_denylist(ToId, CurrentUid),
    case {IsFriend, InDenylist} of
        {true, 0} ->
            NowTs = imboy_dt:now(),
            NowMS = imboy_dt:rfc3339_to(NowTs, millisecond),
            From = imboy_hashids:encode(CurrentUid),
            Payload = proplists:get_value(<<"payload">>, Data),
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
            % 存储消息
            msg_c2c_ds:write_msg(CreatedAt, MsgId, Payload, CurrentUid, ToId, NowTs),
            %
            self() ! {reply, [{<<"id">>, MsgId}, {<<"type">>, <<"C2C_SERVER_ACK">>}, {<<"server_ts">>, NowMS}]},

            Msg = [{<<"id">>, MsgId},
                   {<<"type">>, <<"C2C">>},
                   {<<"from">>, From},
                   {<<"to">>, To},
                   {<<"payload">>, Payload},
                   {<<"created_at">>, CreatedAtMs},
                   {<<"server_ts">>, NowMS}],
            MsgJson = jsone:encode(Msg, [native_utf8]),
            MsLi = [0, 5000, 7000, 11000],
            message_ds:send_next(ToId, MsgId, MsgJson, MsLi),
            ok;
        {_, InDenylist2} when InDenylist2 > 0 ->
            Msg = message_ds:assemble_s2c(MsgId, <<"in_denylist">>, To),
            {reply, Msg};
        {false, _InDenylist} ->
            Msg = message_ds:assemble_s2c(MsgId, <<"not_a_friend">>, To),
            {reply, Msg}
    end.


%% 客户端确认C2C投递消息
-spec c2c_client_ack(binary(), integer(), binary()) -> ok.
c2c_client_ack(MsgId, CurrentUid, _DID) ->
    Column = <<"id">>,
    Where = <<"msg_id = '", (ec_cnv:to_binary(MsgId))/binary,"' AND to_id = ", (ec_cnv:to_binary(CurrentUid))/binary>>,
    {ok, _CList, Rows} = msg_c2c_repo:read_msg(Where, Column, 1),
    [msg_c2c_repo:delete_msg(Id) || {Id} <- Rows],
    ok.


%% 客户端撤回消息 for c2c
-spec c2c_revoke(binary(), integer(), Data :: list()) -> ok | {reply, Msg :: list()}.
c2c_revoke(MsgId, CurrentUid, Data) ->
    To = proplists:get_value(<<"to">>, Data),
    From = proplists:get_value(<<"from">>, Data),
    Payload = proplists:get_value(<<"payload">>, Data),
    OriginalMsgId = proplists:get_value(<<"original_msg_id">>, Payload),
    ToId = imboy_hashids:decode(To),
    FromId = imboy_hashids:decode(From),
    ?DEBUG_LOG([From, To, ToId, CurrentUid, Data]),
    
    % 验证权限：只能撤销自己发送的消息
    case CurrentUid =:= FromId of
        true ->
            NowTs = imboy_dt:now(),
            NowMS = imboy_dt:millisecond(),
            
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
                {<<"type">>, <<"C2C">>},
                {<<"from">>, From},
                {<<"to">>, To},
                {<<"payload">>, RevokePayload},
                {<<"server_ts">>, NowMS}
            ],
            
            % 判断对方是否在线
            case user_logic:is_online(ToId) of
                true ->
                    RevokeMsgJson = jsone:encode(RevokeMsg, [native_utf8]),
                    MsLi = [0, 5000, 7000, 11000],
                    message_ds:send_next(ToId, MsgId, RevokeMsgJson, MsLi),
                    ok;
                false ->  % 对端离线处理
                    RevokePayloadJson = jsone:encode(RevokePayload, [native_utf8]),
                    msg_c2c_ds:revoke_offline_msg(RevokePayloadJson, NowTs, MsgId, FromId, ToId)
            end,
            {reply, RevokeMsg};
        false ->
            % 权限不足，返回错误
            ErrorMsg = message_ds:assemble_s2c(MsgId, <<"permission_denied">>, To),
            {reply, ErrorMsg}
    end.

%% 客户端撤回消息确认 for c2c
-spec c2c_revoke_ack(binary(), integer(), Data :: list()) -> ok.
c2c_revoke_ack(MsgId, CurrentUid, Data) ->
    Payload = proplists:get_value(<<"payload">>, Data),
    OriginalMsgId = proplists:get_value(<<"original_msg_id">>, Payload),
    ?DEBUG_LOG([MsgId, CurrentUid, OriginalMsgId]),
    
    % 更新本地消息状态为已撤销
    % 这里可以添加数据库更新逻辑
    ok.

%% 客户端编辑消息 for c2c
-spec c2c_edit(binary(), integer(), Data :: list()) -> ok | {reply, Msg :: list()}.
c2c_edit(MsgId, CurrentUid, Data) ->
    To = proplists:get_value(<<"to">>, Data),
    From = proplists:get_value(<<"from">>, Data),
    Payload = proplists:get_value(<<"payload">>, Data),
    OriginalMsgId = proplists:get_value(<<"original_msg_id">>, Payload),
    NewContent = proplists:get_value(<<"content">>, Payload),
    MsgType = proplists:get_value(<<"msg_type">>, Payload),
    ToId = imboy_hashids:decode(To),
    FromId = imboy_hashids:decode(From),
    ?DEBUG_LOG([From, To, ToId, CurrentUid, Data]),
    
    % 验证权限：只能编辑自己发送的消息
    case CurrentUid =:= FromId of
        true ->
            NowTs = imboy_dt:now(),
            NowMS = imboy_dt:millisecond(),
            
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
                {<<"type">>, <<"C2C">>},
                {<<"from">>, From},
                {<<"to">>, To},
                {<<"payload">>, EditPayload},
                {<<"server_ts">>, NowMS}
            ],
            
            % 判断对方是否在线
            case user_logic:is_online(ToId) of
                true ->
                    EditMsgJson = jsone:encode(EditMsg, [native_utf8]),
                    MsLi = [0, 5000, 7000, 11000],
                    message_ds:send_next(ToId, MsgId, EditMsgJson, MsLi),
                    ok;
                false ->  % 对端离线处理
                    EditPayloadJson = jsone:encode(EditPayload, [native_utf8]),
                    msg_c2c_ds:edit_offline_msg(EditPayloadJson, NowTs, MsgId, FromId, ToId)
            end,
            {reply, EditMsg};
        false ->
            % 权限不足，返回错误
            ErrorMsg = message_ds:assemble_s2c(MsgId, <<"permission_denied">>, To),
            {reply, ErrorMsg}
    end.

%% 客户端编辑消息确认 for c2c
-spec c2c_edit_ack(binary(), integer(), Data :: list()) -> ok.
c2c_edit_ack(MsgId, CurrentUid, Data) ->
    Payload = proplists:get_value(<<"payload">>, Data),
    OriginalMsgId = proplists:get_value(<<"original_msg_id">>, Payload),
    NewContent = proplists:get_value(<<"content">>, Payload),
    EditedAt = proplists:get_value(<<"edited_at">>, Payload),
    ?DEBUG_LOG([MsgId, CurrentUid, OriginalMsgId, NewContent, EditedAt]),
    
    % 更新本地消息内容
    % 这里可以添加数据库更新逻辑
    ok.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
