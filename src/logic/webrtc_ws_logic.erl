-module(webrtc_ws_logic).
%%%
% webrtc_ws 业务逻辑模块
% webrtc_ws business logic module
%%%

%% @doc WebRTC 事件处理
%% 处理 WebRTC 相关的 WebSocket 消息，检查好友关系和黑名单
%% @param CurrentUid 当前用户ID
%% @param ToUid 目标用户ID
%% @param MsgId 消息ID
%% @param Msg 消息内容（JSON 格式）
%% @returns ok | {reply, binary()}
-export([event/4]).

-include_lib("eunit/include/eunit.hrl").
-include("chat.hrl").
-include("common.hrl").
-include_lib("kernel/include/logger.hrl").

%% ===================================================================
%% API
%% ===================================================================

% for webrtc
-spec event(integer(), integer(), binary(), binary()) -> ok | {reply, binary()}.
event(CurrentUid, ToUid, MsgId, Msg) when
    is_integer(CurrentUid),
    CurrentUid > 0,
    is_integer(ToUid),
    ToUid > 0,
    is_binary(MsgId),
    is_binary(Msg)
->
    % 判断当前用户是否是 ToUid 用户的朋友
    IsFriend = friend_ds:is_friend(ToUid, CurrentUid),
    % 判断当前用户是否在 ToUid 的黑名单里面
    InDenylist = user_denylist_logic:in_denylist(ToUid, CurrentUid),
    case {IsFriend, InDenylist} of
        {true, 0} ->
            case is_webrtc_offer(Msg) andalso imboy_syn:list_by_uid(ToUid) =:= [] of
                true ->
                    %% 对端无任何在线设备：offer 若照常 send_next 会因无在线
                    %% 订阅者被静默丢弃，主叫只能空等 60s 超时并误显示
                    %% “对方无应答”。直接回 S2C peer_offline 让主叫快速失败。
                    MsgMap = message_ds:assemble_s2c(MsgId, <<"peer_offline">>, ToUid),
                    {reply, jsone:encode(MsgMap, [native_utf8])};
                false ->
                    do_send_next(ToUid, MsgId, Msg)
            end;
        {_, InDenylist2} when InDenylist2 > 0 ->
            MsgMap = message_ds:assemble_s2c(MsgId, <<"in_denylist">>, ToUid),
            {reply, jsone:encode(MsgMap, [native_utf8])};
        {false, _InDenylist} ->
            MsgMap = message_ds:assemble_s2c(MsgId, <<"not_a_friend">>, ToUid),
            {reply, jsone:encode(MsgMap, [native_utf8])}
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 好友/黑名单校验通过后的常规转发 + SERVER_ACK 回执
do_send_next(ToUid, MsgId, Msg) ->
    %% MsLi: 消息状态列表，[0] 表示未读
    MsLi = [0],
    message_ds:send_next(ToUid, MsgId, Msg, MsLi),
    %% 回执 WEBRTC_SERVER_ACK：发送方确知服务端已收，
    %% 否则客户端机制A（_pendingMessages）对每条 webrtc 信令必报确认超时。
    %% 必须走 JSON 预编码投递路径（websocket_info 的 binary 分支，
    %% v2 连接包 MSG_S2C 帧且 payload 恒为 JSON）；不能 {reply, Map}——
    %% 该路径对 protobuf 客户端走枚举编码，MsgDirection 无 WEBRTC_SERVER_ACK 会丢 type。
    Ack = #{
        <<"id">> => MsgId,
        <<"type">> => <<"WEBRTC_SERVER_ACK">>,
        <<"in_reply_to">> => MsgId,
        <<"server_ts">> => elib_dt:millisecond()
    },
    self() ! {reply, jsone:encode(Ack, [native_utf8])},
    ok.

%% @doc 判断信令是否为呼叫邀请（offer）。
%% webrtc_* 信令是整包透传 JSON，此处只做类型识别，不改写内容。
is_webrtc_offer(Msg) when is_binary(Msg) ->
    try jsone:decode(Msg) of
        #{<<"type">> := Type} when is_binary(Type) ->
            cowboy_bstr:to_lower(Type) =:= <<"webrtc_offer">>;
        _ ->
            false
    catch
        _:_ ->
            false
    end;
is_webrtc_offer(_) ->
    false.

%% ===================================================================
%% EUnit tests.
%% ===================================================================
