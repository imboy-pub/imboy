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
event(CurrentUid, ToUid, MsgId, Msg) ->
    % 判断当前用户是否是 ToUid 用户的朋友
    IsFriend = friend_ds:is_friend(ToUid, CurrentUid),
    % 判断当前用户是否在 ToUid 的黑名单里面
    InDenylist = user_denylist_logic:in_denylist(ToUid, CurrentUid),
    case {IsFriend, InDenylist} of
        {true, 0} ->
            MsLi = [0],
            message_ds:send_next(ToUid, MsgId, Msg, MsLi),
            ok;
        {_, InDenylist2} when InDenylist2 > 0 ->
            MsgMap = message_ds:assemble_s2c(MsgId, <<"in_denylist">>, elib_hashids:encode(ToUid)),
            {reply, jsone:encode(MsgMap, [native_utf8])};
        {false, _InDenylist} ->
            MsgMap = message_ds:assemble_s2c(MsgId, <<"not_a_friend">>, elib_hashids:encode(ToUid)),
            {reply, jsone:encode(MsgMap, [native_utf8])}
    end.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%

%% ===================================================================
%% EUnit tests.
%% ===================================================================

