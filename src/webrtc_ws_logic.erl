-module(webrtc_ws_logic).
%%%
% webrtc_ws 业务逻辑模块
% webrtc_ws business logic module
%%%

-export([
    event/4
]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("imboy/include/chat.hrl").
-include_lib("imboy/include/common.hrl").
-include_lib("kernel/include/logger.hrl").

%% ------------------------------------------------------------------
%% api
%% ------------------------------------------------------------------


% for webrtc
-spec event(integer(), integer(), binary(), binary()) -> ok.
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
            Msg = message_ds:assemble_s2c(MsgId, <<"in_denylist">>, ToUid),
            {reply, Msg};
        {false, _InDenylist} ->
            Msg = message_ds:assemble_s2c(MsgId, <<"not_a_friend">>, ToUid),
            {reply, Msg}
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% -------------------------------------------------------------------

%

%% ------------------------------------------------------------------
%% EUnit tests.
%% ------------------------------------------------------------------

-ifdef(EUNIT).
%addr_test_() ->
%    [?_assert(is_public_addr(?PUBLIC_IPV4ADDR)),
%     ?_assert(is_public_addr(?PUBLIC_IPV6ADDR)),
%     ?_test(my_if_addr(inet)),
%     ?_test(my_if_addr(inet6))].
-endif.
