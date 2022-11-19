-module(webrtc_ws_logic).
%%%
% webrtc_ws 业务逻辑模块
% webrtc_ws business logic module
%%%

-export([
    event/3
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
-spec event(Uid::integer(), MsgId::binary(), Msg::binary()) -> ok.
event(Uid, MsgId, Msg) ->
    % MsLi = [0, 1000, 3000, 5000, 7000],
    MsLi = [0, 1500, 1500, 3000, 1000, 3000, 5000],
    message_ds:send_next(Uid, MsgId, Msg, MsLi),
    ok.

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
