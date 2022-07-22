-module(webrtc_ws_logic).
%%%
% webrtc_ws 业务逻辑模块
% webrtc_ws business logic module
%%%

-export([
    callback/1,
    authenticate/2,
    create/3,
    join/3,
    leave/3
]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/logger.hrl").
-include_lib("imboy/include/common.hrl").

%% ------------------------------------------------------------------
%% api
%% ------------------------------------------------------------------

callback(create_callback) ->
  {webrtc_ws_logic, create};
callback(join_callback) ->
  {webrtc_ws_logic, join};
callback(leave_callback) ->
  {webrtc_ws_logic, leave}.

authenticate(_Username, Password) ->
  %% in a real scenario this may lookup the password in the db, request an external service, etc.
  % {ok, Password} = application:get_env(example, example_password),
  % Password.
  Password.

create(Room, Username, _OtherUsers) ->
  lager:info("~s created ~s", [Username, Room]).

join(Room, Username, _OtherUsers) ->
  lager:info("~s joined ~s", [Username, Room]).

leave(Room, Username, _OtherUsers) ->
  lager:info("~s left ~s", [Username, Room]).


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
