-module(app_version_handler).

%%%
% app_version 控制器模块
% app_version controller module
%%%
-behavior(cowboy_rest).

-export([init/2]).

-ifdef(EUNIT).

-include_lib("eunit/include/eunit.hrl").

-endif.

-include("log.hrl").

-include_lib("kernel/include/logger.hrl").

-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    % ?DEBUG_LOG(State0),
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 =
        case Action of
            check ->
                check(Method, Req0, State);
            false ->
                Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

check(<<"GET">>, Req0, _State) ->
    Cos = cowboy_req:header(<<"cos">>, Req0, <<"web">>),
    % imboy_log:info(Cos),
    #{vsn := Vsn} = cowboy_req:match_qs([{vsn, [], <<"">>}], Req0),
    #{region_code := RegionCode} = cowboy_req:match_qs([{region_code, [], <<>>}], Req0),

    Res = app_version_repo:find(Cos, RegionCode),
    % ?DEBUG_LOG([Res]),
    LastVsn = maps:get(<<"vsn">>, Res, <<"0.0.0">>),
    % ?DEBUG_LOG([LastVsn, Res, WhereMap]),
    %  updatable = [true | false]
    imboy_response:success(Req0, Res#{<<"updatable">> => ec_semver:lt(Vsn, LastVsn)}).

%% ===================================================================
%% EUnit tests.
%% ===================================================================

-ifdef(EUNIT).

%addr_test_() ->
%    [?_assert(is_public_addr(?PUBLIC_IPV4ADDR)),
%     ?_assert(is_public_addr(?PUBLIC_IPV6ADDR)),
%     ?_test(my_if_addr(inet)),
%     ?_test(my_if_addr(inet6))].
-endif.
