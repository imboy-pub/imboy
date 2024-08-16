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
-include_lib("imlib/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imlib/include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================

init(Req0, State0) ->
    % ?LOG(State0),
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 = case Action of
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
    Column = <<"region_code,type, package_name, app_name, vsn, download_url, description, force_update">>,
    Where = case RegionCode of
        <<>> ->
            <<"status=1 AND type='", Cos/binary, "'">>;
        _ ->
             <<"status=1 AND region_code='", RegionCode/binary, "' AND type='", Cos/binary, "'">>
    end,

    Res = app_version_repo:find(Where, Column),
    % ?LOG([Res]),
    LastVsn = maps:get(<<"vsn">>, Res, <<"0.0.0">>),
    % ?LOG([LastVsn, Res, Where]),
    %  updatable = [true | false]
    imboy_response:success(Req0, Res#{
        <<"updatable">> => ec_semver:lt(Vsn, LastVsn)
    }).

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
