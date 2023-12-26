-module(adm_app_version_handler).
%%%
% adm_app_version 控制器模块
% adm_app_version controller module
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
    % ?LOG(State),
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 = case Action of
        demo_action ->
            demo_action(Method, Req0, State);
        false ->
            Req0
    end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

demo_action(<<"GET">>, Req0, _State) ->
    Body = <<>>,
    cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/html; charset=utf-8">>
        , <<"Access-Control-Allow-Origin">> => <<"*">>
    }, Body, Req0);
demo_action(<<"POST">>, Req0, State) ->
    CurrentUid = maps:get(current_uid, State),
    % Uid = imboy_hashids:uid_encode(CurrentUid),

    PostVals = imboy_req:post_params(Req0),
    Val1 = proplists:get_value(<<"val1">>, PostVals, ""),
    Val2 = proplists:get_value(<<"val2">>, PostVals, ""),
    adm_app_version_logic:demo(CurrentUid, Val1, Val2),
    imboy_response:success(Req0, PostVals, "success.").

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
