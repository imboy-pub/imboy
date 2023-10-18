-module(test_handler).
%%%
% test 控制器模块
% test controller module
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
    Req1 = case Action of
        req_get ->
            req_get(Req0, State);
        req_post ->
            req_post(Req0, State);
        false ->
            Req0
    end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

% imboy_req:get("http://127.0.0.1:9800/test/req_get?type=list&a=1").

req_get(Req0, _State) ->
    % CurrentUid = maps:get(current_uid, State),
    % Uid = imboy_hashids:uid_encode(CurrentUid),
    #{type := Type} = cowboy_req:match_qs([{type, [], undefined}], Req0),
    #{a := A} = cowboy_req:match_qs([{a, [], 0}], Req0),

    % test_logic:demo(CurrentUid, Val1, Val2),
    imboy_response:success(Req0, [
        {<<"a">>, A}
        , {<<"config">>, imboy_func:implode("", [config_ds:env(test)])}
        , {<<"type">>, Type}
        , {<<"host">>, cowboy_req:header(<<"host">>, Req0)}
        , {<<"client">>, cowboy_req:header(<<"client">>, Req0)}
        , {<<"content-type">>, cowboy_req:header(<<"content-type">>, Req0)}
    ], "success.").


% imboy_req:post("http://127.0.0.1:9800/test/req_post", #{type => 1, b => 2}).
% imboy_req:post("http://127.0.0.1:9800/test/req_post", [1,2,3]).
req_post(Req0, _State) ->
    % CurrentUid = maps:get(current_uid, State),
    % Uid = imboy_hashids:uid_encode(CurrentUid),

    PostVals = imboy_req:post_params(Req0),
    % Val1 = proplists:get_value(<<"val1">>, PostVals, ""),
    % Val2 = proplists:get_value(<<"val2">>, PostVals, ""),
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
