-module(adm_index_handler).
%%%
% adm_index 控制器模块
% adm_index controller module
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
        index ->
            index(Req0, State);
        false ->
            Req0
    end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

index(Req0, State) ->
    % Host = "http://127.0.0.1:9806",
    % Location = "/adm/passport/login",
    % Req1 = cowboy_req:reply(302
    %     % , [{'Location', Host ++ Location}]
    %     , #{<<"Location">> => imboy_func:implode("", [Host,  Location])}
    %     % , <<"abc">>
    %     , Req0),
    % {halt, Req1, State}.

    AdmUser = maps:get(adm_user_id, State, []),
    AdmUserC = imboy_req:cookie(<<"adm_user_id">>, Req0),
    % Uid = imboy_hashids:uid_encode(CurrentUid),

    % PostVals = imboy_req:post_params(Req0),
    % Val1 = proplists:get_value(<<"val1">>, PostVals, ""),
    % Val2 = proplists:get_value(<<"val2">>, PostVals, ""),
    % adm_index_logic:demo(CurrentUid, Val1, Val2),
    imboy_response:success(Req0, [AdmUserC, AdmUser, State], "success.").

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
