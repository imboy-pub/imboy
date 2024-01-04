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
    Method = cowboy_req:method(Req0),
    Req1 = case Action of
        index ->
            index(Method, Req0, State);
        welcome ->
            welcome(Method, Req0, State);
        false ->
            Req0
    end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

index(<<"GET">>, Req0, State) ->
    % AdmUserId = maps:get(adm_user_id, State, []),
    {ok, Body} = imboy_dtl:template(adm_index_dtl, [
         {"coversation_online_user", imboy_syn:count_user()}
         , {"coversation_online_device", imboy_syn:count()}
    ] ++ imboy_dtl:imadm_param(State), imadm),

    % {ok, Body} = file:read_file(iolist_to_binary([code:priv_dir(imadm), "/template/adm_index_dtl.html"])),
    cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/html; charset=utf-8">>
        , <<"Access-Control-Allow-Origin">> => <<"*">>
    }, Body, Req0).


welcome(<<"GET">>, Req0, State) ->
    % AdmUserId = maps:get(adm_user_id, State, []),
    {ok, Body} = imboy_dtl:template(adm_welcome_dtl, [
         {"coversation_online_user", imboy_syn:count_user()}
         , {"coversation_online_device", imboy_syn:count()}
    ] ++ imboy_dtl:imadm_param(State), imadm),

    % {ok, Body} = file:read_file(iolist_to_binary([code:priv_dir(imadm), "/template/adm_welcome_dtl.html"])),
    cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/html; charset=utf-8">>
        , <<"Access-Control-Allow-Origin">> => <<"*">>
    }, Body, Req0).

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
