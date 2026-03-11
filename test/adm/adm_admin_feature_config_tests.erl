-module(adm_admin_feature_config_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

init_config_features_success_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"GET">> end}
        ]},
        {adm_user_logic, [
            {'find', 3, fun(1001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 1001, <<"role_id">> => [1]}
            end}
        ]},
        {imboy_feature, [
            {'all', 0, fun() ->
                #{<<"core">> => true, <<"channel">> => false}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        {ok, RespReq, State} = adm_admin_handler:init(#{}, #{action => config_features, adm_user_id => 1001}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(#{<<"core">> => true, <<"channel">> => false}, maps:get(payload, RespReq)),
        ?assertEqual(#{adm_user_id => 1001}, State)
    end).

init_config_features_forbidden_without_settings_permission_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"GET">> end}
        ]},
        {adm_user_logic, [
            {'find', 3, fun(3001, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 3001, <<"role_id">> => [3]}
            end}
        ]},
        {imboy_feature, [
            {'all', 0, fun() -> erlang:error(should_not_be_called) end}
        ]},
        {elib_response, [
            {'error', 3, fun(Req, Msg, Code) ->
                Req#{response_status => 403, error_msg => Msg, error_code => Code}
            end}
        ]}
    ], fun() ->
        {ok, RespReq, _State} = adm_admin_handler:init(#{}, #{action => config_features, adm_user_id => 3001}),
        ?assertEqual(403, maps:get(response_status, RespReq)),
        ?assertEqual(?ERR_FORBIDDEN, maps:get(error_code, RespReq)),
        ?assertEqual(<<"无权限操作"/utf8>>, maps:get(error_msg, RespReq)),
        ?assertEqual(0, meck:num_calls(imboy_feature, all, 0))
    end).
