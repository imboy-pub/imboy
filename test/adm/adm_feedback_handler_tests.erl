-module(adm_feedback_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

module_loaded_test_() ->
    ?TEST_SIMPLE(fun() ->
        code:ensure_loaded(adm_feedback_handler),
        ?assertMatch({file, _}, code:is_loaded(adm_feedback_handler))
    end).

init_index_returns_normalized_json_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"GET">> end}
        ]},
        {elib_param, [
            {'int', 3, fun(ajax, _Req, _Default) -> {ok, 1} end},
            {'page', 1, fun(_Req) -> {1, 20} end}
        ]},
        {feedback_repo, [
            {'tablename', 0, fun() -> <<"feedback">> end}
        ]},
        {elib_pg, [
            {'page_with_total', 6, fun(_Tb, _Column, _Where, _Order, 1, 20) ->
                {ok, #{
                    list => [#{<<"feedback_id">> => 11, <<"user_id">> => 9, <<"body">> => <<"ok">>}],
                    items => [legacy],
                    total => 1,
                    page => 1,
                    size => 20
                }}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        {ok, Req, _State} = adm_feedback_handler:init(#{}, #{action => index}),
        Payload = maps:get(payload, Req),
        [Item] = maps:get(list, Payload),
        ?assertEqual(200, maps:get(response_status, Req)),
        ?assertEqual(<<"fb_11">>, maps:get(<<"feedback_id">>, Item)),
        ?assertEqual(false, maps:is_key(items, Payload))
    end).

init_reply_converts_reply_pid_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"POST">> end}
        ]},
        {adm_user_logic, [
            {'find', 3, fun(1, <<"id,nickname">>, _Key) -> #{<<"nickname">> => <<"管理员"/utf8>>} end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"feedback_id">> => <<"123">>, <<"feedback_reply_pid">> => <<"77">>, <<"body">> => <<"已处理"/utf8>>}
            end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> <<"2026-03-13T09:00:00Z">> end}
        ]},
        {feedback_ds, [
            {'add_reply', 1, fun(Data) ->
                ?assertEqual(123, maps:get(<<"feedback_id">>, Data)),
                ?assertEqual(77, maps:get(<<"feedback_reply_pid">>, Data)),
                ?assertEqual(1, maps:get(<<"replier_user_id">>, Data)),
                ok
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, _Payload, _Msg) -> Req#{response_status => 200} end}
        ]}
    ], fun() ->
        {ok, Req, _State} = adm_feedback_handler:init(#{}, #{action => reply, adm_user_id => 1}),
        ?assertEqual(200, maps:get(response_status, Req))
    end).

init_reply_invalid_feedback_id_returns_error_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"POST">> end}
        ]},
        {adm_user_logic, [
            {'find', 3, fun(1, <<"id,nickname">>, _Key) -> #{<<"nickname">> => <<"管理员"/utf8>>} end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) -> #{<<"feedback_id">> => <<"bad">>, <<"body">> => <<"x">>} end}
        ]},
        {elib_response, [
            {'error', 1, fun(Req) -> Req#{response_status => 400} end}
        ]}
    ], fun() ->
        {ok, Req, _State} = adm_feedback_handler:init(#{}, #{action => reply, adm_user_id => 1}),
        ?assertEqual(400, maps:get(response_status, Req))
    end).

workflow_config_get_permission_denied_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"GET">> end}
        ]},
        {adm_user_logic, [
            {'find', 3, fun(9, <<"id,role_id">>, _Key) -> #{<<"id">> => 9, <<"role_id">> => 3} end}
        ]},
        {adm_index_handler, [
            {'role_acl', 1, fun(3) -> {<<"viewer">>, [], []} end}
        ]},
        {elib_response, [
            {'error', 3, fun(Req, _Msg, Code) -> Req#{response_status => Code} end}
        ]}
    ], fun() ->
        {ok, Req, _State} = adm_feedback_handler:init(#{}, #{action => workflow_config, adm_user_id => 9}),
        ?assertEqual(?ERR_FORBIDDEN, maps:get(response_status, Req))
    end).

workflow_config_get_returns_default_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"GET">> end}
        ]},
        {adm_user_logic, [
            {'find', 3, fun(1, <<"id,role_id">>, _Key) -> #{<<"id">> => 1, <<"role_id">> => 2} end}
        ]},
        {adm_index_handler, [
            {'role_acl', 1, fun(2) -> {<<"ops">>, [<<"feedback:workflow:read">>], []} end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) -> Req#{response_status => 200, payload => Payload} end}
        ]}
    ], fun() ->
        persistent_term:erase({adm_feedback_handler, workflow_config}),
        {ok, Req, _State} = adm_feedback_handler:init(#{}, #{action => workflow_config, adm_user_id => 1}),
        Payload = maps:get(payload, Req),
        ?assertEqual(200, maps:get(response_status, Req)),
        ?assert(is_list(maps:get(<<"reply_templates">>, Payload))),
        ?assertEqual(24, maps:get(<<"sla_hours">>, Payload)),
        persistent_term:erase({adm_feedback_handler, workflow_config})
    end).

workflow_config_put_normalizes_templates_and_sla_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"PUT">> end}
        ]},
        {adm_user_logic, [
            {'find', 3, fun(1, <<"id,role_id">>, _Key) -> #{<<"id">> => 1, <<"role_id">> => 2} end}
        ]},
        {adm_index_handler, [
            {'role_acl', 1, fun(2) -> {<<"ops">>, [<<"feedback:workflow:write">>], []} end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"reply_templates">> => [<<" 模板A "/utf8>>, #{<<"text">> => <<"模板A"/utf8>>}, <<"模板B"/utf8>>],
                    <<"sla_hours">> => <<"9999">>
                }
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) -> Req#{response_status => 200, payload => Payload} end}
        ]}
    ], fun() ->
        persistent_term:erase({adm_feedback_handler, workflow_config}),
        {ok, Req, _State} = adm_feedback_handler:init(#{}, #{action => workflow_config, adm_user_id => 1}),
        Payload = maps:get(payload, Req),
        ?assertEqual(200, maps:get(response_status, Req)),
        ?assertEqual([<<"模板A"/utf8>>, <<"模板B"/utf8>>], maps:get(<<"reply_templates">>, Payload)),
        ?assertEqual(720, maps:get(<<"sla_hours">>, Payload)),
        persistent_term:erase({adm_feedback_handler, workflow_config})
    end).
