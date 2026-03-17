-module(adm_app_ddl_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

mock_request() ->
    #{method => <<"GET">>, path => <<"/adm/app_ddl">>}.

init_false_action_passthrough_test_() ->
    ?WITH_MECK(cowboy_req, [
        {'method', 1, fun(_Req) -> <<"GET">> end}
    ], fun() ->
        Req = mock_request(),
        {ok, RespReq, State} = adm_app_ddl_handler:init(Req, #{action => false, adm_user_id => 100}),
        ?assertEqual(Req, RespReq),
        ?assertEqual(#{adm_user_id => 100}, State)
    end).

init_index_action_returns_paged_payload_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"GET">> end}
        ]},
        {elib_param, [
            {'int', 3, fun(ajax, _Req, _Default) -> {ok, 1} end},
            {'page', 1, fun(_Req) -> {1, 20} end}
        ]},
        {app_ddl_repo, [
            {'tablename', 0, fun() -> <<"app_ddl">> end}
        ]},
        {elib_pg, [
            {'page_with_total', 6, fun(Tb, Column, Where, OrderBy, Page, Size) ->
                ?assertEqual(<<"app_ddl">>, Tb),
                ?assertEqual(<<"id, ddl, down_ddl,old_vsn,new_vsn,status,updated_at,created_at">>, Column),
                ?assertEqual(#{}, Where),
                ?assertEqual(<<"id desc">>, OrderBy),
                ?assertEqual(1, Page),
                ?assertEqual(20, Size),
                {ok, #{list => [], total => 0, page => 1, size => 20}}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        {ok, RespReq, State} = adm_app_ddl_handler:init(Req, #{action => index}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(0, maps:get(total, maps:get(payload, RespReq))),
        ?assertEqual(#{}, State)
    end).

init_index_non_get_returns_405_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"POST">> end},
            {'reply', 4, fun(405, _Headers, <<"Method Not Allowed">>, Req) ->
                Req#{response_status => 405}
            end}
        ]},
        {elib_param, [
            {'int', 3, fun(ajax, _Req, _Default) -> {ok, 1} end}
        ]}
    ], fun() ->
        Req = mock_request(),
        {ok, RespReq, _State} = adm_app_ddl_handler:init(Req, #{action => index}),
        ?assertEqual(405, maps:get(response_status, RespReq))
    end).

init_save_action_persists_post_values_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"POST">> end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"new_vsn">> => <<"1.0.0">>,
                    <<"old_vsn">> => <<"0.9.0">>,
                    <<"status">> => 1,
                    <<"ddl">> => <<"CREATE TABLE test;">>,
                    <<"down_ddl">> => <<"DROP TABLE test;">>
                }
            end}
        ]},
        {app_ddl_ds, [
            {'save', 6, fun(AdmUserId, NewVsn, OldVsn, Status, Ddl, DownDdl) ->
                ?assertEqual(100, AdmUserId),
                ?assertEqual(<<"1.0.0">>, NewVsn),
                ?assertEqual(<<"0.9.0">>, OldVsn),
                ?assertEqual(1, Status),
                ?assertEqual(<<"CREATE TABLE test;">>, Ddl),
                ?assertEqual(<<"DROP TABLE test;">>, DownDdl),
                {ok, 1}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, Payload, <<"success."/utf8>>) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        {ok, RespReq, State} = adm_app_ddl_handler:init(Req, #{action => save, adm_user_id => 100}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(<<"1.0.0">>, maps:get(<<"new_vsn">>, maps:get(payload, RespReq))),
        ?assertEqual(#{adm_user_id => 100}, State)
    end).

init_save_non_post_passthrough_test_() ->
    ?WITH_MECK(cowboy_req, [
        {'method', 1, fun(_Req) -> <<"GET">> end}
    ], fun() ->
        Req = mock_request(),
        {ok, RespReq, State} = adm_app_ddl_handler:init(Req, #{action => save, adm_user_id => 100}),
        ?assertEqual(Req, RespReq),
        ?assertEqual(#{adm_user_id => 100}, State)
    end).

init_delete_action_success_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"DELETE">> end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) -> #{<<"id">> => <<"test_id">>} end}
        ]},
        {app_ddl_ds, [
            {'delete', 1, fun(<<"test_id">>) -> {ok, 1} end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, Payload, <<"success."/utf8>>) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        {ok, RespReq, _State} = adm_app_ddl_handler:init(Req, #{action => delete}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(<<"test_id">>, maps:get(<<"id">>, maps:get(payload, RespReq)))
    end).

init_delete_action_error_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"DELETE">> end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) -> #{<<"id">> => <<"invalid_id">>} end}
        ]},
        {app_ddl_ds, [
            {'delete', 1, fun(<<"invalid_id">>) -> {error, not_found} end}
        ]},
        {elib_response, [
            {'error', 3, fun(Req, <<"删除失败"/utf8>>, _Code) ->
                Req#{response_status => 500}
            end}
        ]}
    ], fun() ->
        Req = mock_request(),
        {ok, RespReq, _State} = adm_app_ddl_handler:init(Req, #{action => delete}),
        ?assertEqual(500, maps:get(response_status, RespReq))
    end).

init_delete_non_delete_passthrough_test_() ->
    ?WITH_MECK(cowboy_req, [
        {'method', 1, fun(_Req) -> <<"GET">> end}
    ], fun() ->
        Req = mock_request(),
        {ok, RespReq, _State} = adm_app_ddl_handler:init(Req, #{action => delete}),
        ?assertEqual(Req, RespReq)
    end).
