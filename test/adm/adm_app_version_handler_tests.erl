-module(adm_app_version_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% adm_app_version_handler 模块的 EUnit 测试
%%%
%%% 目标：覆盖版本列表分页、保存、删除等管理敏感动作
%%%===================================================================

init_index_pagination_success_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'int', 3, fun(Key, _Req, Default) ->
                ?assertEqual(ajax, Key),
                ?assertEqual(-2, Default),
                {ok, 1}
            end},
            {'page', 1, fun(_Req) -> {2, 15} end}
        ]},
        {app_version_repo, [
            {'tablename', 0, fun() -> <<"public.app_version">> end}
        ]},
        {elib_pg, [
            {'page_with_total', 6, fun(Tb, Column, Where, OrderBy, Page, Size) ->
                ?assertEqual(<<"public.app_version">>, Tb),
                ?assertEqual(<<"*">>, Column),
                ?assertEqual(#{}, Where),
                ?assertEqual(<<"sort desc, updated_at desc">>, OrderBy),
                ?assertEqual(2, Page),
                ?assertEqual(15, Size),
                {ok, #{
                    items => [#{<<"id">> => 9, <<"vsn">> => <<"2.1.0">>}],
                    page => Page,
                    size => Size,
                    total => 1,
                    total_pages => 1
                }}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = #{method => <<"GET">>},
        {ok, RespReq, NewState} = adm_app_version_handler:init(Req, #{action => index, trace => true}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(false, maps:is_key(action, NewState)),
        Payload = maps:get(payload, RespReq),
        ?assertEqual(1, maps:get(total, Payload))
    end).

init_save_converts_payload_and_calls_logic_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"id">> => 33,
                    <<"region_code">> => <<"us">>,
                    <<"type">> => <<"android">>,
                    <<"package_name">> => <<"pub.imboy.app">>,
                    <<"app_name">> => <<"IMBoy">>,
                    <<"vsn">> => <<"2.10.3">>,
                    <<"sign_key">> => <<"sig-k">>,
                    <<"download_url">> => <<"https://cdn.example/app.apk">>,
                    <<"description">> => <<"stable">>,
                    <<"force_update">> => <<"1">>,
                    <<"status">> => <<"2">>
                }
            end}
        ]},
        {adm_app_version_logic, [
            {'vsn_sort', 1, fun(Vsn) ->
                ?assertEqual(<<"2.10.3">>, Vsn),
                2010003
            end},
            {'save', 1, fun(Data) ->
                ?assertEqual(33, maps:get(id, Data)),
                ?assertEqual(<<"us">>, maps:get(region_code, Data)),
                ?assertEqual(<<"android">>, maps:get(type, Data)),
                ?assertEqual(2010003, maps:get(sort, Data)),
                ?assertEqual(1, maps:get(force_update, Data)),
                ?assertEqual(2, maps:get(status, Data)),
                ok
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, Payload, Msg) ->
                Req#{response_status => 200, payload => Payload, msg => Msg}
            end}
        ]}
    ], fun() ->
        Req = #{method => <<"POST">>},
        {ok, RespReq, _State} = adm_app_version_handler:init(Req, #{action => save}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(<<"success.">>, maps:get(msg, RespReq)),
        ?assertEqual(1, meck:num_calls(adm_app_version_logic, save, 1))
    end).

init_delete_calls_logic_with_expected_where_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"id">> => 88}
            end}
        ]},
        {adm_app_version_logic, [
            {'delete', 1, fun(Where) ->
                ?assertEqual(<<"status = 0 AND id = 88">>, Where),
                ok
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, Payload, Msg) ->
                Req#{response_status => 200, payload => Payload, msg => Msg}
            end}
        ]}
    ], fun() ->
        Req = #{method => <<"DELETE">>},
        {ok, RespReq, _State} = adm_app_version_handler:init(Req, #{action => delete}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(<<"success.">>, maps:get(msg, RespReq)),
        ?assertEqual(1, meck:num_calls(adm_app_version_logic, delete, 1))
    end).
