-module(adm_stats_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% adm_stats_handler 模块的 EUnit 测试
%%%
%%% 目标：覆盖统计总览、趋势与排行接口
%%%===================================================================

init_overview_aggregates_core_metrics_test_() ->
    ?WITH_MECKS([
        {elib_pg, [
            {'one', 2, fun(Sql, Params) ->
                ?assertEqual([], Params),
                case Sql of
                    <<"SELECT COUNT(*) FROM user">> -> {ok, #{count => 1000}};
                    <<"SELECT COUNT(*) FROM user WHERE created_at >= CURRENT_DATE">> -> {ok, #{count => 12}};
                    <<"SELECT COUNT(*) FROM group">> -> {ok, #{count => 321}};
                    <<"SELECT COUNT(*) FROM group WHERE created_at >= CURRENT_DATE">> -> {ok, #{count => 7}};
                    <<"SELECT COUNT(*) FROM msg_c2c WHERE created_at >= CURRENT_DATE">> -> {ok, #{count => 80}};
                    <<"SELECT COUNT(*) FROM msg_c2g WHERE created_at >= CURRENT_DATE">> -> {ok, #{count => 20}}
                end
            end}
        ]},
        {imboy_syn, [
            {'count_user', 0, fun() -> 56 end},
            {'count', 0, fun() -> 89 end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = #{method => <<"GET">>},
        {ok, RespReq, _State} = adm_stats_handler:init(Req, #{action => overview}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        Payload = maps:get(payload, RespReq),
        ?assertEqual(1000, maps:get(total_users, Payload)),
        ?assertEqual(12, maps:get(today_users, Payload)),
        ?assertEqual(321, maps:get(total_groups, Payload)),
        ?assertEqual(7, maps:get(today_groups, Payload)),
        ?assertEqual(56, maps:get(online_users, Payload)),
        ?assertEqual(89, maps:get(online_devices, Payload)),
        ?assertEqual(100, maps:get(today_messages, Payload))
    end).

init_user_stats_supports_custom_days_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'int', 3, fun(days, _Req, _Default) -> {ok, 14} end}
        ]},
        {elib_pg, [
            {'query', 2, fun(Sql, Params) ->
                ?assertEqual([], Params),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"FROM user">>)),
                {ok, [#{<<"date">> => <<"2026-02-20">>, <<"count">> => 3}]}
            end},
            {'one', 2, fun(Sql, Params) ->
                ?assertEqual(<<"SELECT COUNT(*) FROM user WHERE status = $1">>, Sql),
                case Params of
                    [1] -> {ok, #{count => 900}};
                    [0] -> {ok, #{count => 20}};
                    [-1] -> {ok, #{count => 5}}
                end
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = #{method => <<"GET">>},
        {ok, RespReq, _State} = adm_stats_handler:init(Req, #{action => user}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        Payload = maps:get(payload, RespReq),
        ?assertEqual(900, maps:get(active_users, Payload)),
        ?assertEqual(20, maps:get(banned_users, Payload)),
        ?assertEqual(5, maps:get(deleted_users, Payload)),
        ?assertEqual(1, length(maps:get(daily_new, Payload)))
    end).

init_message_stats_returns_daily_series_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'int', 3, fun(days, _Req, _Default) -> {ok, 7} end}
        ]},
        {elib_pg, [
            {'query', 2, fun(Sql, Params) ->
                ?assertEqual([], Params),
                case binary:match(Sql, <<"FROM msg_c2c">>) of
                    nomatch ->
                        ?assertNotEqual(nomatch, binary:match(Sql, <<"FROM msg_c2g">>)),
                        {ok, [#{<<"date">> => <<"2026-02-21">>, <<"count">> => 8}]};
                    _ ->
                        {ok, [#{<<"date">> => <<"2026-02-21">>, <<"count">> => 13}]}
                end
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = #{method => <<"GET">>},
        {ok, RespReq, _State} = adm_stats_handler:init(Req, #{action => message}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        Payload = maps:get(payload, RespReq),
        ?assertEqual(1, length(maps:get(daily_c2c, Payload))),
        ?assertEqual(1, length(maps:get(daily_c2g, Payload)))
    end).

init_ranking_group_member_metric_test_() ->
    ?WITH_MECKS([
        {elib_pg, [
            {'query', 2, fun(Sql, [Limit]) ->
                ?assertEqual(5, Limit),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"FROM \"group\" g">>)),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"member_count as metric">>)),
                {ok, [#{<<"id">> => 1, <<"name">> => <<"tech">>, <<"metric">> => 128}]}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = #{
            method => <<"GET">>,
            qs => <<"type=group&metric=member&limit=5">>
        },
        {ok, RespReq, _State} = adm_stats_handler:init(Req, #{action => ranking}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        Payload = maps:get(payload, RespReq),
        Items = maps:get(list, Payload),
        ?assertEqual(false, maps:is_key(items, Payload)),
        [First | _] = Items,
        ?assertEqual(128, maps:get(<<"metric">>, First))
    end).

init_ranking_falls_back_to_user_message_test_() ->
    ?WITH_MECKS([
        {elib_pg, [
            {'query', 2, fun(Sql, [Limit]) ->
                ?assertEqual(3, Limit),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"FROM \"user\" u">>)),
                {ok, [#{<<"id">> => 7, <<"metric">> => 99}]}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = #{
            method => <<"GET">>,
            qs => <<"type=unknown&metric=unknown&limit=3">>
        },
        {ok, RespReq, _State} = adm_stats_handler:init(Req, #{action => ranking}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        Payload = maps:get(payload, RespReq),
        [First | _] = maps:get(list, Payload),
        ?assertEqual(false, maps:is_key(items, Payload)),
        ?assertEqual(99, maps:get(<<"metric">>, First))
    end).
