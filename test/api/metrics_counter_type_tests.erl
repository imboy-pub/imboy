-module(metrics_counter_type_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

http_text_path_preserves_system_gauges_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {peer, 1, fun(_) -> {{127, 0, 0, 1}, 12345} end},
                {header, 3, fun(_, _, _) -> <<"text/plain">> end},
                {reply, 4, fun(Status, _, Body, Req) -> Req#{status => Status, body => Body} end}
            ]},
            {elib_metric, [
                {get_all_metrics, 0, fun() ->
                    #{counters => #{e2ee_recovery_failed_total => 1}}
                end}
            ]}
        ],
        fun() ->
            {ok, Req, _} = metrics_handler:init(#{}, #{}),
            ?assertEqual(200, maps:get(status, Req)),
            Lines = binary:split(maps:get(body, Req), <<"\n">>, [global]),
            ?assertEqual(1, count(<<"# TYPE e2ee_recovery_failed_total counter">>, Lines)),
            ?assertEqual(1, count(<<"# TYPE imboy_ws_connections_current gauge">>, Lines)),
            ?assertEqual(1, count(<<"# TYPE erlang_vm_memory_bytes gauge">>, Lines))
        end
    ).

counter_families_have_one_type_declaration_test() ->
    Metrics = #{
        counters => #{
            e2ee_recovery_failed_total => 1,
            {e2ee_backup_op_failed_total, #{op => get}} => 2,
            {e2ee_backup_op_failed_total, #{op => put}} => 3
        }
    },
    Lines = lines(Metrics),
    ?assertEqual(1, count(<<"# TYPE e2ee_recovery_failed_total counter">>, Lines)),
    ?assertEqual(1, count(<<"# TYPE e2ee_backup_op_failed_total counter">>, Lines)),
    ?assert(lists:member(<<"e2ee_backup_op_failed_total{op=\"get\"} 2">>, Lines)),
    ?assert(lists:member(<<"e2ee_backup_op_failed_total{op=\"put\"} 3">>, Lines)).

system_gauges_keep_their_type_even_with_total_suffix_test() ->
    Counters = #{
        imboy_ws_connections_total => 2,
        {erlang_vm_memory_bytes_total, #{kind => total}} => 100,
        {erlang_vm_memory_bytes_total, #{kind => ets}} => 20,
        olm_otk_exhausted_total => 1
    },
    Gauges = maps:remove(olm_otk_exhausted_total, Counters),
    Lines = lines(#{counters => Counters, metric_gauges => Gauges}),
    ?assertEqual(1, count(<<"# TYPE imboy_ws_connections_current gauge">>, Lines)),
    ?assertEqual(1, count(<<"# TYPE erlang_vm_memory_bytes gauge">>, Lines)),
    ?assertEqual(1, count(<<"# TYPE olm_otk_exhausted_total counter">>, Lines)).

lines(Metrics) ->
    binary:split(metrics_handler:format_prometheus(Metrics), <<"\n">>, [global]).

count(Line, Lines) ->
    length([Found || Found <- Lines, Found =:= Line]).
