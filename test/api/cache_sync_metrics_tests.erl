-module(cache_sync_metrics_tests).
-include_lib("eunit/include/eunit.hrl").
-export([sequence_evidence/0, stopped_sync/0]).

sequence_evidence_is_distinct_and_payload_free_test() -> isolated(sequence_evidence).
stopped_sync_has_no_fabricated_protocol_identity_test() -> isolated(stopped_sync).

isolated(Function) ->
    {ok, Peer, _} = peer:start_link(#{
        connection => standard_io,
        args => ["+S", "1:1", "+A", "1", "-pz" | code:get_path()]
    }),
    try
        peer:call(Peer, ?MODULE, Function, [])
    after
        peer:stop(Peer)
    end.

sequence_evidence() ->
    ets:new(imboy_cache_meta, [named_table, public, set]),
    Peer = 'metrics_peer@synthetic',
    Boot = <<7:128>>,
    ets:insert(imboy_cache_meta, [
        {node_boot, {node(), <<8:128>>}},
        {{peer_incarnation, Peer}, Boot},
        {{seq, {Peer, Boot}}, 9},
        {{covered_seq, {Peer, Boot}}, {0, 3}},
        {{snapshot_seq_floor, {Peer, Boot}}, 6},
        {{peer_epoch, Peer}, 2},
        {{hash_divergent, Peer}, true},
        {{journal, 1}, secret_payload},
        {{rev, secret_key}, secret_revision}
    ]),
    try
        M = metrics_handler:cache_sync_metrics(),
        L = #{peer => Peer, boot => binary:encode_hex(Boot, lowercase)},
        ?assertEqual(9, maps:get({imboy_cache_sync_observed_seq, L}, M)),
        ?assertEqual(3, maps:get({imboy_cache_sync_applied_prefix, L}, M)),
        ?assertEqual(6, maps:get({imboy_cache_sync_snapshot_floor, L}, M)),
        ?assertEqual(2, maps:get({imboy_cache_sync_peer_epoch, #{peer => Peer}}, M)),
        ?assertEqual(1, maps:get({imboy_cache_sync_hash_divergent, #{peer => Peer}}, M)),
        [Info] = [I || {{imboy_cache_sync_protocol_info, I}, 1} <- maps:to_list(M)],
        ?assertEqual(<<"2">>, maps:get(protocol, Info)),
        ?assertEqual(<<"1">>, maps:get(supported_min, Info)),
        ?assertEqual(<<"2">>, maps:get(supported_max, Info)),
        ?assertEqual(
            binary:encode_hex(imboy_cache_sync_proto:config_hash(), lowercase),
            maps:get(config_hash, Info)
        ),
        Text = metrics_handler:format_prometheus(#{counters => M, metric_gauges => M}),
        ?assertNotEqual(
            nomatch, binary:match(Text, <<"# TYPE imboy_cache_sync_observed_seq gauge">>)
        ),
        ?assertEqual(nomatch, binary:match(Text, <<"secret">>)),
        assert_json_response_has_protocol_identity(),
        ets:insert(imboy_cache_meta, {{covered_seq, {Peer, Boot}}, 99}),
        ?assertEqual(
            -1,
            maps:get(
                {imboy_cache_sync_applied_prefix, L}, metrics_handler:cache_sync_metrics()
            )
        ),
        ets:delete(imboy_cache_meta, {covered_seq, {Peer, Boot}}),
        ?assertEqual(
            -1,
            maps:get(
                {imboy_cache_sync_applied_prefix, L},
                metrics_handler:cache_sync_metrics()
            )
        )
    after
        ets:delete(imboy_cache_meta)
    end.

assert_json_response_has_protocol_identity() ->
    Modules = [cowboy_req, elib_metric, elib_response],
    meck:new(Modules, [non_strict, no_link]),
    try
        meck:expect(cowboy_req, peer, fun(_) -> {{127, 0, 0, 1}, 12345} end),
        meck:expect(cowboy_req, header, fun(_, _, _) -> <<>> end),
        meck:expect(elib_metric, get_all_metrics, fun() -> #{counters => #{}} end),
        meck:expect(elib_response, success, fun(Req, Payload, _) ->
            Req#{payload => Payload}
        end),
        {ok, Req, _} = metrics_handler:init(#{}, #{}),
        Payload = maps:get(payload, Req),
        ?assertNot(maps:is_key(metric_gauges, Payload)),
        Body = jsone:encode(Payload),
        ?assertNotEqual(nomatch, binary:match(Body, <<"imboy_cache_sync_protocol_info_">>)),
        ?assertNotEqual(nomatch, binary:match(Body, <<"imboy_cache_sync_applied_prefix_">>)),
        ?assertEqual(nomatch, binary:match(Body, <<"secret">>))
    after
        meck:unload(Modules)
    end.

stopped_sync() ->
    M = metrics_handler:cache_sync_metrics(),
    ?assertEqual(-1, maps:get(imboy_cache_sync_status, M)),
    ?assertEqual([], [I || {{imboy_cache_sync_protocol_info, I}, _} <- maps:to_list(M)]).
