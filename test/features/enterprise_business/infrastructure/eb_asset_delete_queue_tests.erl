-module(eb_asset_delete_queue_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

drain_uses_claimed_scope_and_uuid_test_() ->
    drain_case(ok, <<"done">>, {ok, []}).

missing_object_is_acknowledged_test_() ->
    drain_case({error, not_found}, <<"done">>, {ok, []}).

delete_failure_is_reported_test_() ->
    drain_case(
        {error, unavailable},
        <<"retry_wait">>,
        {ok, [
            #{
                organization_id => 11,
                workspace_id => 22,
                asset_id => 33,
                reason => unavailable
            }
        ]}
    ).

stale_ack_is_not_success_test_() ->
    drain_case(
        ok,
        <<"stale_token">>,
        {error, {ack_failed, {ack_rejected, <<"stale_token">>}}}
    ).

ack_error_is_not_success_test_() ->
    drain_case(
        ok,
        {error, disconnected},
        {error, {ack_failed, disconnected}}
    ).

purge_reports_only_its_own_scope_failures_test_() ->
    Own = #{organization_id => 99, workspace_id => 88, asset_id => 44, reason => unavailable},
    Foreign = #{organization_id => 11, workspace_id => 22, asset_id => 33, reason => unavailable},
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'with_tx', 2, fun(_, [{reraise, true}]) ->
                    {ok, #{deleted => 0}}
                end}
            ]},
            {eb_pg_asset_delete_queue, [
                {'drain', 3, fun(99, 88, 1) ->
                    {ok, [Foreign, Own]}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {ok, #{deleted => 0, object_delete_failures => [Own]}},
                eb_pg_purge:purge_batch(99, 88, #{now => 1, batch_limit => 1})
            )
        end
    ).

drain_case(DeleteResult, AckResult, Expected) ->
    Calls = atomics:new(1, []),
    Frozen = #{
        <<"storage_driver">> => <<"disk">>,
        <<"storage_namespace">> => <<"test">>,
        <<"storage_container">> => <<"assets">>,
        <<"storage_locator_sha256">> => <<"digest">>
    },
    Identity = #{driver => disk, key => <<"claimed-key">>},
    Row = maps:merge(
        maps:from_list([{<<"out_", Key/binary>>, Value} || {Key, Value} <- maps:to_list(Frozen)]),
        #{
            <<"out_organization_id">> => 11,
            <<"out_workspace_id">> => 22,
            <<"out_asset_id">> => 33,
            <<"out_object_key">> => <<"claimed-key">>
        }
    ),
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 2, fun
                    (<<"SELECT out_organization_id,", _/binary>>, [Token]) ->
                        ?assertEqual(16, byte_size(epgsql_codec_uuid:encode(Token, uuid, []))),
                        case atomics:add_get(Calls, 1, 1) of
                            1 -> {ok, [Row]};
                            2 -> {ok, []}
                        end;
                    (<<"SELECT fn_v5_ack_eb", _/binary>>, [11, 22, 33, Token, Success]) ->
                        ?assertEqual(16, byte_size(epgsql_codec_uuid:encode(Token, uuid, []))),
                        ?assertEqual(
                            DeleteResult =:= ok orelse DeleteResult =:= {error, not_found}, Success
                        ),
                        case AckResult of
                            {error, _} -> AckResult;
                            _ -> {ok, [#{<<"fn_v5_ack_eb">> => AckResult}]}
                        end
                end}
            ]},
            {attachment_storage, [
                {'frozen_location', 2, fun(Key, ActualFrozen) ->
                    ?assertEqual(<<"claimed-key">>, Key),
                    ?assertEqual(Frozen, ActualFrozen),
                    {ok, Identity}
                end},
                {'delete_snapshot', 1, fun(ActualIdentity) ->
                    ?assertEqual(Identity, ActualIdentity),
                    DeleteResult
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(Expected, eb_pg_asset_delete_queue:drain(99, 88, 2)),
            ?assertEqual(1, meck:num_calls(attachment_storage, frozen_location, '_')),
            ?assertEqual(1, meck:num_calls(attachment_storage, delete_snapshot, '_')),
            ?assertEqual(
                1,
                meck:num_calls(
                    elib_pg,
                    query,
                    [<<"SELECT fn_v5_ack_eb($1, $2, $3, $4, $5)">>, '_']
                )
            )
        end
    ).
