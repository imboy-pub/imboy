-module(attachment_delete_ack_tests).
-include_lib("eunit/include/eunit.hrl").
ack_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun legal_statuses/0,
        fun stale_and_missing_rejected/0,
        fun db_error_propagates/0,
        fun eb_drain_preserves_ack_failure/0,
        fun ordinary_worker_does_not_count_failed_delete/0,
        fun ordinary_ack_db_failure_is_not_success/0,
        fun empty_ack_rows_rejected/0
    ]}.
setup() ->
    meck:new(elib_pg, [non_strict, no_link]),
    meck:new(attachment_storage, [passthrough, no_link]),
    ok.
cleanup(_) ->
    meck:unload(elib_pg),
    meck:unload(attachment_storage).
respond(Status) ->
    meck:expect(elib_pg, query, fun(_, _, _) -> {ok, [#{<<"fn_v5_ack_ordinary">> => Status}]} end),
    meck:expect(elib_pg, query, fun(_, _) -> {ok, [#{<<"fn_v5_ack_eb">> => Status}]} end).
legal_statuses() ->
    [
        begin
            respond(Status),
            ?assertEqual(ok, attachment_gc_repo:v5_ack(conn, <<"key">>, <<"token">>, Ok)),
            ?assertEqual(ok, eb_pg_asset_delete_queue:ack(1, 2, 3, <<"token">>, Ok))
        end
     || {Status, Ok} <- [{<<"done">>, true}, {<<"retry_wait">>, false}, {<<"blocked">>, false}]
    ].
stale_and_missing_rejected() ->
    [
        begin
            respond(Status),
            ?assertMatch({error, _}, attachment_gc_repo:v5_ack(conn, <<"key">>, <<"token">>, Ok)),
            ?assertMatch({error, _}, eb_pg_asset_delete_queue:ack(1, 2, 3, <<"token">>, Ok))
        end
     || {Status, Ok} <- [
            {<<"stale_token">>, true},
            {<<"stale_token">>, false},
            {<<"missing">>, true},
            {<<"not_leased">>, true},
            {<<"done">>, false},
            {<<"retry_wait">>, true}
        ]
    ].
db_error_propagates() ->
    meck:expect(elib_pg, query, fun(_, _, _) -> {error, db_down} end),
    meck:expect(elib_pg, query, fun(_, _) -> {error, db_down} end),
    ?assertEqual({error, db_down}, attachment_gc_repo:v5_ack(conn, <<"key">>, <<"token">>, true)),
    ?assertEqual({error, db_down}, eb_pg_asset_delete_queue:ack(1, 2, 3, <<"token">>, true)).
eb_drain_preserves_ack_failure() ->
    meck:expect(attachment_storage, frozen_location, fun(_, _) -> {ok, #{driver => disk}} end),
    meck:expect(attachment_storage, delete_snapshot, fun(_) -> ok end),
    meck:expect(elib_pg, query, fun(Sql, _) ->
        case binary:match(Sql, <<"fn_v5_claim_eb">>) of
            nomatch ->
                {error, ack_db_down};
            _ ->
                {ok, [
                    #{
                        <<"out_organization_id">> => 1,
                        <<"out_workspace_id">> => 2,
                        <<"out_asset_id">> => 3,
                        <<"out_object_key">> => <<"key">>
                    }
                ]}
        end
    end),
    ?assertEqual(
        {error, {ack_failed, ack_db_down}}, eb_pg_asset_delete_queue:drain(1, 2, 1, <<"token">>)
    ).
ordinary_worker_does_not_count_failed_delete() ->
    meck:expect(attachment_storage, frozen_location, fun(_, _) ->
        {ok, #{namespace => <<"n">>, container => <<"c">>}}
    end),
    put(ack_claim_count, 0),
    meck:expect(elib_pg, query, fun(_, Sql0, _) ->
        Sql = iolist_to_binary(Sql0),
        case binary:match(Sql, <<"fn_v5_claim_ordinary">>) of
            nomatch ->
                {ok, [#{<<"fn_v5_ack_ordinary">> => <<"retry_wait">>}]};
            _ ->
                case get(ack_claim_count) of
                    0 ->
                        put(ack_claim_count, 1),
                        {ok, [#{<<"out_path">> => <<"key">>}]};
                    _ ->
                        {ok, []}
                end
        end
    end),
    ?assertEqual(
        {ok, 0},
        attachment_delete_worker:run_batch(conn, fun(_, _, _, _) -> {error, storage_down} end)
    ).

ordinary_ack_db_failure_is_not_success() ->
    meck:expect(attachment_storage, frozen_location, fun(_, _) ->
        {ok, #{namespace => <<"n">>, container => <<"c">>}}
    end),
    meck:expect(elib_pg, query, fun(_, Sql0, _) ->
        case binary:match(iolist_to_binary(Sql0), <<"fn_v5_claim_ordinary">>) of
            nomatch -> {error, ack_db_down};
            _ -> {ok, [#{<<"out_path">> => <<"key">>}]}
        end
    end),
    ?assertEqual(
        {error, {batch_failed, {ack_failed, ack_db_down}, 0}},
        attachment_delete_worker:run_batch(conn, fun(_, _, _, _) -> ok end)
    ).
empty_ack_rows_rejected() ->
    meck:expect(elib_pg, query, fun(_, _, _) -> {ok, []} end),
    meck:expect(elib_pg, query, fun(_, _) -> {ok, []} end),
    ?assertEqual(
        {error, {invalid_ack_result, []}},
        attachment_gc_repo:v5_ack(conn, <<"key">>, <<"token">>, true)
    ),
    ?assertEqual(
        {error, {invalid_ack_result, []}}, eb_pg_asset_delete_queue:ack(1, 2, 3, <<"token">>, true)
    ).
