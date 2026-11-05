-module(eb_direct_purge_mixed_pg_tests).
-include_lib("eunit/include/eunit.hrl").

mixed_purge_test_() ->
    {setup, fun() -> eunit_runner:eunit_setup_with_db() end, fun eunit_runner:eunit_cleanup_db/1,
        fun(_) -> {timeout, 90, fun mixed_purge/0} end}.

mixed_purge() ->
    Org = 99006101,
    Ws = 99006102,
    Asset = 99006105,
    Deferred = 99006301,
    Ready = 99006302,
    %% The clone is dedicated to this test: seed synthetic rows with all
    %% production guards re-enabled before the actual purge transaction.
    ok = elib_pg:with_tx(fun(Conn) ->
        {ok, _} = elib_pg:query(Conn, <<"SET LOCAL session_replication_role=replica">>, []),
        {ok, _} = elib_pg:execute(
            Conn,
            <<
                "INSERT INTO enterprise_message(id,organization_id,workspace_id,conversation_id,"
                "sender_type,sender_contact_id,client_msg_id,retention_days,retain_until)"
                " SELECT $1,c.organization_id,c.workspace_id,c.id,'contact',c.contact_id,$2,1,now()-interval '1 hour'"
                " FROM enterprise_conversation c JOIN enterprise_asset a ON a.conversation_id=c.id"
                " WHERE a.id=$3"
            >>,
            [Deferred, <<"direct-purge-deferred">>, Asset]
        ),
        {ok, _} = elib_pg:execute(
            Conn,
            <<
                "INSERT INTO enterprise_message(id,organization_id,workspace_id,conversation_id,"
                "sender_type,sender_contact_id,client_msg_id,retention_days,retain_until)"
                " SELECT $1,c.organization_id,c.workspace_id,c.id,'contact',c.contact_id,$2,1,now()-interval '1 hour'"
                " FROM enterprise_conversation c JOIN enterprise_asset a ON a.conversation_id=c.id"
                " WHERE a.id=$3"
            >>,
            [Ready, <<"direct-purge-ready">>, Asset]
        ),
        {ok, _} = elib_pg:execute(
            Conn,
            <<"UPDATE enterprise_asset SET message_id=$1,retain_until=now()-interval '1 hour' WHERE id=$2">>,
            [Deferred, Asset]
        ),
        {ok, _} = elib_pg:execute(
            Conn,
            <<
                "UPDATE attachment SET cipher='synthetic' WHERE path="
                "(SELECT object_key FROM enterprise_asset_direct_link WHERE asset_id=$1)"
            >>,
            [Asset]
        ),
        ok
    end),
    Now = erlang:system_time(second),
    {ok, First} = eb_pg_purge:purge_batch(Org, Ws, #{now => Now, batch_limit => 10}),
    ?assertEqual([Ready], maps:get(purged, First)),
    ?assertMatch([{Deferred, {ineligible, direct_retirement_deferred}}], maps:get(skipped, First)),
    ?assertEqual(
        1, scalar(<<"SELECT count(*) AS n FROM enterprise_message WHERE id=$1">>, [Deferred])
    ),
    ?assertEqual(
        0, scalar(<<"SELECT count(*) AS n FROM enterprise_message WHERE id=$1">>, [Ready])
    ),
    ?assertEqual(
        1,
        scalar(
            <<"SELECT count(*) AS n FROM enterprise_direct_purge_wait WHERE item_kind='message' AND item_id=$1">>,
            [Deferred]
        )
    ),
    {ok, Second} = eb_pg_purge:purge_batch(Org, Ws, #{now => Now, batch_limit => 10}),
    ?assertEqual([], maps:get(purged, Second)),
    {ok, _} = elib_pg:execute(
        <<
            "UPDATE enterprise_direct_purge_wait SET next_retry_at=clock_timestamp()-interval '1 second'"
            " WHERE item_kind='message' AND item_id=$1"
        >>,
        [Deferred]
    ),
    {ok, Third} = eb_pg_purge:purge_batch(Org, Ws, #{now => Now, batch_limit => 10}),
    ?assertEqual([], maps:get(purged, Third)),
    ?assertEqual(
        2,
        scalar(
            <<"SELECT attempts AS n FROM enterprise_direct_purge_wait WHERE item_kind='message' AND item_id=$1">>,
            [Deferred]
        )
    ),
    ?assertEqual(
        1,
        scalar(
            <<"SELECT count(*) AS n FROM attach_pending WHERE confirmed_attachment_id IS NOT NULL AND direct_state='CONFIRMED' AND object_key=(SELECT object_key FROM enterprise_asset_direct_link WHERE asset_id=$1)">>,
            [Asset]
        )
    ).

scalar(Sql, Params) ->
    {ok, [#{<<"n">> := Value}]} = elib_pg:query(Sql, Params),
    Value.
