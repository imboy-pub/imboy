-module(message_dedup_migration_tests).

-include_lib("eunit/include/eunit.hrl").

message_cross_timestamp_dedup_migration_test() ->
    {ok, Up} = file:read_file(
        "priv/migrations/00000083_message_cross_timestamp_dedup.up.sql"
    ),
    {ok, Down} = file:read_file(
        "priv/migrations/00000083_message_cross_timestamp_dedup.down.sql"
    ),
    ?assertNotEqual(nomatch, binary:match(Up, <<"FOREACH message_table">>)),
    ?assertNotEqual(nomatch, binary:match(Up, <<"HAVING min(created_at) <> max(created_at)">>)),
    ?assertNotEqual(nomatch, binary:match(Up, <<"ERRCODE = 'unique_violation'">>)),
    {TriggerPos, _} = binary:match(Up, <<"CREATE TRIGGER trg_msg_c2c_cross_timestamp_dedup">>),
    {PrecheckPos, _} = binary:match(Up, <<"FOREACH message_table">>),
    ?assert(TriggerPos < PrecheckPos),
    ?assertNotEqual(nomatch, binary:match(Up, <<"pg_advisory_xact_lock">>)),
    ?assertNotEqual(nomatch, binary:match(Up, <<"WHERE msg_id = $1">>)),
    ?assertNotEqual(nomatch, binary:match(Up, <<"existing_created_at = NEW.created_at">>)),
    lists:foreach(
        fun(Table) ->
            Trigger = <<"trg_", Table/binary, "_cross_timestamp_dedup">>,
            ?assertNotEqual(nomatch, binary:match(Up, Trigger)),
            ?assertNotEqual(nomatch, binary:match(Down, Trigger))
        end,
        [<<"msg_c2c">>, <<"msg_c2g">>, <<"msg_c2s">>, <<"msg_s2c">>]
    ),
    ?assertNotEqual(
        nomatch,
        binary:match(Down, <<"DROP FUNCTION IF EXISTS public.fn_message_cross_timestamp_dedup()">>)
    ).
