-module(channel_reaction_summary_migration_tests).

-include_lib("eunit/include/eunit.hrl").

empty_reaction_summary_uses_json_object_test() ->
    {ok, Up} = file:read_file(
        "priv/migrations/00000084_channel_reaction_summary_empty_object.up.sql"
    ),
    ?assertNotEqual(
        nomatch,
        binary:match(Up, <<"COALESCE((">>)
    ),
    ?assertNotEqual(
        nomatch,
        binary:match(Up, <<"), '{}'::jsonb)">>)
    ).
