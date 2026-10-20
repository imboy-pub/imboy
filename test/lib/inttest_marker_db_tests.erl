-module(inttest_marker_db_tests).
-include_lib("eunit/include/eunit.hrl").

schema_mode_contract_test() ->
    ?assertEqual(full, inttest_marker_db:schema_mode(#{})),
    ?assertEqual(full, inttest_marker_db:schema_mode(#{schema_mode => full})),
    ?assertEqual(empty, inttest_marker_db:schema_mode(#{schema_mode => empty})),
    ?assertError({marker_db_invalid_schema_mode, typo},
        inttest_marker_db:schema_mode(#{schema_mode => typo})),
    ?assertError(marker_db_empty_schema_cannot_reuse_or_target,
        inttest_marker_db:schema_mode(#{schema_mode => empty, reuse => true})),
    ?assertError(marker_db_empty_schema_cannot_reuse_or_target,
        inttest_marker_db:schema_mode(#{schema_mode => empty, migration_target => 1})),
    %% Invalid modes fail before configuration, application startup or PG access.
    ?assertError({marker_db_invalid_schema_mode, typo},
        inttest_marker_db:provision(#{schema_mode => typo})),
    ?assertError(marker_db_empty_schema_cannot_reuse_or_target,
        inttest_marker_db:provision(#{schema_mode => empty, reuse => true})).
