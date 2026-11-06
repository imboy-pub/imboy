%% Pure regression checks: no PG, Garage, listeners or application startup.
%% 纯回归：不启动数据库、Garage、监听器或应用。
-module(attachment_w403_owned_fixture_tests).
-include_lib("eunit/include/eunit.hrl").

owned_manifest_namespace_test() ->
    ?assertEqual(
        ok,
        attachment_w403_owned_fixture:assert_fixture_database(
            <<"attv5_chain_owned">>, #{}, [], "unused"
        )
    ).

ordinary_database_rejected_test() ->
    ?assertError(
        {no_go, w403_fixture_database_unverified},
        attachment_w403_owned_fixture:assert_fixture_database(
            <<"imboy_production">>, #{}, [], "unused"
        )
    ).

chain_namespace_test() ->
    ?assertEqual(
        ok,
        attachment_w403_owned_fixture:assert_chain_fixture_database(
            <<"attv5_chain_owned">>, #{}, [], "unused"
        )
    ),
    ?assertError(
        {no_go, w403_chain_database_unverified},
        attachment_w403_owned_fixture:assert_chain_fixture_database(
            <<"attv5_other">>, #{}, [], "unused"
        )
    ).

native_ci_name_requires_full_identity_test() ->
    ?assertError(
        {badmatch, nomatch},
        attachment_w403_owned_fixture:assert_fixture_database(
            <<"imboy_chain_ci_unverified">>, #{}, [], "unused"
        )
    ).

partial_cleanup_test() ->
    ?assertEqual(ok, attachment_w403_owned_fixture:cleanup(undefined)),
    ?assertEqual(ok, attachment_w403_owned_fixture:cleanup(#{})).

cleanup_restores_environment_test() ->
    Before = application:get_all_env(imboy),
    try
        ok = application:set_env(imboy, w403_owned_fixture_regression, changed),
        ok = attachment_w403_owned_fixture:cleanup(#{w403_before => #{env => Before}}),
        ?assertEqual(lists:sort(Before), lists:sort(application:get_all_env(imboy)))
    after
        ok = attachment_w403_owned_fixture:cleanup(#{w403_before => #{env => Before}})
    end.

helper_exports_no_test_or_gateway_entry_test() ->
    Exports = attachment_w403_owned_fixture:module_info(exports),
    ?assertEqual(
        lists:sort([
            {setup, 0},
            {cleanup, 1},
            {assert_fixture_database, 4},
            {assert_chain_fixture_database, 4},
            {module_info, 0},
            {module_info, 1}
        ]),
        lists:sort(Exports)
    ).
