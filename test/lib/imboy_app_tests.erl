-module(imboy_app_tests).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    Previous = application:get_env(imboy, auto_migrate),
    meck:new(imboy_migrate, [passthrough, no_link]),
    Previous.

cleanup(Previous) ->
    meck:unload(imboy_migrate),
    case Previous of
        undefined -> application:unset_env(imboy, auto_migrate);
        {ok, Value} -> application:set_env(imboy, auto_migrate, Value)
    end,
    ok.

auto_migrate_gate_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun enabled_runs_migration/0,
        fun disabled_skips_migration/0,
        fun invalid_config_fails_fast/0
    ]}.

enabled_runs_migration() ->
    application:set_env(imboy, auto_migrate, true),
    meck:expect(imboy_migrate, migrate, fun() -> ok end),
    ?assertEqual(ok, imboy_app:maybe_migrate()),
    ?assertEqual(1, meck:num_calls(imboy_migrate, migrate, [])).

disabled_skips_migration() ->
    application:set_env(imboy, auto_migrate, false),
    meck:expect(imboy_migrate, migrate, fun() -> ok end),
    ?assertEqual(ok, imboy_app:maybe_migrate()),
    ?assertEqual(0, meck:num_calls(imboy_migrate, migrate, [])).

invalid_config_fails_fast() ->
    application:set_env(imboy, auto_migrate, enabled),
    ?assertError(
        {invalid_config, auto_migrate, enabled},
        imboy_app:maybe_migrate()
    ).
