-module(attachment_delete_reaper_tests).
-include_lib("eunit/include/eunit.hrl").

reaper_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun disabled_has_no_db_calls/0,
        fun both_queues_have_independent_timers/0,
        fun db_failure_keeps_periodic_recovery/0,
        fun child_specs_respect_opt_in_and_feature/0,
        fun slow_ordinary_does_not_block_enterprise/0,
        {timeout, 40, fun real_timer_recovers_without_manual_reconcile/0}
    ]}.
setup() ->
    meck:new(attachment_gc_repo, [non_strict, no_link]),
    meck:new(eb_pg_asset_delete_queue, [non_strict, no_link]),
    meck:new(imboy_feature, [non_strict, no_link]),
    meck:expect(imboy_feature, enabled, fun(_) -> true end),
    application:unset_env(imboy, attachment_delete_worker_enabled).
cleanup(_) ->
    [
        case whereis(N) of
            undefined -> ok;
            P -> gen_server:stop(P)
        end
     || N <- [attachment_delete_reaper, enterprise_attachment_delete_reaper]
    ],
    application:unset_env(imboy, attachment_delete_worker_enabled),
    meck:unload().
disabled_has_no_db_calls() ->
    {ok, P} = attachment_delete_reaper:start_link(ordinary),
    ?assertEqual(false, maps:get(enabled, sys:get_state(P))),
    ?assertEqual(0, meck:num_calls(attachment_gc_repo, '_', '_')).
both_queues_have_independent_timers() ->
    application:set_env(imboy, attachment_delete_worker_enabled, true),
    Parent = self(),
    meck:expect(attachment_gc_repo, v5_reap, fun(100) ->
        Parent ! ordinary,
        {ok, 1}
    end),
    meck:expect(eb_pg_asset_delete_queue, reap, fun(100) ->
        Parent ! enterprise,
        {ok, 1}
    end),
    {ok, O} = attachment_delete_reaper:start_link(ordinary),
    {ok, E} = attachment_delete_reaper:start_link(enterprise),
    O ! reap,
    E ! reap,
    receive
        ordinary -> ok
    after 1000 -> error(no_ordinary_reaper)
    end,
    receive
        enterprise -> ok
    after 1000 -> error(no_enterprise_reaper)
    end,
    ?assert(is_reference(maps:get(timer, sys:get_state(O)))),
    ?assert(is_reference(maps:get(timer, sys:get_state(E)))).
db_failure_keeps_periodic_recovery() ->
    application:set_env(imboy, attachment_delete_worker_enabled, true),
    Parent = self(),
    meck:expect(attachment_gc_repo, v5_reap, fun(100) ->
        Parent ! failed,
        {error, db_down}
    end),
    {ok, P} = attachment_delete_reaper:start_link(ordinary),
    P ! reap,
    receive
        failed -> ok
    after 1000 -> error(no_reaper)
    end,
    S = sys:get_state(P),
    ?assert(is_process_alive(P)),
    ?assert(erlang:read_timer(maps:get(timer, S)) > 29000).

child_specs_respect_opt_in_and_feature() ->
    ?assertEqual([], attachment_delete_reaper:child_specs()),
    application:set_env(imboy, attachment_delete_worker_enabled, true),
    ?assertEqual(3, length(attachment_delete_reaper:child_specs())),
    meck:expect(imboy_feature, enabled, fun(_) -> false end),
    ?assertEqual(2, length(attachment_delete_reaper:child_specs())).

real_timer_recovers_without_manual_reconcile() ->
    application:set_env(imboy, attachment_delete_worker_enabled, true),
    Parent = self(),
    meck:expect(
        attachment_gc_repo,
        v5_reap,
        fun(100) ->
            Parent ! timer_recovered,
            {ok, 1}
        end
    ),
    {ok, P} = attachment_delete_reaper:start_link(ordinary),
    Started = erlang:monotonic_time(millisecond),
    receive
        timer_recovered -> ok
    after 32000 -> error(periodic_recovery_missing)
    end,
    ?assert(erlang:monotonic_time(millisecond) - Started >= 29000),
    ?assert(is_process_alive(P)),
    ?assert(is_reference(maps:get(timer, sys:get_state(P)))).

slow_ordinary_does_not_block_enterprise() ->
    application:set_env(imboy, attachment_delete_worker_enabled, true),
    Parent = self(),
    meck:expect(
        attachment_gc_repo,
        v5_reap,
        fun(100) ->
            Parent ! ordinary_started,
            receive
                continue -> {ok, 0}
            end
        end
    ),
    meck:expect(
        eb_pg_asset_delete_queue,
        reap,
        fun(100) ->
            Parent ! enterprise_recovered,
            {ok, 0}
        end
    ),
    {ok, O} = attachment_delete_reaper:start_link(ordinary),
    {ok, E} = attachment_delete_reaper:start_link(enterprise),
    O ! reap,
    receive
        ordinary_started -> ok
    after 1000 -> error(no_ordinary_start)
    end,
    E ! reap,
    try
        receive
            enterprise_recovered -> ok
        after 1000 -> error(queues_share_blocking_loop)
        end
    after
        O ! continue
    end.
