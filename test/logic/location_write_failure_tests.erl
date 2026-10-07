-module(location_write_failure_tests).
-include_lib("eunit/include/eunit.hrl").

location_write_failure_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun(_) -> ?_test(failed_save_does_not_report_visible()) end,
        fun(_) -> ?_test(failed_delete_does_not_report_hidden()) end,
        fun(_) -> ?_test(hidden_endpoint_reports_delete_failure()) end,
        fun(_) -> ?_test(successful_writes_update_visibility()) end
    ]}.

setup() ->
    Modules = [user_setting_ds, geo_people_nearby_ds, imboy_plugin_registry, elib_response],
    ok = meck:new(Modules, [non_strict, no_link]),
    ok = meck:expect(user_setting_ds, save, fun(_, _, _) -> ok end),
    ok = meck:expect(imboy_plugin_registry, required_feature, fun(_, _, _) -> undefined end),
    ok = meck:expect(elib_response, error, fun(Req, _) -> Req#{response_status => 400} end),
    ok = meck:expect(elib_response, success, fun(Req, _, _) -> Req#{response_status => 200} end),
    Modules.

cleanup(Modules) ->
    meck:unload(Modules).

failed_save_does_not_report_visible() ->
    ok = meck:expect(geo_people_nearby_ds, save, fun(_, _, _) -> {error, db_unavailable} end),
    ?assertMatch({error, _}, location_logic:make_myself_visible(100, <<"23">>, <<"113">>)),
    ?assertEqual(0, meck:num_calls(user_setting_ds, save, ['_', '_', '_'])).

failed_delete_does_not_report_hidden() ->
    ok = meck:expect(geo_people_nearby_ds, delete, fun(_) -> {error, db_unavailable} end),
    ?assertMatch({error, _}, location_logic:make_myself_unvisible(100)),
    ?assertEqual(0, meck:num_calls(user_setting_ds, save, ['_', '_', '_'])).

hidden_endpoint_reports_delete_failure() ->
    ok = meck:expect(geo_people_nearby_ds, delete, fun(_) -> {error, db_unavailable} end),
    {ok, Req, _} = location_handler:init(#{}, #{
        action => make_myself_unvisible, current_uid => 100
    }),
    ?assertEqual(400, maps:get(response_status, Req)),
    ?assertEqual(0, meck:num_calls(elib_response, success, ['_', '_', '_'])).

successful_writes_update_visibility() ->
    ok = meck:expect(geo_people_nearby_ds, save, fun(_, _, _) -> {ok, 1} end),
    ok = meck:expect(geo_people_nearby_ds, delete, fun(_) -> {ok, 0} end),
    ?assertEqual(ok, location_logic:make_myself_visible(100, <<"23">>, <<"113">>)),
    ?assertEqual(ok, location_logic:make_myself_unvisible(100)),
    ?assertEqual(
        1, meck:num_calls(user_setting_ds, save, [100, <<"people_nearby_visible">>, true])
    ),
    ?assertEqual(
        1, meck:num_calls(user_setting_ds, save, [100, <<"people_nearby_visible">>, false])
    ).
