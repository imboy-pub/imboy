-module(app_upgrade_log_handler_tests).

%%%===================================================================
%%% @doc F16：异步入库失败必须落错误日志。
%%%
%%% elib_async:async/1 是裸 spawn，返回值无人消费——此前 CHECK 违约的
%%% {error, _} 在这里静默消失，升级漏斗自 prompted 之后全部丢失
%%% （2026-10-20 真机升级联调实证）。这组用例钉住“失败必须可观测”。
%%% @end
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").

insert_logged_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun ok_insert_writes_no_error_log/0,
        fun failed_insert_writes_error_log_with_context/0
    ]}.

setup() ->
    meck:new(app_upgrade_log_logic, [no_link, passthrough]),
    meck:new(elib_log, [no_link, passthrough]),
    meck:expect(elib_log, internal_log, fun(_Level, _Msg, _Module, _Line) -> ok end),
    ok.

cleanup(_) ->
    unload(app_upgrade_log_logic),
    unload(elib_log),
    ok.

unload(Mod) ->
    try
        meck:unload(Mod)
    catch
        _:_ -> ok
    end.

data() ->
    #{
        <<"did">> => <<"d1">>,
        <<"uid">> => 1,
        <<"cos">> => <<"android">>,
        <<"client_vsn">> => <<"1.0.0-alpha.17">>,
        <<"target_vsn">> => <<"1.0.0-alpha.18">>,
        <<"event">> => <<"download_done">>,
        <<"upgrade_type">> => <<>>,
        <<"extra">> => <<"{}">>
    }.

ok_insert_writes_no_error_log() ->
    meck:expect(app_upgrade_log_logic, insert, fun(_) -> {ok, 1} end),
    ?assertEqual(ok, app_upgrade_log_handler:insert_logged(data())),
    ?assertEqual(0, meck:num_calls(elib_log, internal_log, ['_', '_', '_', '_'])).

failed_insert_writes_error_log_with_context() ->
    Reason = {check_violation, <<"chk_app_upgrade_log_upgrade_type">>},
    meck:expect(app_upgrade_log_logic, insert, fun(_) -> {error, Reason} end),
    ?assertEqual(ok, app_upgrade_log_handler:insert_logged(data())),
    ?assertEqual(
        1, meck:num_calls(elib_log, internal_log, [error, '_', app_upgrade_log_handler, '_'])
    ),
    %% 日志里必须带上失败原因和定位字段，不能只有一句“失败”
    [{_, {elib_log, internal_log, [error, Msg, _, _]}, ok}] = meck:history(elib_log),
    [app_upgrade_log_insert_failed, Reason, Context] = Msg,
    ?assertEqual(<<"download_done">>, maps:get(<<"event">>, Context)),
    ?assertEqual(<<>>, maps:get(<<"upgrade_type">>, Context)),
    ?assertNot(maps:is_key(<<"extra">>, Context)).
