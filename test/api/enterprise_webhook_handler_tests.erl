%% enterprise_webhook_handler_tests
%% CP-CON-02 — INT-23 GET /api/internal/v1/webhook/deliveries 的旧 offset
%% 参数 versioned 400（DEC-INT23-COMPAT）HTTP 层钉死：
%%   * query 出现 page / size（任一）→ 400，信封 {"error":{"code":
%%     "cursor_required_v1","message":...}}（同 internal 错误信封形态；
%%     code 不在 enterprise_internal_error 的 13 个 stable 码内——versioned
%%     迁移错误是本读面专属，不新造 stable 码）；
%%   * 参数门在进事务（elib_pg:with_tx）之前：无 page/size 时不受本门影响；
%%   * logic 返回 {error, {<<"cursor_required_v1">>, _}} 时 handler 同样映射
%%     400 versioned 信封（双保险：门在 handler，logic 亦守合同）。
%%
%% 纯单元：meck cowboy_req + meck elib_pg:with_tx（不触 DB；200 页形态由
%% enterprise_msg_asset_webhook_pg_tests / wiring HTTP 套件承担）。
-module(enterprise_webhook_handler_tests).

-include_lib("eunit/include/eunit.hrl").

-define(VERSIONED_CODE, <<"cursor_required_v1">>).

%%%===================================================================
%%% Fixture
%%%===================================================================

setup() ->
    ok = meck:new(cowboy_req, [no_link]),
    ok = meck:new(elib_pg, [no_passthrough_cover]),
    ok.

teardown(_) ->
    lists:foreach(
        fun(Mod) ->
            try
                meck:unload(Mod)
            catch
                _:_ -> ok
            end
        end,
        [cowboy_req, elib_pg]
    ),
    ok.

%% cowboy_req 桩（enterprise_msg_asset_webhook_pg_tests 同款收口：reply 捕获
%% 到进程字典）。deliveries 是 GET，不走 idempotency/body 路径。
mock_cowboy(Qs) ->
    ok = meck:expect(cowboy_req, method, 1, fun(_R) -> <<"GET">> end),
    ok = meck:expect(
        cowboy_req, path, 1, fun(_R) -> <<"/api/internal/v1/webhook/deliveries">> end
    ),
    ok = meck:expect(cowboy_req, parse_qs, 1, fun(_R) -> Qs end),
    ok =
        meck:expect(cowboy_req, reply, 4, fun(Status, _H, Body, _R) ->
            put(cpcon02_reply, {Status, Body}),
            req
        end),
    erase(cpcon02_reply),
    ok.

run_deliveries() ->
    {ok, _Req, _State} =
        enterprise_webhook_handler:init(req, #{
            action => deliveries, enterprise_internal => #{}
        }),
    case get(cpcon02_reply) of
        {Status, Body} -> {Status, jsone:decode(Body)};
        undefined -> erlang:error(no_reply_captured)
    end.

%%%===================================================================
%%% 用例
%%%===================================================================

page_param_returns_versioned_400_test_() ->
    {setup, fun setup/0, fun teardown/1, fun() ->
        mock_cowboy([{<<"page">>, <<"2">>}]),
        {400, Decoded} = run_deliveries(),
        ?assertEqual(
            ?VERSIONED_CODE,
            maps:get(<<"code">>, maps:get(<<"error">>, Decoded))
        ),
        ?assert(is_binary(maps:get(<<"message">>, maps:get(<<"error">>, Decoded)))),
        %% 参数门在事务之前：page 出现时不得进 with_tx
        ?assertEqual(0, meck:num_calls(elib_pg, with_tx, 1))
    end}.

size_param_returns_versioned_400_test_() ->
    {setup, fun setup/0, fun teardown/1, fun() ->
        mock_cowboy([{<<"size">>, <<"20">>}]),
        {400, Decoded} = run_deliveries(),
        ?assertEqual(
            ?VERSIONED_CODE,
            maps:get(<<"code">>, maps:get(<<"error">>, Decoded))
        ),
        ?assertEqual(0, meck:num_calls(elib_pg, with_tx, 1))
    end}.

page_and_size_together_return_versioned_400_test_() ->
    {setup, fun setup/0, fun teardown/1, fun() ->
        mock_cowboy([{<<"page">>, <<"1">>}, {<<"size">>, <<"10">>}]),
        {400, Decoded} = run_deliveries(),
        ?assertEqual(
            ?VERSIONED_CODE,
            maps:get(<<"code">>, maps:get(<<"error">>, Decoded))
        ),
        ?assertEqual(0, meck:num_calls(elib_pg, with_tx, 1))
    end}.

legacy_params_alongside_new_ones_still_rejected_test_() ->
    {setup, fun setup/0, fun teardown/1, fun() ->
        %% page 与 cursor 混用：同样拒——不存在「带着 page 用游标」的过渡形态
        mock_cowboy([{<<"page">>, <<"3">>}, {<<"cursor">>, <<"x.y">>}]),
        {400, Decoded} = run_deliveries(),
        ?assertEqual(
            ?VERSIONED_CODE,
            maps:get(<<"code">>, maps:get(<<"error">>, Decoded))
        ),
        ?assertEqual(0, meck:num_calls(elib_pg, with_tx, 1))
    end}.

no_legacy_params_passes_gate_test_() ->
    {setup, fun setup/0, fun teardown/1, fun() ->
        mock_cowboy([{<<"page_size">>, <<"5">>}]),
        %% 门放行：进入事务（桩掉 with_tx 返回内部错误，证明流程越过参数门）
        meck:expect(elib_pg, with_tx, fun(_F) -> {error, gate_probe} end),
        {500, Decoded} = run_deliveries(),
        ?assertEqual(
            <<"internal_error">>,
            maps:get(<<"code">>, maps:get(<<"error">>, Decoded))
        ),
        ?assertEqual(1, meck:num_calls(elib_pg, with_tx, 1))
    end}.

logic_versioned_error_maps_to_400_test_() ->
    {setup, fun setup/0, fun teardown/1, fun() ->
        mock_cowboy([]),
        %% 双保险：logic 层返回 cursor_required_v1 时 handler 映射 400 信封
        meck:expect(
            elib_pg,
            with_tx,
            fun(_F) -> {rollback, {business_error, ?VERSIONED_CODE}} end
        ),
        {400, Decoded} = run_deliveries(),
        ?assertEqual(
            ?VERSIONED_CODE,
            maps:get(<<"code">>, maps:get(<<"error">>, Decoded))
        )
    end}.
