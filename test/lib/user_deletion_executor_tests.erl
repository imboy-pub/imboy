-module(user_deletion_executor_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% 账号删除执行器（user_deletion_executor）真 PostgreSQL 测试。
%%%
%%% 覆盖（W1-D9）：
%%%   * 处置清单中**不存在**的表（42P01 undefined_table）必须按
%%%     「零行已处置」成功，并以 WARN 审计事件留痕（不静默）——
%%%     背景：delete_spec 含 doc_draft_sections（data-disposition.yml 的
%%%     D-02 delete 决策），但全仓考古（git log --all -S）无任何建表
%%%     迁移、无 schema 依据；旧实现在此表处 {delete_failed,...} 崩溃，
%%%     令整个删除事务失败（user_deletion_orchestrator_tests 3 例红）。
%%%   * 修复不得波及正常删除路径：存在的表照删。
%%%
%%% ORG-02 按契约演进（Core Contract C17 + 计划 §1.6；任务卡 ORG-02
%%% 授权 adapter 修改下的测试演进）：execute_main_tx 首语句为 deletion
%%% preflight 门（fail-closed 编排接入）。故：
%%%   * 原 absent-table 例补挂五域全 stub 注册表（registry 属测试配置，
%%%     不触碰生产代码、不给未实现域默认空 blocker）——保住该例的
%%%     「正常删除路径 + WARN 留痕」回归价值；
%%%   * 新增三例：blockers 拒（无部分删除）/ facts 不可得拒（稳定原因
%%%     DEPENDENCY_FACTS_UNAVAILABLE）/ preflight 后删除前并发注入
%%%     org owner 变更由 DB RESTRICT 兜底（C17 TOCTOU 口径）。
%%%
%%% 运行：make eunit-local EUNIT_CONFIG=config/sys.local.eb.config t=user_deletion_executor_tests

absent_table_is_zero_rows_disposed_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        Uid = new_uid(),
        Peer = new_uid(),
        MsgId = new_uid(),
        Self = self(),
        with_full_registry(fun() ->
            %% 拦截 elib_log 的 WARN 留痕事件（passthrough 转发，不破坏其余日志）
            ok = meck:new(elib_log, [passthrough, no_link]),
            ok = meck:expect(elib_log, internal_log, fun(Level, Msg, M, L) ->
                maybe_report_absent(Self, Msg),
                meck:passthrough([Level, Msg, M, L])
            end),
            ok = meck:expect(elib_log, internal_log, fun(Level, Fmt, Args, M, L) ->
                maybe_report_absent(Self, Fmt),
                meck:passthrough([Level, Fmt, Args, M, L])
            end),
            try
                %% 种子：真实用户 + 真实个人消息（验证「存在的表照删」）
                ok = create_user(Uid),
                ok = create_user(Peer),
                {ok, _} = elib_pg:query(
                    <<
                        "INSERT INTO public.msg_c2c (id, from_id, to_id, msg_id, msg_type, payload)"
                        " VALUES ($1, $2, $3, 'm-absent-probe', 't', 'p')"
                    >>,
                    [MsgId, Uid, Peer]
                ),
                %% 主删除事务：清单含不存在的表（doc_draft_sections）。
                %% 旧实现：{delete_failed,<<"doc_draft_sections">>,{error,...42P01...}}
                %% 崩溃 → 事务整体回滚 → 消息/用户行残留（基线 3 例红的根因）。
                case
                    elib_pg:with_tx(fun(Conn) ->
                        user_deletion_executor:execute_main_tx(Conn, Uid)
                    end)
                of
                    ok ->
                        ok;
                    {error, Reason} ->
                        erlang:error({execute_main_tx_failed, Reason})
                end,
                %% ① 存在的表照删
                {ok, []} = elib_pg:query(
                    <<"SELECT id FROM public.msg_c2c WHERE id = $1">>, [MsgId]
                ),
                %% ② 不存在的表按「零行已处置」成功，且必须留下 WARN 审计事件
                %%    （不静默）：[user_deletion_absent_table, Uid, Table, zero_rows_disposed]
                receive_absent_event(Self, <<"doc_draft_sections">>)
            after
                _ = (catch meck:unload(elib_log)),
                cleanup_user(Uid),
                cleanup_user(Peer)
            end
        end)
    end).

%% ORG-02（C17）：preflight blockers 非空 → 整事务拒绝、零部分删除。
preflight_blockers_abort_no_partial_deletion_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        Uid = new_uid(),
        Peer = new_uid(),
        MsgId = new_uid(),
        with_full_registry(blockers, fun() ->
            try
                ok = create_user(Uid),
                ok = create_user(Peer),
                {ok, _} = elib_pg:query(
                    <<
                        "INSERT INTO public.msg_c2c (id, from_id, to_id, msg_id, msg_type, payload)"
                        " VALUES ($1, $2, $3, 'm-blocker-probe', 't', 'p')"
                    >>,
                    [MsgId, Uid, Peer]
                ),
                {error, {deletion_preflight_blockers, [Blocker]}} =
                    elib_pg:with_tx(fun(Conn) ->
                        user_deletion_executor:execute_main_tx(Conn, Uid)
                    end),
                %% 稳定 blocker code（计划 §1.6 冻结六码之一）
                ?assertEqual(<<"ORG_OWNER_ACTIVE">>, maps:get(code, Blocker)),
                %% 资源/账号完好：用户行、消息行俱在（无部分删除）
                {ok, [_]} = elib_pg:query(
                    <<"SELECT id FROM public.\"user\" WHERE id = $1">>, [Uid]
                ),
                {ok, [_]} = elib_pg:query(
                    <<"SELECT id FROM public.msg_c2c WHERE id = $1">>, [MsgId]
                )
            after
                cleanup_user(Uid),
                cleanup_user(Peer)
            end
        end)
    end).

%% ORG-02（C17/§1.6）：任一域 facts 不可得 → 稳定原因
%% DEPENDENCY_FACTS_UNAVAILABLE，拒绝且零部分删除。
preflight_unavailable_abort_no_partial_deletion_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        Uid = new_uid(),
        with_full_registry(unavailable, fun() ->
            try
                ok = create_user(Uid),
                {error, {deletion_preflight_unavailable, Detail}} =
                    elib_pg:with_tx(fun(Conn) ->
                        user_deletion_executor:execute_main_tx(Conn, Uid)
                    end),
                ?assertEqual(
                    <<"DEPENDENCY_FACTS_UNAVAILABLE">>, maps:get(code, Detail)
                ),
                {ok, [_]} = elib_pg:query(
                    <<"SELECT id FROM public.\"user\" WHERE id = $1">>, [Uid]
                )
            after
                cleanup_user(Uid)
            end
        end)
    end).

%% ORG-02（C17）：preflight 是解释性证据非 TOCTOU 保障——
%% 「preflight 后、删除前」并发注入 org owner 变更（stub provider 在
%% 编排期间经独立连接提交）→ 删除事务最终被
%% fk_organization_owner（ON DELETE RESTRICT，00000126）稳定拒绝，
%% 事务整体回滚、无部分删除。
concurrent_change_after_preflight_rejected_by_db_restrict_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        Uid = new_uid(),
        OrgId = new_uid(),
        MsgId = new_uid(),
        Peer = new_uid(),
        with_full_registry(agent_inserts_org_owner, fun() ->
            ok = application:set_env(imboy, preflight_stub_org_id, OrgId),
            try
                ok = create_user(Uid),
                ok = create_user(Peer),
                {ok, _} = elib_pg:query(
                    <<
                        "INSERT INTO public.msg_c2c (id, from_id, to_id, msg_id, msg_type, payload)"
                        " VALUES ($1, $2, $3, 'm-toctou-probe', 't', 'p')"
                    >>,
                    [MsgId, Uid, Peer]
                ),
                %% facts 全绿（stub 撒谎/竞态窗口），但并发注入的 owner
                %% 行令用户主行删除撞 RESTRICT → 事务回滚
                {error, _Reason} =
                    elib_pg:with_tx(fun(Conn) ->
                        user_deletion_executor:execute_main_tx(Conn, Uid)
                    end),
                {ok, [_]} = elib_pg:query(
                    <<"SELECT id FROM public.\"user\" WHERE id = $1">>, [Uid]
                ),
                {ok, [_]} = elib_pg:query(
                    <<"SELECT id FROM public.msg_c2c WHERE id = $1">>, [MsgId]
                ),
                {ok, [#{<<"owner_id">> := Uid}]} = elib_pg:query(
                    <<"SELECT owner_id FROM public.organization WHERE id = $1">>, [OrgId]
                )
            after
                application:unset_env(imboy, preflight_stub_org_id),
                _ = elib_pg:query(
                    <<"DELETE FROM public.organization WHERE id = $1">>, [OrgId]
                ),
                cleanup_user(Uid),
                cleanup_user(Peer)
            end
        end)
    end).

%% ===================================================================
%% Internal
%% ===================================================================

%% 五域全 stub 注册表（测试配置）：registry 覆盖只换 provider 实现，
%% 不给未实现域默认空 blocker——required 域集合仍由编排器强制。
with_full_registry(Behavior, Fun) ->
    ok = application:set_env(
        imboy,
        deletion_preflight_providers,
        organization_preflight_stub_providers:full_registry()
    ),
    ok = application:set_env(imboy, preflight_stub_behavior, Behavior),
    try
        Fun()
    after
        application:unset_env(imboy, deletion_preflight_providers),
        application:unset_env(imboy, preflight_stub_behavior)
    end.

with_full_registry(Fun) ->
    with_full_registry(ok_all, Fun).

%% 结构化 WARN 事件：[user_deletion_absent_table, Uid, Table, zero_rows_disposed]
maybe_report_absent(Self, [user_deletion_absent_table, _Uid, Table, zero_rows_disposed]) ->
    Self ! {absent_event, Table};
maybe_report_absent(_Self, _Other) ->
    ok.

new_uid() ->
    erlang:system_time(millisecond) * 1000 + rand:uniform(999).

create_user(Uid) ->
    {ok, _} = elib_pg:query(
        <<
            "INSERT INTO public.\"user\" (id, account, password, reg_ip, reg_cosv)"
            " VALUES ($1, $2, 'x', '127.0.0.1', '')"
        >>,
        [Uid, <<"u", (integer_to_binary(Uid))/binary>>]
    ),
    ok.

cleanup_user(Uid) ->
    lists:foreach(fun(Sql) -> _ = elib_pg:query(Sql, [Uid]) end, [
        <<"DELETE FROM public.user_deletion_job WHERE user_id = $1">>,
        <<"DELETE FROM public.user_deletion_request WHERE user_id = $1">>,
        <<"DELETE FROM public.msg_c2c WHERE from_id = $1 OR to_id = $1">>,
        <<"DELETE FROM public.\"user\" WHERE id = $1">>
    ]).

receive_absent_event(Self, Table) ->
    receive
        {absent_event, Table} ->
            ok;
        {absent_event, _Other} ->
            receive_absent_event(Self, Table)
    after 5000 ->
        erlang:error({absent_event_not_logged, Table})
    end.
