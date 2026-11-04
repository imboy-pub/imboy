-module(imboy_quality_ct03_migration_SUITE).

%%% IMBOY_ERLANG_QUALITY P2-09 CT03：空库迁移、已有 schema 重启/幂等。
%%% 真实产品合同（冻结自 src/lib/imboy_migrate.erl migrate/1）：
%%%   空库上 erlang_migrate:up(strict) 应用全部 priv/migrations 并通过
%%%   verify_c2c_order_guards；同 schema 再次迁移 = 幂等 no-op（重启场景）；
%%%   核心表（msg_c2c/channel/workspace_member/msg_store）与迁移版本表真实存在。
%%% 空库为本任务私有 PG 集群内新建，用后即删；无模板替代核心断言。

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    empty_db_migrate_idempotent/1
]).

all() ->
    [empty_db_migrate_idempotent].

init_per_suite(Config) ->
    eunit_runner:ct_suite_setup(Config).

end_per_suite(Config) ->
    eunit_runner:ct_suite_cleanup(Config).

empty_db_migrate_idempotent(_Config) ->
    Base = config_ds:env(super_account),
    true = is_map(Base),
    #{database := SrcDb} = Base,
    true = (SrcDb =/= <<>>) andalso (SrcDb =/= []),
    %% config 值为 string 形态（sys.config 文本），统一按 list 处理
    Pid = os:getpid(),
    NewDb = "imboy_quality_ct03_" ++ Pid,
    Owner = maps:get(username, Base, "postgres"),
    Admin = Base#{database => "postgres"},

    %% 预清理 + 建空库（非模板；表/数据须从零迁移应用）。
    %% 扩展是集群前置（EUNIT_120S §6 readiness 合同）：与源库同清单安装，
    %% 不属于迁移义务本身。
    {ok, A1} = epgsql:connect(Admin),
    try
        {ok, _, _} = epgsql:squery(A1, "DROP DATABASE IF EXISTS " ++ NewDb),
        {ok, _, _} = epgsql:squery(A1, "CREATE DATABASE " ++ NewDb ++ " OWNER " ++ Owner)
    after
        epgsql:close(A1)
    end,
    {ok, A0} = epgsql:connect(Admin),
    Extensions =
        try
            %% 扩展是 database 级（pg_extension 各库独立）：必须连源库读取
            SrcDb2 = maps:get(database, Base),
            {ok, ASrc} = epgsql:connect(Admin#{database => SrcDb2}),
            ExRows =
                try
                    {ok, _, R} = epgsql:squery(
                        ASrc, "SELECT extname FROM pg_extension WHERE extname <> 'plpgsql'"
                    ),
                    R
                after
                    epgsql:close(ASrc)
                end,
            [binary_to_list(N) || {N} <- ExRows]
        after
            epgsql:close(A0)
        end,
    {ok, A2} = epgsql:connect(Admin#{database => NewDb}),
    try
        lists:foreach(
            fun(Ext) ->
                {ok, _, _} = epgsql:squery(A2, "CREATE EXTENSION IF NOT EXISTS " ++ Ext)
            end,
            Extensions
        )
    after
        epgsql:close(A2)
    end,

    %% 切换迁移超管到空库并执行真实迁移。
    %% migrate/0 经 config_ds:env(super_account) 读 application env
    %% （config_ds:set 写 DB 配置表，不影响本 VM 的 sys.config 值）。
    Old = application:get_env(imboy, super_account),
    ok = application:set_env(imboy, super_account, Base#{database => NewDb}),
    try
        %% 空库全量迁移（内含 verify_c2c_order_guards，守卫缺失即失败）
        ok = imboy_migrate:migrate(),

        {ok, C} = epgsql:connect(Base#{database => NewDb}),
        try
            %% 核心 DDL/数据合同：关键表真实存在
            {ok, _, [{Cnt}]} = epgsql:squery(
                C,
                "SELECT count(*) FROM information_schema.tables WHERE table_schema='public'"
                " AND table_name IN ('msg_c2c','channel','workspace_member','msg_store')"
            ),
            ?assertEqual(<<"4">>, Cnt),

            %% 迁移版本表已记录（版本表存在）
            {ok, _, [{VCnt}]} = epgsql:squery(
                C,
                "SELECT count(*) FROM information_schema.tables"
                " WHERE table_schema='public' AND table_name LIKE '%migration%'"
            ),
            true = binary_to_integer(VCnt) >= 1,

            %% 幂等：同 schema 再次迁移（重启场景）= ok 且不破坏守卫
            ok = imboy_migrate:migrate(),
            {ok, _, [{GCnt}]} = epgsql:squery(
                C, "SELECT count(*) FROM pg_trigger WHERE tgname LIKE '%c2c_order%'"
            ),
            true = binary_to_integer(GCnt) >= 1
        after
            epgsql:close(C)
        end
    after
        %% 恢复原连接并删除空库
        restore_super_account(Old),
        {ok, A3} = epgsql:connect(Admin),
        try
            {ok, _, _} = epgsql:squery(A3, "DROP DATABASE IF EXISTS " ++ NewDb)
        after
            epgsql:close(A3)
        end
    end,
    {comment, "空库全量迁移 + 关键表合同 + 幂等重启 + c2c 写守卫留存"}.

restore_super_account(undefined) ->
    application:unset_env(imboy, super_account);
restore_super_account({ok, Val}) ->
    ok = application:set_env(imboy, super_account, Val).
