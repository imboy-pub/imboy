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
    empty_db_migrate_idempotent/1,
    exception_cleanup/1
]).

all() ->
    [empty_db_migrate_idempotent, exception_cleanup].

init_per_suite(Config) ->
    eunit_runner:ct_suite_setup(Config).

end_per_suite(Config) ->
    eunit_runner:ct_suite_cleanup(Config).

empty_db_migrate_idempotent(_Config) ->
    {Base, Admin, Owner, NewDb} = test_context("migration"),
    with_test_database(Admin, NewDb, Owner, fun() ->
        SrcDb = maps:get(database, Base),
        Extensions = source_extensions(Admin, SrcDb),
        %% CI 源库合同包含 uuid-ossp；名称中的连字符要求按 PG identifier 引用。
        true = lists:member(<<"uuid-ossp">>, Extensions),
        ok = install_extensions(Admin, NewDb, Extensions),

        %% 切换迁移超管到空库并执行真实迁移。
        %% migrate/0 经 config_ds:env(super_account) 读取 application env；
        %% config_ds:set 写 DB 配置表，不影响本 VM 的 sys.config 值。
        with_super_account(Base#{database => NewDb}, fun() ->
            ok = imboy_migrate:migrate(),
            verify_migration(Base#{database => NewDb})
        end)
    end),
    {comment, "空库全量迁移 + 关键表合同 + 幂等重启 + c2c 写守卫留存"}.

%% 显式验证异常路径：迁移超管必须恢复，已创建的临时数据库必须删除。
exception_cleanup(_Config) ->
    {Base, Admin, Owner, NewDb} = test_context("cleanup"),
    Original = application:get_env(imboy, super_account),
    ?assertEqual("\"uuid-ossp\"", quote_pg_identifier(<<"uuid-ossp">>)),
    ?assertEqual("\"odd\"\"name\"", quote_pg_identifier("odd\"name")),
    ExtensionFailure =
        try
            with_test_database(Admin, NewDb, Owner, fun() ->
                with_connection(Admin#{database => NewDb}, fun(Conn) ->
                    create_extension_if_missing(Conn, <<"ct03-missing-extension">>)
                end)
            end),
            no_exception
        catch
            error:{extension_initialization_failed, <<"ct03-missing-extension">>, _} ->
                extension_init_failed
        end,
    ?assertEqual(extension_init_failed, ExtensionFailure),
    ?assertEqual(false, database_exists(Admin, NewDb)),
    ?assertError(
        ct03_injected_failure,
        with_test_database(Admin, NewDb, Owner, fun() ->
            with_super_account(Base#{database => NewDb}, fun() ->
                error(ct03_injected_failure)
            end)
        end)
    ),
    ?assertEqual(Original, application:get_env(imboy, super_account)),
    ?assertEqual(false, database_exists(Admin, NewDb)),
    {comment, "扩展初始化异常及迁移异常后均清理临时数据库并恢复 super_account"}.

test_context(Tag) ->
    Base = config_ds:env(super_account),
    true = is_map(Base),
    #{database := SrcDb} = Base,
    true = (SrcDb =/= <<>>) andalso (SrcDb =/= []),
    Admin = Base#{database => "postgres"},
    Owner = maps:get(username, Base, "postgres"),
    NewDb =
        "imboy_quality_ct03_" ++ Tag ++ "_" ++ os:getpid() ++ "_" ++
            integer_to_list(erlang:unique_integer([positive, monotonic])),
    true = length(NewDb) =< 63,
    {Base, Admin, Owner, NewDb}.

with_test_database(Admin, Db, Owner, Fun) ->
    %% Cleanup 包住预清理、CREATE DATABASE、扩展初始化和迁移全过程；
    %% 任意一步抛错都尝试回收本用例唯一命名的数据库。
    with_cleanup(
        fun() ->
            prepare_empty_database(Admin, Db, Owner),
            Fun()
        end,
        fun() -> drop_test_database(Admin, Db) end
    ).

prepare_empty_database(Admin, Db, Owner) ->
    with_connection(Admin, fun(Conn) ->
        ok = drop_database(Conn, Db),
        Sql =
            "CREATE DATABASE " ++ quote_pg_identifier(Db) ++
                " OWNER " ++ quote_pg_identifier(Owner),
        {ok, _, _} = epgsql:squery(Conn, Sql),
        ok
    end).

drop_test_database(Admin, Db) ->
    with_connection(Admin, fun(Conn) -> drop_database(Conn, Db) end).

drop_database(Conn, Db) ->
    %% 只操作本套件生成的临时库；FORCE 清理异常中断遗留的库内会话。
    Sql = "DROP DATABASE IF EXISTS " ++ quote_pg_identifier(Db) ++ " WITH (FORCE)",
    {ok, _, _} = epgsql:squery(Conn, Sql),
    ok.

source_extensions(Admin, SrcDb) ->
    with_connection(Admin#{database => SrcDb}, fun(Conn) ->
        {ok, _, Rows} = epgsql:squery(
            Conn,
            "SELECT extname FROM pg_extension WHERE extname <> 'plpgsql' ORDER BY extname"
        ),
        [Name || {Name} <- Rows]
    end).

install_extensions(Admin, Db, Extensions) ->
    with_connection(Admin#{database => Db}, fun(Conn) ->
        lists:foreach(
            fun(Ext) ->
                ok = create_extension_if_missing(Conn, Ext)
            end,
            Extensions
        ),
        {ok, _, Rows} = epgsql:squery(
            Conn,
            "SELECT extname FROM pg_extension WHERE extname <> 'plpgsql' ORDER BY extname"
        ),
        Installed = [Name || {Name} <- Rows],
        Missing = [Ext || Ext <- Extensions, not lists:member(Ext, Installed)],
        [] = Missing,
        true = lists:member(<<"uuid-ossp">>, Installed),
        ok
    end).

create_extension_if_missing(Conn, Ext) ->
    %% extname 是标识符，不是 SQL 关键字；uuid-ossp 等名称必须双引号包裹。
    Sql = "CREATE EXTENSION IF NOT EXISTS " ++ quote_pg_identifier(Ext),
    case epgsql:squery(Conn, Sql) of
        {ok, _, _} -> ok;
        Error -> error({extension_initialization_failed, Ext, Error})
    end.

verify_migration(DbConfig) ->
    with_connection(DbConfig, fun(Conn) ->
        %% 核心 DDL 合同：关键表真实存在
        {ok, _, [{Cnt}]} = epgsql:squery(
            Conn,
            "SELECT count(*) FROM information_schema.tables WHERE table_schema='public'"
            " AND table_name IN ('msg_c2c','channel','workspace_member','msg_store')"
        ),
        ?assertEqual(<<"4">>, Cnt),

        %% 迁移版本表已记录（版本表存在）
        {ok, _, [{VCnt}]} = epgsql:squery(
            Conn,
            "SELECT count(*) FROM information_schema.tables"
            " WHERE table_schema='public' AND table_name LIKE '%migration%'"
        ),
        true = binary_to_integer(VCnt) >= 1,

        %% 幂等：同 schema 再次迁移（重启场景）= ok 且不破坏守卫
        ok = imboy_migrate:migrate(),
        {ok, _, [{GCnt}]} = epgsql:squery(
            Conn, "SELECT count(*) FROM pg_trigger WHERE tgname LIKE '%c2c_order%'"
        ),
        true = binary_to_integer(GCnt) >= 1,
        ok
    end).

with_super_account(Config, Fun) ->
    Old = application:get_env(imboy, super_account),
    with_cleanup(
        fun() ->
            ok = application:set_env(imboy, super_account, Config),
            Fun()
        end,
        fun() -> restore_super_account(Old) end
    ).

with_connection(Args, Fun) ->
    {ok, Conn} = epgsql:connect(Args),
    try
        Fun(Conn)
    after
        %% 连接可能已因 SQL/网络异常断开；关闭失败不能阻止外层数据库清理。
        try
            epgsql:close(Conn)
        catch
            _:_ -> ok
        end
    end.

with_cleanup(Fun, Cleanup) ->
    Outcome =
        try
            {ok, Fun()}
        catch
            Class:Reason:Stack -> {raised, Class, Reason, Stack}
        end,
    CleanupOutcome =
        try
            case Cleanup() of
                ok -> ok;
                Other -> {error, {unexpected_cleanup_result, Other}}
            end
        catch
            CleanupClass:CleanupReason:CleanupStack ->
                {error, {CleanupClass, CleanupReason, CleanupStack}}
        end,
    case {Outcome, CleanupOutcome} of
        {{ok, Value}, ok} ->
            Value;
        {{ok, _Value}, {error, CleanupError}} ->
            error({cleanup_failed, CleanupError});
        {{raised, RaisedClass, RaisedReason, RaisedStack}, ok} ->
            erlang:raise(RaisedClass, RaisedReason, RaisedStack);
        {{raised, RaisedClass, RaisedReason, RaisedStack}, {error, CleanupError}} ->
            erlang:raise(
                RaisedClass,
                {RaisedReason, {cleanup_failed, CleanupError}},
                RaisedStack
            )
    end.

database_exists(Admin, Db) ->
    with_connection(Admin, fun(Conn) ->
        %% Db 仅由 test_context/1 生成（固定前缀、标签、PID 和整数）。
        Sql = "SELECT 1 FROM pg_database WHERE datname = '" ++ Db ++ "'",
        {ok, _, Rows} = epgsql:squery(Conn, Sql),
        Rows =/= []
    end).

quote_pg_identifier(Value) when is_binary(Value) ->
    quote_pg_identifier(binary_to_list(Value));
quote_pg_identifier(Value) when is_atom(Value) ->
    quote_pg_identifier(atom_to_list(Value));
quote_pg_identifier(Value) when is_list(Value) ->
    [$" |
        lists:flatmap(
            fun
                ($") -> [$", $"];
                (Char) -> [Char]
            end,
            Value
        )] ++ [$"].

restore_super_account(undefined) ->
    application:unset_env(imboy, super_account);
restore_super_account({ok, Val}) ->
    ok = application:set_env(imboy, super_account, Val).
