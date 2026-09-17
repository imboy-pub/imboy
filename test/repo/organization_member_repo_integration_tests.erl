-module(organization_member_repo_integration_tests).

-include_lib("eunit/include/eunit.hrl").

%% Owner 转移真库集成套件（ORG-01 持久化面 + 单事务转移语义）。
%%
%% 库供给契约（废除 127.0.0.1:4323/moya_zcode_* 硬编码，2026-09-17）：
%%   * 目标服务器解析优先级：环境变量 > eunit VM 启动配置
%%     （`-config config/sys.local` 装载的 application:get_env(imboy, pg_conf)
%%     内的 epgsql connect 参数）。源码不内置任何主机/端口/库名/口令。
%%   * 每次运行建**一次性 marker 库** org_member_inttest_<us>_<rand>：
%%     建库 → erlang_migrate:up 全链（strict，与生产/演练同口径，含
%%     00000126/127 的 single-active-owner 索引与 DEFERRABLE invariant）
%%     → 测试体 BEGIN/ROLLBACK → cleanup DROP DATABASE。
%%   * 不再在事务内重放 00000113：其 up 会把 127 已 DEFERRABLE 化的
%%     trg_organization_primary_owner_member_guard 换回旧即时版，使
%%     「先降旧 owner」一步被即时 guard 误拒（500），与生产行为不符。
%%   * 无 skip 分支：环境/配置/迁移任一不可用都必须显式 FAIL（带 env 变量名
%%     或失败步骤），禁止静默 skip 伪装绿。
%%
%% 约束时机语义（与生产 elib_pg:with_tx 对齐）：seed 段保持
%% SET CONSTRAINTS ALL IMMEDIATE（让 owner guard 的拒绝在语句点确定发生）；
%% 两个 transfer 断言前显式切回 SET CONSTRAINTS ALL DEFERRED——生产转移
%% 依赖的正是「提交时才裁决」的 deferred invariant。

-define(ENV_HOST, "ORG_MEMBER_INTTEST_PG_HOST").
-define(ENV_PORT, "ORG_MEMBER_INTTEST_PG_PORT").
-define(ENV_USER, "ORG_MEMBER_INTTEST_PG_USER").
-define(ENV_PASS, "ORG_MEMBER_INTTEST_PG_PASSWORD").
-define(ENV_MAINT_DB, "ORG_MEMBER_INTTEST_MAINT_DB").

-define(OWNER, 99113001).
-define(MEMBER, 99113002).
-define(ORG, 99113101).
-define(WS, 99113201).
-define(GROUP, 99113301).

organization_member_persistence_test_() ->
    {timeout, 900,
        {setup, fun setup_conn/0, fun close_conn/1, fun(State) ->
            ?_test(with_rollback(maps:get(conn, State), fun verify_persistence/1))
        end}}.

setup_conn() ->
    {ok, _} = application:ensure_all_started(epgsql),
    _ = code:add_patha("deps/erlang_migrate/ebin"),
    Server = resolve_server(),
    MaintConn = connect_server(Server, maint_db()),
    DbName = marker_db_name(),
    try
        ok = create_db(MaintConn, DbName),
        ok = ensure_extensions(Server, DbName),
        ok = migrate_result(migrate_fresh_db(Server, DbName)),
        {ok, Conn} = epgsql:connect(conn_opts(Server, DbName)),
        #{conn => Conn, maint_conn => MaintConn, db => DbName, server => Server}
    catch
        Class:Reason:Stack ->
            close_quiet(MaintConn),
            try_drop_db(Server, DbName),
            erlang:raise(Class, {org_member_inttest_setup_failed, Reason}, Stack)
    end.

close_conn(#{conn := Conn, maint_conn := MaintConn, db := DbName, server := Server}) ->
    close_quiet(Conn),
    close_quiet(MaintConn),
    try_drop_db(Server, DbName),
    ok.

close_quiet(Conn) ->
    try
        epgsql:close(Conn)
    catch
        _:_ -> ok
    end.

%% 目标服务器：环境变量逐项覆盖 config（imboy.pg_conf 的 epgsql 连接段）。
resolve_server() ->
    Defaults = config_pg_conn(),
    User = env_or(?ENV_USER, maps:get(username, Defaults, undefined)),
    Pass = env_or(?ENV_PASS, maps:get(password, Defaults, undefined)),
    case {User, Pass} of
        {undefined, _} ->
            erlang:error({org_member_inttest_missing_pg_user, ?ENV_USER});
        {_, undefined} ->
            erlang:error({org_member_inttest_missing_pg_password, ?ENV_PASS});
        _ ->
            ok
    end,
    #{
        host => env_or(?ENV_HOST, maps:get(host, Defaults, "127.0.0.1")),
        port => normalize_port(env_or(?ENV_PORT, maps:get(port, Defaults, 5432))),
        username => User,
        password => Pass
    }.

config_pg_conn() ->
    %% -config 注入的应用环境在应用 load 时才合并；本套件不启动 imboy 应用，
    %% 故先 load（零进程副作用）再取 imboy.pg_conf。
    case application:load(imboy) of
        ok -> ok;
        {error, {already_loaded, imboy}} -> ok;
        {error, LoadReason} -> erlang:error({org_member_inttest_app_load_failed, LoadReason})
    end,
    case application:get_env(imboy, pg_conf) of
        {ok, #{start_mfa := {epgsql, connect, [Opts]}}} when is_map(Opts) ->
            Opts;
        {ok, Other} ->
            erlang:error({org_member_inttest_pg_conf_shape, Other});
        undefined ->
            erlang:error({org_member_inttest_pg_conf_missing, 'imboy.pg_conf'})
    end.

env_or(Key, Default) ->
    case os:getenv(Key) of
        false -> Default;
        "" -> Default;
        Value -> Value
    end.

normalize_port(Port) when is_integer(Port) -> Port;
normalize_port(Str) when is_list(Str) -> list_to_integer(Str).

maint_db() ->
    case env_or(?ENV_MAINT_DB, undefined) of
        undefined -> <<"postgres">>;
        Name -> list_to_binary(Name)
    end.

marker_db_name() ->
    Ts = os:system_time(microsecond),
    Rand = rand:uniform(16#FFFFFFFF),
    <<"org_member_inttest_", (integer_to_binary(Ts))/binary, "_",
        (integer_to_binary(Rand))/binary>>.

conn_opts(#{host := Host, port := Port, username := User, password := Pass}, Db) ->
    #{
        host => Host,
        port => Port,
        username => User,
        password => Pass,
        database => Db,
        timeout => 10000
    }.

create_db(Conn, DbName) ->
    case epgsql:squery(Conn, <<"CREATE DATABASE ", DbName/binary>>) of
        {ok, _, _} -> ok;
        {error, Reason} -> erlang:error({org_member_inttest_create_db_failed, Reason})
    end.

connect_server(Server, Db) ->
    case epgsql:connect(conn_opts(Server, Db)) of
        {ok, Conn} -> Conn;
        {error, Reason} -> erlang:error({org_member_inttest_connect_failed, Db, Reason})
    end.

%% 仓规（clean deploy 前置）：PG 扩展先于迁移存在，迁移文件自身不建扩展。
%% 清单与 imboy_v1（4396 dev 库）pg_extension 同源；缺任何一个都属于环境
%% 不满足，显式 FAIL。
required_extensions() ->
    [
        <<"pgcrypto">>,
        <<"pg_jieba">>,
        <<"timescaledb">>,
        <<"vector">>,
        <<"postgis">>,
        <<"pg_trgm">>,
        <<"btree_gin">>,
        <<"btree_gist">>,
        <<"citext">>,
        <<"unaccent">>,
        <<"intarray">>,
        <<"pg_stat_statements">>
    ].

ensure_extensions(Server, DbName) ->
    Conn = connect_server(Server, DbName),
    try
        lists:foreach(
            fun(Ext) ->
                Sql = <<"CREATE EXTENSION IF NOT EXISTS ", Ext/binary>>,
                case epgsql:squery(Conn, Sql) of
                    {ok, _, _} ->
                        ok;
                    {error, Reason} ->
                        erlang:error({org_member_inttest_extension_failed, Ext, Reason})
                end
            end,
            required_extensions()
        )
    after
        close_quiet(Conn)
    end.

migrate_fresh_db(Server, DbName) ->
    MigConn = connect_server(Server, DbName),
    try
        %% strict 与生产/演练（scripts/drill_migrate.escript）同口径；
        %% 全链 up 含 00000126/127 的 owner invariant 与 deferred guard。
        erlang_migrate:up(#{conn => MigConn, dir => "priv/migrations", strict => true})
    after
        close_quiet(MigConn)
    end.

migrate_result(ok) -> ok;
migrate_result({ok, _Applied}) -> ok;
migrate_result({error, Reason}) -> erlang:error({org_member_inttest_migrate_failed, Reason}).

try_drop_db(Server, DbName) ->
    case epgsql:connect(conn_opts(Server, maint_db())) of
        {ok, C} ->
            Drop =
                try epgsql:squery(C, <<"DROP DATABASE ", DbName/binary>>) of
                    {ok, _, _} -> ok;
                    Other -> {left_behind, Other}
                catch
                    _:_ -> {left_behind, drop_crashed}
                end,
            case Drop of
                ok ->
                    ok;
                {left_behind, Why} ->
                    io:format(
                        "~nWARNING: org_member_inttest marker db not dropped (~0p): ~ts~n",
                        [Why, DbName]
                    )
            end,
            close_quiet(C);
        {error, _} ->
            io:format("~nWARNING: maint reconnect failed, marker db ~ts left behind~n", [DbName])
    end.

with_rollback(Conn, Fun) ->
    ok = squery(Conn, <<"BEGIN">>),
    try
        Fun(Conn)
    after
        ok = squery(Conn, <<"ROLLBACK">>)
    end.

verify_persistence(Conn) ->
    assert_schema(Conn),
    seed(Conn),
    assert_owner_synced(Conn),
    assert_member_lifecycle(Conn),
    assert_workspace_membership_unchanged(Conn),
    assert_owner_guard(Conn),
    assert_owner_transfer_rolls_back_on_failure(Conn),
    assert_owner_transfer_keeps_workspace_unchanged(Conn).

assert_schema(Conn) ->
    {ok, _, [{<<"organization_member">>}]} = epgsql:equery(
        Conn, <<"SELECT to_regclass('public.organization_member')::text">>, []
    ),
    {ok, _, [{2}]} = epgsql:equery(
        Conn,
        <<"SELECT COUNT(*) FROM pg_trigger WHERE tgname IN ",
            "('trg_organization_owner_member_sync',",
            " 'trg_organization_primary_owner_member_guard') AND NOT tgisinternal">>,
        []
    ).

seed(Conn) ->
    ok = exec(Conn, <<
        "INSERT INTO \"user\" (id,password,account,reg_ip,reg_cosv) VALUES "
        "(99113001,'x','org_member_owner','127.0.0.1','x'),"
        "(99113002,'x','org_member_peer','127.0.0.1','x')"
    >>),
    ok = exec(Conn, <<
        "INSERT INTO organization (id,name,owner_id) "
        "VALUES (99113101,'Organization member integration',99113001)"
    >>),
    ok = exec(Conn, <<
        "INSERT INTO workspace (id,name,owner_id,organization_id) "
        "VALUES (99113201,'Cross organization workspace',99113001,99113101)"
    >>),
    ok = exec(Conn, <<
        "INSERT INTO workspace_member "
        "(workspace_id,user_id,role,invited_by,joined_at,status) VALUES "
        "(99113201,99113001,'owner',NULL,CURRENT_TIMESTAMP,'active'),"
        "(99113201,99113002,'member',99113001,CURRENT_TIMESTAMP,'active')"
    >>),
    ok = exec(Conn, <<
        "INSERT INTO \"group\" "
        "(id,owner_uid,creator_uid,scope,workspace_id,title) "
        "VALUES (99113301,99113001,99113001,'workspace',99113201,'Shared group')"
    >>),
    ok = exec(Conn, <<
        "INSERT INTO group_member (id,group_id,user_id,role,is_join,status) "
        "VALUES (99113401,99113301,99113002,0,true,1)"
    >>),
    ok = exec(Conn, <<"SET CONSTRAINTS ALL IMMEDIATE">>).

assert_owner_synced(Conn) ->
    {ok, Owner} = organization_member_repo:find_active_tx(
        Conn, ?ORG, ?OWNER, <<"organization_id,user_id,role,status">>
    ),
    ?assertEqual(<<"owner">>, maps:get(<<"role">>, Owner)),
    ?assertEqual(<<"active">>, maps:get(<<"status">>, Owner)).

assert_member_lifecycle(Conn) ->
    ?assertMatch(
        {ok, changed, _},
        organization_member_repo:upsert_active_tx(
            Conn, ?ORG, ?MEMBER, <<"member">>, ?OWNER
        )
    ),
    ?assertMatch(
        {ok, unchanged, _},
        organization_member_repo:upsert_active_tx(
            Conn, ?ORG, ?MEMBER, <<"member">>, ?OWNER
        )
    ),
    ok = organization_member_repo:update_role_tx(Conn, ?ORG, ?MEMBER, <<"admin">>),
    ok = organization_member_repo:remove_tx(Conn, ?ORG, ?MEMBER),
    ?assertEqual(
        {error, not_found},
        organization_member_repo:find_active_tx(Conn, ?ORG, ?MEMBER, <<"role">>)
    ),
    ?assertMatch(
        {ok, changed, _},
        organization_member_repo:upsert_active_tx(
            Conn, ?ORG, ?MEMBER, <<"member">>, ?OWNER
        )
    ).

assert_owner_guard(Conn) ->
    assert_rejected_write(Conn, fun() ->
        organization_member_repo:update_role_tx(Conn, ?ORG, ?OWNER, <<"member">>)
    end),
    assert_rejected_write(Conn, fun() ->
        organization_member_repo:remove_tx(Conn, ?ORG, ?OWNER)
    end),
    {ok, Owner} = organization_member_repo:find_active_tx(
        Conn, ?ORG, ?OWNER, <<"role,status">>
    ),
    ?assertEqual(<<"owner">>, maps:get(<<"role">>, Owner)),
    ?assertEqual(<<"active">>, maps:get(<<"status">>, Owner)).

assert_rejected_write(Conn, Fun) ->
    ok = squery(Conn, <<"SAVEPOINT organization_owner_guard">>),
    ?assertMatch({error, _}, Fun()),
    ok = squery(Conn, <<"ROLLBACK TO SAVEPOINT organization_owner_guard">>),
    ok = squery(Conn, <<"RELEASE SAVEPOINT organization_owner_guard">>).

assert_workspace_membership_unchanged(Conn) ->
    ok = organization_member_repo:remove_tx(Conn, ?ORG, ?MEMBER),
    {ok, _, [{<<"active">>}]} = epgsql:equery(
        Conn,
        <<"SELECT status FROM workspace_member WHERE workspace_id = $1 AND user_id = $2">>,
        [?WS, ?MEMBER]
    ),
    {ok, _, [{1}]} = epgsql:equery(
        Conn,
        <<"SELECT status FROM group_member WHERE group_id = $1 AND user_id = $2">>,
        [?GROUP, ?MEMBER]
    ),
    ?assertMatch(
        {ok, changed, _},
        organization_member_repo:upsert_active_tx(
            Conn, ?ORG, ?MEMBER, <<"member">>, ?OWNER
        )
    ).

assert_owner_transfer_rolls_back_on_failure(Conn) ->
    %% 注入点 = ORG-01 单事务实现的第一写步骤（store 层降旧 owner）。
    %% 旧实现按 organization_member_repo:update_role_tx 注入已失效：新链路
    %% 不经 repo 角色接口，直写 store UPDATE。
    ok = squery(Conn, <<"SET CONSTRAINTS ALL DEFERRED">>),
    with_bound_logic_tx(Conn, fun() ->
        meck:new(organization_owner_store, [passthrough, no_link]),
        meck:expect(
            organization_owner_store,
            demote_previous_owner_tx,
            fun(_Conn, ?ORG, ?OWNER) -> {error, injected_role_update} end
        ),
        try
            ?assertMatch(
                {error, {500, _}},
                organization_member_logic:transfer_owner(?OWNER, ?ORG, ?MEMBER)
            )
        after
            meck:unload(organization_owner_store)
        end
    end),
    {ok, _, [{?OWNER}]} = epgsql:equery(
        Conn, <<"SELECT owner_id FROM organization WHERE id = $1">>, [?ORG]
    ),
    {ok, _, [{<<"owner">>}]} = epgsql:equery(
        Conn,
        <<"SELECT role FROM organization_member WHERE organization_id = $1 AND user_id = $2">>,
        [?ORG, ?OWNER]
    ),
    {ok, _, [{<<"member">>}]} = epgsql:equery(
        Conn,
        <<"SELECT role FROM organization_member WHERE organization_id = $1 AND user_id = $2">>,
        [?ORG, ?MEMBER]
    ).

assert_owner_transfer_keeps_workspace_unchanged(Conn) ->
    %% 生产转移跑在 elib_pg:with_tx 默认约束时机（deferred 提交时裁决）；
    %% seed 段的 IMMEDIATE 只服务 owner guard 拒绝的确定性，此处切回。
    ok = squery(Conn, <<"SET CONSTRAINTS ALL DEFERRED">>),
    with_bound_logic_tx(Conn, fun() ->
        ?assertMatch(
            {ok, #{
                organization_id := ?ORG,
                owner_id := ?MEMBER,
                previous_owner_id := ?OWNER,
                previous_owner_role := <<"admin">>
            }},
            organization_member_logic:transfer_owner(?OWNER, ?ORG, ?MEMBER)
        )
    end),
    {ok, NewOwner} = organization_member_repo:find_active_tx(
        Conn, ?ORG, ?MEMBER, <<"role,status">>
    ),
    {ok, PreviousOwner} = organization_member_repo:find_active_tx(
        Conn, ?ORG, ?OWNER, <<"role,status">>
    ),
    ?assertEqual(<<"owner">>, maps:get(<<"role">>, NewOwner)),
    ?assertEqual(<<"admin">>, maps:get(<<"role">>, PreviousOwner)),
    {ok, _, [{?OWNER, ?ORG}]} = epgsql:equery(
        Conn,
        <<"SELECT owner_id,organization_id FROM workspace WHERE id = $1">>,
        [?WS]
    ),
    {ok, _, [{<<"owner">>}]} = epgsql:equery(
        Conn,
        <<"SELECT role FROM workspace_member WHERE workspace_id = $1 AND user_id = $2">>,
        [?WS, ?OWNER]
    ),
    {ok, _, [{<<"member">>}]} = epgsql:equery(
        Conn,
        <<"SELECT role FROM workspace_member WHERE workspace_id = $1 AND user_id = $2">>,
        [?WS, ?MEMBER]
    ),
    {ok, _, [{1}]} = epgsql:equery(
        Conn,
        <<"SELECT status FROM group_member WHERE group_id = $1 AND user_id = $2">>,
        [?GROUP, ?MEMBER]
    ).

with_bound_logic_tx(Conn, Fun) ->
    meck:new(elib_pg, [passthrough, no_link]),
    meck:expect(elib_pg, with_tx, fun(TxFun) -> savepoint_tx(Conn, TxFun) end),
    try
        Fun()
    after
        meck:unload(elib_pg)
    end.

savepoint_tx(Conn, Fun) ->
    ok = squery(Conn, <<"SAVEPOINT organization_owner_logic">>),
    try
        Result = Fun(Conn),
        ok = squery(Conn, <<"RELEASE SAVEPOINT organization_owner_logic">>),
        Result
    catch
        throw:{abort_tx, Reason} ->
            rollback_savepoint(Conn),
            {error, Reason};
        Class:Reason:Stacktrace ->
            rollback_savepoint(Conn),
            erlang:raise(Class, Reason, Stacktrace)
    end.

rollback_savepoint(Conn) ->
    ok = squery(Conn, <<"ROLLBACK TO SAVEPOINT organization_owner_logic">>),
    ok = squery(Conn, <<"RELEASE SAVEPOINT organization_owner_logic">>).

exec(Conn, Sql) ->
    case epgsql:squery(Conn, Sql) of
        {ok, _} ->
            ok;
        {ok, _, _} ->
            ok;
        {error, Reason} ->
            erlang:error({sql_error, Reason});
        Results when is_list(Results) ->
            case [Reason || {error, Reason} <- Results] of
                [] -> ok;
                [Reason | _] -> erlang:error({sql_error, Reason})
            end
    end.

squery(Conn, Sql) ->
    case epgsql:squery(Conn, Sql) of
        {ok, _} -> ok;
        {ok, _, _} -> ok;
        {error, Reason} -> erlang:error({sql_error, Reason})
    end.
