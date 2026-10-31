-module(moment_delete_atomic_pg_tests).
-include_lib("eunit/include/eunit.hrl").

delete_transaction_test_() ->
    {timeout, 900, fun() -> isolated_pg_fixture:run(fun run/0) end}.

run() ->
    ok = require_authorized_fixture(),
    ok = logger:set_primary_config(level, error),
    ok = configure_owned_postgres(),
    application:load(imboy),
    application:set_env(imboy, sql_driver, pgsql),
    State = inttest_marker_db:provision(#{
        env_prefix => marker_prefix(),
        connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
    }),
    Conn = maps:get(conn, State),
    Server = maps:get(server, State),
    Opts = maps:merge(
        maps:without([maint_db, connect_extra], Server),
        (maps:get(connect_extra, Server, #{}))#{database => maps:get(db, State)}
    ),
    try
        {ok, _} = application:ensure_all_started(pooler),
        undefined = whereis(pgsql),
        {ok, PoolSup} = pooler:new_pool(#{
            name => pgsql,
            max_count => 2,
            init_count => 1,
            start_mfa => {epgsql, connect, [Opts]}
        }),
        Pool = whereis(pgsql),
        try
            seed(Conn),
            ?assertMatch({error, _}, moment_ds:delete_post(970001, 970101)),
            assert_state(Conn, 970101, 1, 1),
            ?assertMatch({error, _}, moment_ds:delete_post_by_admin(970102)),
            assert_state(Conn, 970102, 1, 1),
            ?assertEqual(ok, moment_ds:delete_post(970001, 970103)),
            assert_state(Conn, 970103, -1, 0),
            ?assertEqual(ok, moment_ds:delete_post_by_admin(970104)),
            assert_state(Conn, 970104, -1, 0),
            ?assertEqual({error, no_permission}, moment_ds:delete_post(970002, 970105)),
            assert_state(Conn, 970105, 1, 1)
        after
            {PoolSup, Pool} = {whereis(pooler_pgsql_pool_sup), whereis(pgsql)},
            pooler:rm_pool(pgsql)
        end
    after
        inttest_marker_db:release(State)
    end.

%% 资源授权形态（no-skip 铁律不变，任一环节失败显式 error）：
%% - native：回执车道，configure 验证回执并注入 <PREFIX>_DB（绑定库形态）。
%% - v5_run：attv5 legacy 车道（V5_RUN_DIR），guard 脚本验证连接身份后仍走
%%   V5 前缀绑定合约（inttest_marker_db fail-closed，缺绑定即显式 error）。
%% - ordinary：quality 普通车道，前两者皆无。原 require_authorized_fixture
%%   把"无回执"一律判 contract_conflict 属误设：inttest_marker_db 的真实合约
%%   并不要求回执，非 V5 前缀走其标准一次性 marker 库形态（自建库 + 12 扩展
%%   + 全链 strict 迁移，release 时 DROP）；连接身份取 -config 注入的
%%   imboy.pg_conf（quality 车道为控制器生成的 worker 私有克隆库 config，
%%   user/pass 缺失由 resolve_server fail-closed），delete 原子性断言不变。
form() ->
    case {native_v5_fixture:enabled(), os:getenv("V5_RUN_DIR")} of
        {true, _} -> native;
        {false, Run} when is_list(Run), Run =/= "" -> v5_run;
        {false, _} -> ordinary
    end.

marker_prefix() ->
    case form() of
        ordinary -> <<"IMBOY_INTTEST_MOMENT">>;
        _ -> <<"V5_INTTEST_MOMENT">>
    end.

require_authorized_fixture() ->
    case form() of
        native -> native_v5_fixture:configure(<<"V5_INTTEST_MOMENT">>);
        _ -> ok
    end.

configure_owned_postgres() ->
    case form() of
        native -> native_v5_fixture:configure(<<"V5_INTTEST_MOMENT">>);
        v5_run -> configure_legacy_postgres();
        ordinary -> ok
    end.

configure_legacy_postgres() ->
    Run = os:getenv("V5_RUN_DIR"),
    Slot = os:getenv("V5_SLOT"),
    true = is_list(Run) andalso filename:pathtype(Run) =:= absolute,
    true = is_list(Slot) andalso Slot =/= "",
    Python = os:find_executable("python3"),
    true = is_list(Python),
    Script = filename:join([
        filename:dirname(code:priv_dir(imboy)),
        "scripts",
        "test",
        "attachment_lifecycle_v5_pg_guard.py"
    ]),
    Port = open_port(
        {spawn_executable, Python},
        [binary, exit_status, stderr_to_stdout, {args, [Script, Run, Slot]}]
    ),
    Output = resource_guard_result(Port, <<>>),
    [_Container, <<"127.0.0.1">>, PgPort] =
        binary:split(string:trim(Output), <<"\t">>, [global]),
    {ok, PasswordBytes} = file:read_file(filename:join([Run, "resources", "pg-attv5.password"])),
    Password = string:trim(PasswordBytes),
    true = byte_size(Password) > 0,
    true = os:putenv("V5_INTTEST_MOMENT_PG_HOST", "127.0.0.1"),
    true = os:putenv("V5_INTTEST_MOMENT_PG_PORT", binary_to_list(PgPort)),
    true = os:putenv("V5_INTTEST_MOMENT_PG_USER", "imboy_user"),
    true = os:putenv("V5_INTTEST_MOMENT_PG_PASSWORD", binary_to_list(Password)),
    true = os:putenv("V5_INTTEST_MOMENT_MAINT_DB", "postgres"),
    ok.

resource_guard_result(Port, Acc) when byte_size(Acc) < 4096 ->
    receive
        {Port, {data, Data}} -> resource_guard_result(Port, <<Acc/binary, Data/binary>>);
        {Port, {exit_status, 0}} -> Acc;
        {Port, {exit_status, Status}} -> error({owned_postgres_guard_failed, Status})
    after 10000 ->
        port_close(Port),
        error(owned_postgres_guard_timeout)
    end;
resource_guard_result(Port, _) ->
    port_close(Port),
    error(owned_postgres_guard_output_too_large).

seed(Conn) ->
    q(
        Conn,
        "INSERT INTO public.\"user\"(id,password,account,reg_ip,reg_cosv) VALUES"
        " (970001,'x','v5-moment-owner','127.0.0.1','x'),"
        " (970002,'x','v5-moment-other','127.0.0.1','x')"
    ),
    q(
        Conn,
        "INSERT INTO moment_post(id,author_uid)"
        " SELECT id,970001 FROM generate_series(970101,970105) AS id"
    ),
    q(
        Conn,
        "INSERT INTO moment_timeline(id,recipient_uid,post_id,author_uid,rank_at)"
        " SELECT id+100,970001,id,970001,now() FROM generate_series(970101,970105) AS id"
    ),
    q(
        Conn,
        "CREATE FUNCTION public.test_moment_hide_failure() RETURNS trigger LANGUAGE plpgsql AS $$"
        " BEGIN IF NEW.post_id IN (970101,970102) THEN RAISE EXCEPTION 'injected hide failure';"
        " END IF; RETURN NEW; END $$"
    ),
    q(
        Conn,
        "CREATE TRIGGER test_moment_hide_failure BEFORE UPDATE ON moment_timeline"
        " FOR EACH ROW EXECUTE FUNCTION public.test_moment_hide_failure()"
    ).

assert_state(Conn, Post, PostStatus, TimelineStatus) ->
    {ok, _, [{PostStatus, TimelineStatus}]} = epgsql:equery(
        Conn,
        "SELECT p.status,t.status FROM moment_post p JOIN moment_timeline t ON t.post_id=p.id"
        " WHERE p.id=$1",
        [Post]
    ).

q(Conn, Sql) ->
    case epgsql:squery(Conn, Sql) of
        {ok, _, _} -> ok;
        {ok, _} -> ok
    end.
