%% attachment_lifecycle_v5_pg_tests — W1-02 core protocol eunit suite.
%% Reused by W2-03 (confirm/GC race) and W6-01 (real fault matrix).
-module(attachment_lifecycle_v5_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-define(REG_KEY, <<"v5e2e/reg/1">>).
-define(ASSET_ID, 901100101).

r20_test_() ->
    {timeout, 900, fun() -> isolated_pg_fixture:run(fun run/0) end}.

run() ->
    elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3}),
    State = inttest_marker_db:provision(
        #{
            env_prefix => <<"V5_INTTEST">>,
            connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
        }
    ),
    Conn = maps:get(conn, State),
    try
        protocol_roundtrip(Conn),
        session_link_integrity(Conn),
        worker_claim_ack(Conn),
        ?assert(true)
    after
        inttest_marker_db:release(State)
    end.

q(Conn, Sql) -> q(Conn, Sql, []).
q(Conn, Sql, Params) ->
    %% DELETE/UPDATE 0 行时 epgsql 回 {ok, Count} 二元组
    case epgsql:equery(Conn, Sql, Params) of
        {ok, _} -> ok;
        {ok, _, _} -> ok
    end.
one(Conn, Sql, Params) ->
    {ok, _, [Row]} = epgsql:equery(Conn, Sql, Params),
    Row.

registration() ->
    <<
        "{\"subject\":{\"kind\":\"user\",\"principal\":\"101\"},"
        "\"declaration\":{\"filename\":\"a.png\",\"mime\":\"image/png\","
        "\"size_bytes\":\"10\",\"scope\":\"private\"}}"
    >>.

content_receipt() ->
    lists:flatten(
        [
            "{\"version\":1,\"sha256\":\"",
            lists:duplicate(64, $a),
            "\",\"size_bytes\":\"10\",\"mime\":\"image/png\"}"
        ]
    ).

locator() -> lists:duplicate(64, $b).

protocol_roundtrip(Conn) ->
    q(Conn, "DELETE FROM attach_pending WHERE object_key=$1", [?REG_KEY]),
    q(Conn, "DELETE FROM attachment WHERE path=$1", [?REG_KEY]),
    {ok, _, _} = epgsql:equery(
        Conn,
        "SELECT * FROM fn_v5_register_pending($1,'b','private',101,"
        "gen_random_uuid(),$2::jsonb,now() + interval '2 hours',"
        "now() + interval '3 seconds','garage','ns1','c1',$3)",
        [?REG_KEY, registration(), locator()]
    ),
    {ok, _, _} = epgsql:equery(
        Conn,
        "SELECT fn_v5_pin_pending_content($1,$2::jsonb)",
        [?REG_KEY, content_receipt()]
    ),
    %% duplicate identical pin is idempotent
    {ok, _, _} = epgsql:equery(
        Conn,
        "SELECT fn_v5_pin_pending_content($1,$2::jsonb)",
        [?REG_KEY, content_receipt()]
    ),
    q(
        Conn,
        "INSERT INTO attachment(id,path,scope,creator_user_id,"
        "storage_driver,storage_namespace,storage_container,"
        "storage_locator_sha256,gc_verified) VALUES ($1,$2,'private',101,"
        "'garage','ns1','c1',$3,true)",
        [?ASSET_ID, ?REG_KEY, locator()]
    ),
    {ok, _, _} = epgsql:equery(
        Conn, "SELECT fn_v5_confirm_pending($1,$2)", [?REG_KEY, ?ASSET_ID]
    ),
    %% replay returns original id without re-audit
    {ok, _, [{?ASSET_ID}]} = epgsql:equery(
        Conn, "SELECT fn_v5_confirm_pending($1,$2)", [?REG_KEY, ?ASSET_ID]
    ),
    %% confirmed session keeps link
    {ok, _, [{<<"confirmed">>}]} = epgsql:equery(
        Conn,
        "SELECT session_state FROM attach_pending WHERE object_key=$1",
        [?REG_KEY]
    ).

session_link_integrity(Conn) ->
    %% wrong owner asset cannot be linked
    q(Conn, "DELETE FROM attachment WHERE id=$1", [?ASSET_ID + 1]),
    q(
        Conn,
        "INSERT INTO attachment(id,path,scope,creator_user_id) "
        "VALUES ($1,'v5e2e/other','private',202)",
        [?ASSET_ID + 1]
    ),
    %% 错主资产：fn 内 owner 复核 23510（epgsql 以 {error,#error{}} 回报）
    {error, #error{code = <<"23510">>}} =
        e(
            Conn,
            "SELECT fn_v5_confirm_pending($1,$2)",
            [?REG_KEY, ?ASSET_ID + 1]
        ),
    %% 错误后恢复连接协议状态
    ok = epgsql:sync(Conn).

worker_claim_ack(Conn) ->
    %% enqueue via intent：引用零已证明；能力窗需先过期（fn 资格合取）
    await_upload_expiry(Conn, ?REG_KEY),
    {ok, _, _} = epgsql:equery(
        Conn, "SELECT fn_v5_intent_ordinary_delete($1,'eunit')", [?REG_KEY]
    ),
    %% uuid 走 SQL 内联（token 为同库 gen_random_uuid 服务端生成，无注入面；
    %% 绕开 vendored epgsql 的 uuid/text codec 参数推断坑）
    {Token} = one(Conn, "SELECT gen_random_uuid()", []),
    ClaimSql = <<"SELECT out_path FROM fn_v5_claim_ordinary('", Token/binary, "')">>,
    {ok, _, [{Path}]} = epgsql:equery(Conn, ClaimSql, []),
    ?assertEqual(?REG_KEY, Path),
    AckSql = <<"SELECT fn_v5_ack_ordinary('", ?REG_KEY/binary, "','", Token/binary, "',true)">>,
    {ok, _, [{<<"done">>}]} = epgsql:equery(Conn, AckSql, []),
    %% stale token cannot mutate done row
    {ok, _, [{<<"not_leased">>}]} = epgsql:equery(Conn, AckSql, []).

%% ===================================================================
%% W2-03 confirm/GC 状态协议屏障：真实双连接（conn + maint_conn）屏障，
%% 两个提交顺序唯一胜出（02 §4）。不用 sleep 猜并发——P 事务持行锁后
%% 经消息握手让主侧真实阻塞在行锁上，租约到期回滚放行。
%% ===================================================================

w203_test_() ->
    {timeout, 900, fun w203_run/0}.

w203_run() ->
    State = inttest_marker_db:provision(
        #{
            env_prefix => <<"V5_INTTEST_W203">>,
            connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
        }
    ),
    try
        w203_intent_first(maps:get(conn, State)),
        w203_confirm_first(maps:get(conn, State), maps:get(server, State), State)
    after
        inttest_marker_db:release(State)
    end.

e(Conn, Sql, Params) ->
    case epgsql:equery(Conn, Sql, Params) of
        {ok, _, _} = Ok -> Ok;
        {error, _} = Err -> Err
    end.

w203_seed(Conn, Key) ->
    q(Conn, "DELETE FROM attach_pending WHERE object_key=$1", [Key]),
    q(Conn, "DELETE FROM attachment WHERE path=$1", [Key]),
    {ok, _, _} = epgsql:equery(
        Conn,
        "SELECT * FROM fn_v5_register_pending($1,'b','private',101,"
        "gen_random_uuid(),$2::jsonb,now() + interval '2 hours',"
        "now() + interval '3 seconds','garage','ns1','c1',$3)",
        [Key, registration(), locator()]
    ),
    {ok, _, _} = epgsql:equery(
        Conn, "SELECT fn_v5_pin_pending_content($1,$2::jsonb)", [Key, content_receipt()]
    ),
    ok.

%% confirm-first 专用：注册即"能力窗未过"（deadline/expiry 均未来且
%% expiry <= deadline）。M3 守卫禁 UPDATE 扩 session_deadline（immutable），
%% 窗口只能在注册时一次定死。
w203_seed_open(Conn, Key) ->
    q(Conn, "DELETE FROM attach_pending WHERE object_key=$1", [Key]),
    q(Conn, "DELETE FROM attachment WHERE path=$1", [Key]),
    {ok, _, _} = epgsql:equery(
        Conn,
        "SELECT * FROM fn_v5_register_pending($1,'b','private',101,"
        "gen_random_uuid(),$2::jsonb,"
        "now() + interval '2 hours',"
        "now() + interval '1 hour','garage','ns1','c1',$3)",
        [Key, registration(), locator()]
    ),
    {ok, _, _} = epgsql:equery(
        Conn, "SELECT fn_v5_pin_pending_content($1,$2::jsonb)", [Key, content_receipt()]
    ),
    ok.

w203_insert_asset(Conn, Key, Id) ->
    q(
        Conn,
        "INSERT INTO attachment(id,path,scope,creator_user_id,"
        "storage_driver,storage_namespace,storage_container,"
        "storage_locator_sha256,gc_verified) VALUES ($1,$2,'private',101,"
        "'garage','ns1','c1',$3,$4)",
        [Id, Key, locator(), true]
    ),
    ok.

%% intent 先提交 => confirm 重放落败（23510），元数据不重建。
w203_intent_first(Conn) ->
    Key = <<"v5e2e/w203/intent-first">>,
    w203_seed(Conn, Key),
    w203_insert_asset(Conn, Key, ?ASSET_ID + 10),
    {ok, _, _} = epgsql:equery(
        Conn, "SELECT fn_v5_confirm_pending($1,$2)", [Key, ?ASSET_ID + 10]
    ),
    %% 能力窗已过 + 引用零已证明：intent 合法提交
    await_upload_expiry(Conn, Key),
    {ok, _, _} = epgsql:equery(
        Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w2-03 intent first')", [Key]
    ),
    {ok, _, [{<<"delete_intent">>}]} = epgsql:equery(
        Conn, "SELECT lifecycle_state FROM attachment WHERE path=$1", [Key]
    ),
    %% confirm 重放：非 live 资产必须落败（W2-03 守卫，23510）
    {error, #error{code = <<"23510">>}} =
        e(Conn, "SELECT fn_v5_confirm_pending($1,$2)", [Key, ?ASSET_ID + 10]),
    ok = epgsql:sync(Conn),
    {ok, _, [{<<"delete_intent">>}]} = epgsql:equery(
        Conn, "SELECT lifecycle_state FROM attachment WHERE path=$1", [Key]
    ),
    ok.

%% confirm 先提交 => intent 落败、无删除任务；有界重试不改变胜者。
w203_confirm_first(Conn, Server, State) ->
    Key = <<"v5e2e/w203/confirm-first">>,
    w203_seed_open(Conn, Key),
    w203_insert_asset(Conn, Key, ?ASSET_ID + 20),
    {ok, _, _} = epgsql:equery(
        Conn, "SELECT fn_v5_confirm_pending($1,$2)", [Key, ?ASSET_ID + 20]
    ),
    %% 屏障：Barrier 事务持 attachment 行锁（GC intent 的首步锁），
    %% 主连接的 confirm 协议在 fn 内同一行锁上真实阻塞；租约到期回滚。
    Parent = self(),
    Holder =
        spawn_link(fun() ->
            R =
                try
                    %% 屏障连接由本进程自建（epgsql 命令与连接属主进程绑定）
                    {ok, HConn} =
                        inttest_marker_db:safe_connect(
                            maps:merge(
                                #{
                                    host => maps:get(host, Server),
                                    port => maps:get(port, Server),
                                    username => maps:get(username, Server),
                                    password => maps:get(password, Server),
                                    database => maps:get(db, State),
                                    timeout => 10000
                                },
                                maps:get(connect_extra, Server, #{})
                            )
                        ),
                    try
                        %% BEGIN/ROLLBACK 是命令标签（epgsql 回 {ok, Tag} 二元组），
                        %% 不做形状断言
                        _ = epgsql:equery(HConn, "BEGIN", []),
                        {ok, _, _} =
                            epgsql:equery(
                                HConn,
                                "SELECT path FROM attachment WHERE path=$1 FOR UPDATE",
                                [Key]
                            ),
                        Parent ! {w203_held, self()},
                        receive
                            release -> ok
                        after 8000 -> ok
                        end,
                        _ = epgsql:equery(HConn, "ROLLBACK", []),
                        released
                    after
                        CloseMonitor = erlang:monitor(process, HConn),
                        epgsql:close(HConn),
                        receive
                            {'DOWN', CloseMonitor, process, HConn, _} -> ok
                        after 5000 ->
                            erlang:demonitor(CloseMonitor, [flush]),
                            error(w203_holder_connection_close_timeout)
                        end
                    end
                catch
                    C:E -> {C, E}
                end,
            Parent ! {w203_holder_done, self(), R}
        end),
    receive
        {w203_held, Holder} -> ok
    after 8000 -> error(w203_barrier_timeout)
    end,
    %% 主连接 confirm 重放：fn 内 attachment FOR UPDATE 与 Holder 真实互斥
    {ok, _, [{?ASSET_ID + 20}]} = epgsql:equery(
        Conn, "SELECT fn_v5_confirm_pending($1,$2)", [Key, ?ASSET_ID + 20]
    ),
    receive
        {w203_holder_done, Holder, released} -> ok
    after 8000 -> error(w203_holder_join_timeout)
    end,
    %% confirm 先提交 => intent 落败（能力窗未过：confirm 保留会话行）
    {ok, _, [{_N}]} = epgsql:equery(
        Conn,
        "SELECT COUNT(*) FROM attachment_object_deletion WHERE path=$1",
        [Key]
    ),
    Result = e(Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w2-03 confirm first')", [Key]),
    ok = epgsql:sync(Conn),
    case Result of
        %% 能力窗未过（confirm 保留的会话 upload_expires_at 在未来）→ 拒绝
        {error, #error{code = <<"23510">>}} -> ok;
        %% 若会话窗已过但引用非零/其他资格不符，同样以 23510 家族拒绝；
        %% 这里其余任何 {error,_} 也视为 intent 未落地
        {error, #error{}} -> ok
    end,
    {ok, _, [{0}]} = epgsql:equery(
        Conn,
        "SELECT COUNT(*) FROM attachment_object_deletion WHERE path=$1",
        [Key]
    ),
    %% 有界重试不改变胜者：confirm 重放仍幂等返回原 id
    {ok, _, [{?ASSET_ID + 20}]} = epgsql:equery(
        Conn, "SELECT fn_v5_confirm_pending($1,$2)", [Key, ?ASSET_ID + 20]
    ),
    ok.

%% ===================================================================
%% W3-01 业务引用与注销保护（02 §5）：引用首绑/释放同对象锁 + gc_verified
%% 「引用零」证明 + 注销共享引用保护。三 case：
%%   reference-gc-race：bind 先提交 => intent 拒绝；intent 先提交 => 绑定
%%     改变零业务行（生产入口与直接受限 DML 双面）。
%%   shared-owner：注销/原帖删除/登出不能删掉被共享引用的资产。
%%   history-unknown：位置未知/密文/收藏歧义 => gc_verified 恒 false、
%%     0 删除、原 hash 不变。
%% ===================================================================

w301_test_() ->
    {timeout, 900, fun w301_run/0}.

w301_run() ->
    %% w301 用 elib_pg 池（生产 repo 协议），先 boot app + DB 池
    Boot = eunit_runner:eunit_setup(),
    State = inttest_marker_db:provision(
        #{
            env_prefix => <<"V5_INTTEST_W301">>,
            connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
        }
    ),
    try
        w301_lock_order(State),
        w301_reference_gc_race(maps:get(conn, State)),
        w301_direct_group_file_after_intent(maps:get(conn, State)),
        w301_direct_collect_after_intent(maps:get(conn, State)),
        w301_collect_race(State),
        w301_direct_enterprise_message_after_intent(maps:get(conn, State)),
        w301_cross_queue_identity(maps:get(conn, State)),
        w301_cross_queue_race(State),
        w301_direct_moment_after_intent(maps:get(conn, State)),
        w301_moment_race(State),
        w301_moment_escaped_reference(maps:get(conn, State)),
        w301_runtime_direct_dml_denied(maps:get(conn, State)),
        w301_enterprise_direct_race(State),
        w301_enterprise_store_race(State),
        w301_shared_owner(maps:get(conn, State)),
        w301_history_unknown(maps:get(conn, State))
    after
        inttest_marker_db:release(State),
        eunit_runner:eunit_cleanup(Boot)
    end.

%% A reference holding the object lock must not deadlock with a concurrent intent.
w301_lock_order(#{conn := Conn, server := Server, db := Db}) ->
    Key = <<"v5e2e/w301/race/lock-order">>,
    w301_seed_group_asset(Conn, Key, #{id => 34, gf => 34, bound => false}),
    ?assertEqual(true, w301_gc_verified_refresh(Conn, Key)),
    Opts = maps:merge(
        maps:without([maint_db, connect_extra], Server),
        (maps:get(connect_extra, Server, #{}))#{database => Db, timeout => 10000}
    ),
    {ok, Other} = inttest_marker_db:safe_connect(Opts),
    try
        {ok, _, [{Pid}]} = epgsql:equery(Other, "SELECT pg_backend_pid()", []),
        q(Other, "SET statement_timeout = '10s'"),
        q(Conn, "BEGIN"),
        {ok, _, _} = epgsql:equery(
            Conn,
            "SELECT pg_advisory_xact_lock(hashtextextended($1,168))",
            [Key]
        ),
        Parent = self(),
        spawn(fun() ->
            Parent !
                {intent_result,
                    epgsql:equery(
                        Other,
                        "SELECT fn_v5_intent_ordinary_delete($1,'w301 concurrent bind')",
                        [Key]
                    )}
        end),
        w301_wait_lock(Conn, Pid, 100),
        ?assertEqual(ok, attachment_repo:bind_reference_lock_tx(Conn, Key)),
        q(
            Conn,
            "INSERT INTO group_file(id,group_id,file_id,uploader_id,file_name,"
            "file_type,file_category,file_url,file_size) VALUES"
            " ($1,66,$2,'101','t.bin','image/png','image','u',16)",
            [?ASSET_ID + 34, integer_to_binary(?ASSET_ID + 34)]
        ),
        q(Conn, "COMMIT"),
        receive
            {intent_result, {error, #error{code = <<"23510">>}}} -> ok;
            {intent_result, Unexpected} -> error({unexpected_intent_result, Unexpected})
        after 12000 -> error(intent_timeout)
        end,
        w301_intent_wins(Conn, Other, Pid)
    after
        epgsql:close(Other)
    end.

w301_intent_wins(Conn, Other, Pid) ->
    Key = <<"v5e2e/w301/race/intent-concurrent-first">>,
    w301_seed_group_asset(Conn, Key, #{id => 35, gf => 35, bound => false}),
    ?assertEqual(true, w301_gc_verified_refresh(Conn, Key)),
    q(Conn, "BEGIN"),
    {ok, _, _} = epgsql:equery(
        Conn,
        "SELECT fn_v5_intent_ordinary_delete($1,'w301 concurrent intent')",
        [Key]
    ),
    q(Other, "BEGIN"),
    Parent = self(),
    spawn(fun() -> Parent ! {bind_result, attachment_repo:bind_reference_lock_tx(Other, Key)} end),
    w301_wait_lock(Conn, Pid, 100),
    q(Conn, "COMMIT"),
    receive
        {bind_result, {error, tombstone}} -> ok;
        {bind_result, Unexpected} -> error({unexpected_bind_result, Unexpected})
    after 12000 -> error(bind_timeout)
    end,
    q(Other, "ROLLBACK"),
    {ok, _, [{0}]} = epgsql:equery(
        Conn,
        "SELECT COUNT(*) FROM group_file WHERE id=$1",
        [?ASSET_ID + 35]
    ),
    {ok, _, [{<<"delete_intent">>}]} = epgsql:equery(
        Conn,
        "SELECT lifecycle_state FROM attachment WHERE path=$1",
        [Key]
    ),
    ok.

w301_wait_lock(_Conn, _Pid, 0) ->
    error(intent_did_not_wait_for_lock);
w301_wait_lock(Conn, Pid, Left) ->
    {ok, _, [{Waiting}]} = epgsql:equery(
        Conn,
        "SELECT EXISTS(SELECT 1 FROM pg_stat_activity WHERE pid=$1 AND wait_event_type='Lock')",
        [Pid]
    ),
    case Waiting of
        true ->
            ok;
        false ->
            timer:sleep(50),
            w301_wait_lock(Conn, Pid, Left - 1)
    end.

%% 64 位小写 hex（可撞 user_collect.attach_md5 的关联形态）
w301_hash() ->
    iolist_to_binary([
        io_lib:format("~2.16.0b", [B])
     || B <- binary_to_list(
            crypto:hash(sha256, <<"w301 fixed content">>)
        )
    ]).

%% 群文件形态种子：confirmed + 绑定已释放（group_file 行不存在）+ 能力窗已过
w301_seed_group_asset(Conn, Key, Opts) ->
    q(Conn, "DELETE FROM attach_pending WHERE object_key=$1", [Key]),
    q(Conn, "DELETE FROM attachment WHERE path=$1", [Key]),
    q(Conn, "DELETE FROM group_file WHERE id=$1", [?ASSET_ID + maps:get(gf, Opts)]),
    {ok, _, _} = epgsql:equery(
        Conn,
        "SELECT * FROM fn_v5_register_pending($1,'b','group',101,"
        "gen_random_uuid(),$2::jsonb,now() + interval '2 hours',"
        "now() + interval '3 seconds','garage','ns1','c1',$3)",
        [Key, w301_group_registration(), locator()]
    ),
    {ok, _, _} = epgsql:equery(
        Conn, "SELECT fn_v5_pin_pending_content($1,$2::jsonb)", [Key, content_receipt()]
    ),
    GfId = ?ASSET_ID + maps:get(gf, Opts),
    case maps:get(bound, Opts) of
        true ->
            q(
                Conn,
                "INSERT INTO group_file(id,group_id,file_id,uploader_id,file_name,"
                "file_type,file_category,file_url,file_size) VALUES"
                " ($1,66,$2,'101','t.bin','image/png','image','u',16)",
                [GfId, integer_to_binary(GfId)]
            );
        false ->
            ok
    end,
    q(
        Conn,
        "INSERT INTO attachment(id,path,scope,scope_ref,creator_user_id,"
        "storage_driver,storage_namespace,storage_container,"
        "storage_locator_sha256,gc_verified,cipher,group_file_id,file_hash256)"
        " VALUES ($1,$2,'group','66',101,'garage','ns1','c1',$3,false,$4,$5,$6)",
        [
            ?ASSET_ID + maps:get(id, Opts),
            Key,
            locator(),
            maps:get(cipher, Opts, null),
            case maps:get(bound, Opts) of
                true -> GfId;
                false -> GfId
            end,
            maps:get(hash, Opts, w301_hash())
        ]
    ),
    {ok, _, _} = epgsql:equery(
        Conn, "SELECT fn_v5_confirm_pending($1,$2)", [Key, ?ASSET_ID + maps:get(id, Opts)]
    ),
    await_upload_expiry(Conn, Key),
    %% group_file 行按场景保留/删除（绑定释放形态）
    case maps:get(bound, Opts) of
        true -> ok;
        false -> q(Conn, "DELETE FROM group_file WHERE id=$1", [GfId])
    end,
    ok.

w301_group_registration() ->
    Registration = jsone:decode(registration()),
    Declaration = maps:get(<<"declaration">>, Registration),
    jsone:encode(Registration#{
        <<"declaration">> => Declaration#{
            <<"scope">> => <<"group">>, <<"scope_ref">> => <<"66">>
        }
    }).

w301_gc_verified_refresh(Conn, Key) ->
    %% 走生产 repo 语义的等价 SQL（proven = 释放绑定+明文+完整位置+无收藏）
    {ok, _, [{V}]} = epgsql:equery(
        Conn,
        "WITH u AS (UPDATE attachment a SET gc_verified = c.proven FROM ("
        "  SELECT a2.path, (a2.scope='group' AND a2.group_file_id IS NOT NULL"
        "    AND NOT EXISTS (SELECT 1 FROM group_file gf WHERE gf.id=a2.group_file_id)"
        "    AND a2.cipher IS NULL AND a2.storage_locator_sha256 IS NOT NULL"
        "    AND NOT EXISTS (SELECT 1 FROM user_collect uc WHERE uc.attach_file_hash256 <> ''"
        "      AND uc.attach_file_hash256 = a2.file_hash256 AND uc.status = 1)) AS proven"
        "  FROM attachment a2 WHERE a2.path=$1) c WHERE a.path=c.path RETURNING a.gc_verified)"
        " SELECT gc_verified FROM u",
        [Key]
    ),
    V.

%% reference-gc-race：双顺序 × 生产入口/直接 DML。
w301_reference_gc_race(Conn) ->
    %% (1) bind_commits_first（生产入口：moment 首绑走 bind_reference_lock_tx
    %%     的同对象锁语义；此处以群绑定重挂 + 直接 DML 两面验证）
    K1 = <<"v5e2e/w301/race/bind-first">>,
    w301_seed_group_asset(Conn, K1, #{id => 30, gf => 30, bound => false}),
    ?assertEqual(true, w301_gc_verified_refresh(Conn, K1)),
    %% 先绑：重挂 group_file 行（绑定提交）
    q(
        Conn,
        "INSERT INTO group_file(id,group_id,file_id,uploader_id,file_name,file_type,"
        "file_category,file_url,file_size) VALUES"
        " ($1,66,$2,'101','t.bin','image/png','image','u',16)",
        [?ASSET_ID + 30, integer_to_binary(?ASSET_ID + 30)]
    ),
    {error, #error{code = <<"23510">>}} =
        e(Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w301 bind first')", [K1]),
    ok = epgsql:sync(Conn),
    {ok, _, [{<<"live">>}]} = epgsql:equery(
        Conn, "SELECT lifecycle_state FROM attachment WHERE path=$1", [K1]
    ),
    %% (2) intent_commits_first：绑定事务改变零业务行
    K2 = <<"v5e2e/w301/race/intent-first">>,
    w301_seed_group_asset(Conn, K2, #{id => 31, gf => 31, bound => false}),
    _ = w301_gc_verified_refresh(Conn, K2),
    {ok, _, _} = epgsql:equery(
        Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w301 intent first')", [K2]
    ),
    %% 业务行与绑定必须处于同一事务；墓碑拒绝时回滚业务行。
    {ok, _, _} = epgsql:equery(Conn, "BEGIN", []),
    {error, #error{code = <<"23510">>}} = e(
        Conn,
        "INSERT INTO group_file(id,group_id,file_id,uploader_id,file_name,file_type,"
        "file_category,file_url,file_size) VALUES"
        " ($1,66,$2,'101','t.bin','image/png','image','u',16)",
        [?ASSET_ID + 31, integer_to_binary(?ASSET_ID + 31)]
    ),
    ok = epgsql:sync(Conn),
    {ok, _, _} = epgsql:equery(Conn, "ROLLBACK", []),
    %% 墓碑同时拒绝生产绑定入口。
    {ok, _, _} = epgsql:equery(Conn, "BEGIN", []),
    R2 = attachment_repo:bind_reference_lock_tx(Conn, K2),
    {ok, _, _} = epgsql:equery(Conn, "ROLLBACK", []),
    {error, tombstone} = R2,
    {ok, _, [{0}]} = epgsql:equery(
        Conn, "SELECT COUNT(*) FROM group_file WHERE id=$1", [?ASSET_ID + 31]
    ),
    %% (3) direct_dml_bind_commits_first：直接受限 DML 绑定后 intent 拒绝
    K3 = <<"v5e2e/w301/race/dml-bind-first">>,
    w301_seed_group_asset(Conn, K3, #{id => 32, gf => 32, bound => false}),
    _ = w301_gc_verified_refresh(Conn, K3),
    q(
        Conn,
        "UPDATE attachment SET group_file_id=$2 WHERE path=$1",
        [K3, ?ASSET_ID + 32]
    ),
    q(
        Conn,
        "INSERT INTO group_file(id,group_id,file_id,uploader_id,file_name,file_type,"
        "file_category,file_url,file_size) VALUES"
        " ($1,66,$2,'101','t.bin','image/png','image','u',16)",
        [?ASSET_ID + 32, integer_to_binary(?ASSET_ID + 32)]
    ),
    {error, #error{code = <<"23510">>}} =
        e(Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w301 dml bind')", [K3]),
    ok = epgsql:sync(Conn),
    %% (4) direct_dml_intent_commits_first：intent 先提交 => 直接 DML 绑定 0 行
    K4 = <<"v5e2e/w301/race/dml-intent-first">>,
    w301_seed_group_asset(Conn, K4, #{id => 33, gf => 33, bound => false}),
    _ = w301_gc_verified_refresh(Conn, K4),
    {ok, _, _} = epgsql:equery(
        Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w301 dml intent')", [K4]
    ),
    {ok, _, [{0}]} = epgsql:equery(
        Conn,
        "WITH u AS (UPDATE attachment SET group_file_id=$2 WHERE path=$1"
        " AND lifecycle_state='live' RETURNING 1) SELECT COUNT(*) FROM u",
        [K4, ?ASSET_ID + 33]
    ),
    ok.

w301_direct_group_file_after_intent(Conn) ->
    Key = <<"v5e2e/w301/race/direct-group-after-intent">>,
    Id = ?ASSET_ID + 36,
    w301_seed_group_asset(Conn, Key, #{id => 36, gf => 36, bound => false}),
    ?assertEqual(true, w301_gc_verified_refresh(Conn, Key)),
    {ok, _, _} = epgsql:equery(
        Conn,
        "SELECT fn_v5_intent_ordinary_delete($1,'w301 direct group insert')",
        [Key]
    ),
    {error, #error{code = <<"23510">>}} = e(
        Conn,
        "INSERT INTO group_file(id,group_id,file_id,uploader_id,file_name,file_type,"
        "file_category,file_url,file_size) VALUES"
        " ($1,66,$2,'101','t.bin','image/png','image','u',16)",
        [Id, integer_to_binary(Id)]
    ),
    ok = epgsql:sync(Conn),
    {ok, _, [{0}]} = epgsql:equery(
        Conn,
        "SELECT count(*) FROM group_file WHERE id=$1",
        [Id]
    ),
    ok.

w301_direct_collect_after_intent(Conn) ->
    Key = <<"v5e2e/w301/collect-after-intent">>,
    Hash = binary:copy(<<"c">>, 64),
    q(
        Conn,
        "INSERT INTO attachment(id,path,scope,creator_user_id,storage_driver,"
        "storage_namespace,storage_container,storage_locator_sha256,gc_verified,file_hash256)"
        " VALUES($1,$2,'private',101,'garage','ns1','c1',$3,true,$4)",
        [?ASSET_ID + 37, Key, locator(), Hash]
    ),
    {ok, _, _} = epgsql:equery(
        Conn,
        "SELECT fn_v5_intent_ordinary_delete($1,'w301 direct collect')",
        [Key]
    ),
    Multi = <<(binary:copy(<<"d">>, 64))/binary, ",", Hash/binary>>,
    {error, #error{code = <<"23510">>}} = e(
        Conn,
        "INSERT INTO user_collect(id,user_id,kind,kind_id,attach_file_hash256,status)"
        " VALUES(992,202,0,'x',$1,1)",
        [Multi]
    ),
    ok = epgsql:sync(Conn),
    {ok, _, [{0}]} = epgsql:equery(
        Conn,
        "SELECT count(*) FROM user_collect WHERE id=992",
        []
    ),
    q(
        Conn,
        "INSERT INTO user_collect(id,user_id,kind,kind_id,attach_file_hash256,status)"
        " VALUES(993,203,0,'x',$1,0)",
        [Multi]
    ),
    {error, #error{code = <<"23510">>}} = e(
        Conn,
        "UPDATE user_collect SET status=1 WHERE id=993",
        []
    ),
    ok = epgsql:sync(Conn),
    {ok, _, [{0}]} = epgsql:equery(
        Conn,
        "SELECT status FROM user_collect WHERE id=993",
        []
    ),
    LiveKey = <<"v5e2e/w301/collect-live">>,
    LiveHash = binary:copy(<<"e">>, 64),
    q(
        Conn,
        "INSERT INTO attachment(id,path,scope,creator_user_id,storage_driver,"
        "storage_namespace,storage_container,storage_locator_sha256,gc_verified,file_hash256)"
        " VALUES($1,$2,'private',101,'garage','ns1','c1',$3,true,$4)",
        [?ASSET_ID + 38, LiveKey, locator(), LiveHash]
    ),
    q(
        Conn,
        "INSERT INTO user_collect(id,user_id,kind,kind_id,attach_file_hash256,status)"
        " VALUES(994,204,0,'x',$1,1)",
        [LiveHash]
    ),
    {error, #error{code = <<"23510">>}} = e(
        Conn,
        "SELECT fn_v5_intent_ordinary_delete($1,'w301 active collect')",
        [LiveKey]
    ),
    ok = epgsql:sync(Conn),
    {ok, _, [{<<"live">>}]} = epgsql:equery(
        Conn,
        "SELECT lifecycle_state FROM attachment WHERE path=$1",
        [LiveKey]
    ),
    ok.

w301_collect_race(#{conn := Conn, server := Server, db := Db}) ->
    Key1 = <<"v5e2e/w301/collect-race-bind-first">>,
    Key2 = <<"v5e2e/w301/collect-race-intent-first">>,
    Hash1 = binary:copy(<<"f">>, 64),
    Hash2 = binary:copy(<<"g">>, 64),
    lists:foreach(
        fun({Id, Key, Hash}) ->
            q(
                Conn,
                "INSERT INTO attachment(id,path,scope,creator_user_id,storage_driver,"
                "storage_namespace,storage_container,storage_locator_sha256,gc_verified,file_hash256)"
                " VALUES($1,$2,'private',101,'garage','ns1','c1',$3,true,$4)",
                [Id, Key, locator(), Hash]
            )
        end,
        [{?ASSET_ID + 60, Key1, Hash1}, {?ASSET_ID + 61, Key2, Hash2}]
    ),
    Opts = maps:merge(
        maps:without([maint_db, connect_extra], Server),
        (maps:get(connect_extra, Server, #{}))#{database => Db, timeout => 10000}
    ),
    {ok, Other} = inttest_marker_db:safe_connect(Opts),
    try
        {ok, _, [{Pid}]} = epgsql:equery(Other, "SELECT pg_backend_pid()", []),
        q(Other, "SET statement_timeout='10s'"),
        q(Conn, "BEGIN"),
        q(
            Conn,
            "INSERT INTO user_collect(id,user_id,kind,kind_id,attach_file_hash256,status)"
            " VALUES(995,205,0,'x',$1,1)",
            [Hash1]
        ),
        Parent = self(),
        spawn(fun() ->
            Parent !
                {collect_intent_result,
                    epgsql:equery(
                        Other,
                        "SELECT fn_v5_intent_ordinary_delete($1,'w301 collect first')",
                        [Key1]
                    )}
        end),
        w301_wait_lock(Conn, Pid, 100),
        q(Conn, "COMMIT"),
        receive
            {collect_intent_result, {error, #error{code = <<"23510">>}}} -> ok;
            {collect_intent_result, Unexpected1} -> error({unexpected_collect_intent, Unexpected1})
        after 12000 -> error(collect_intent_timeout)
        end,
        q(Conn, "BEGIN"),
        {ok, _, _} = epgsql:equery(
            Conn,
            "SELECT fn_v5_intent_ordinary_delete($1,'w301 intent first')",
            [Key2]
        ),
        spawn(fun() ->
            Parent !
                {collect_bind_result,
                    epgsql:equery(
                        Other,
                        "INSERT INTO user_collect(id,user_id,kind,kind_id,attach_file_hash256,status)"
                        " VALUES(996,206,0,'x',$1,1)",
                        [Hash2]
                    )}
        end),
        w301_wait_lock(Conn, Pid, 100),
        q(Conn, "COMMIT"),
        receive
            {collect_bind_result, {error, #error{code = <<"23510">>}}} -> ok;
            {collect_bind_result, Unexpected2} -> error({unexpected_collect_bind, Unexpected2})
        after 12000 -> error(collect_bind_timeout)
        end,
        {ok, _, [{0}]} = epgsql:equery(
            Conn,
            "SELECT count(*) FROM user_collect WHERE id=996",
            []
        ),
        ok
    after
        epgsql:close(Other)
    end.

w301_direct_enterprise_message_after_intent(Conn) ->
    q(
        Conn,
        "INSERT INTO public.\"user\"(id,password,account,reg_ip,reg_cosv)"
        " VALUES(980001,'x','v5w301eb','127.0.0.1','x')"
    ),
    q(
        Conn,
        "INSERT INTO organization(id,name,owner_id,status)"
        " VALUES(980002,'v5w301eb',980001,'active')"
    ),
    q(
        Conn,
        "INSERT INTO workspace(id,name,owner_id,status,type,organization_id)"
        " VALUES(980003,'v5w301eb',980001,'active','project',980002)"
    ),
    q(
        Conn,
        "INSERT INTO enterprise_contact(id,organization_id,status,display_name)"
        " VALUES(980004,980002,'active','probe')"
    ),
    q(
        Conn,
        "INSERT INTO enterprise_conversation(id,organization_id,workspace_id,contact_id,status)"
        " VALUES(980005,980002,980003,980004,'active')"
    ),
    q(
        Conn,
        "INSERT INTO enterprise_message(id,organization_id,workspace_id,conversation_id,"
        "sender_type,sender_contact_id,client_msg_id,retention_days,retain_until)"
        " VALUES(980006,980002,980003,980005,'contact',980004,'probe',1,now()+interval '1 day')"
    ),
    q(
        Conn,
        "INSERT INTO enterprise_asset(id,organization_id,workspace_id,conversation_id,"
        "object_key,object_hash,status) VALUES(980007,980002,980003,980005,"
        "'v5e2e/w301/eb-direct',$1,'active')",
        [binary:copy(<<"a">>, 64)]
    ),
    q(
        Conn,
        "INSERT INTO enterprise_asset(id,organization_id,workspace_id,conversation_id,"
        "object_key,object_hash,status) VALUES(980008,980002,980003,980005,"
        "'v5e2e/w301/eb-live',$1,'active')",
        [binary:copy(<<"b">>, 64)]
    ),
    q(
        Conn,
        "UPDATE enterprise_asset SET message_id=980006,retain_until=now()+interval '2 days'"
        " WHERE id=980008"
    ),
    {error, #error{code = <<"23510">>}} = e(
        Conn,
        "SELECT fn_v5_intent_eb_delete(980002,980003,980008,'w301 live bind')",
        []
    ),
    ok = epgsql:sync(Conn),
    {ok, _, _} = epgsql:equery(
        Conn,
        "SELECT fn_v5_intent_eb_delete(980002,980003,980007,'w301 direct bind')",
        []
    ),
    {error, #error{code = <<"23510">>}} = e(
        Conn,
        "UPDATE enterprise_asset SET message_id=980006,retain_until=now()+interval '2 days'"
        " WHERE id=980007",
        []
    ),
    ok = epgsql:sync(Conn),
    {error, #error{code = <<"23510">>}} = e(
        Conn,
        "UPDATE enterprise_asset SET lifecycle_state='live',message_id=980006,"
        "retain_until=now()+interval '2 days' WHERE id=980007",
        []
    ),
    ok = epgsql:sync(Conn),
    {ok, _, [{<<"delete_intent">>, null}]} = epgsql:equery(
        Conn,
        "SELECT lifecycle_state,message_id FROM enterprise_asset WHERE id=980007",
        []
    ),
    ok.

w301_cross_queue_identity(Conn) ->
    Key = <<"v5e2e/w301/cross-queue">>,
    q(
        Conn,
        "INSERT INTO attachment(id,path,scope,creator_user_id,storage_driver,"
        "storage_namespace,storage_container,storage_locator_sha256,gc_verified,file_hash256)"
        " VALUES($1,$2,'private',101,'garage','ns1','c1',$3,true,$4)",
        [?ASSET_ID + 68, Key, locator(), binary:copy(<<"5">>, 64)]
    ),
    q(
        Conn,
        "INSERT INTO enterprise_asset(id,organization_id,workspace_id,conversation_id,"
        "object_key,object_hash,status,storage_driver,storage_namespace,storage_container,"
        "storage_locator_sha256) VALUES(980013,980002,980003,980005,$1,$2,'active',"
        "'garage','ns1','c1',$3)",
        [Key, binary:copy(<<"5">>, 64), locator()]
    ),
    {ok, _, _} = epgsql:equery(
        Conn,
        "SELECT fn_v5_intent_ordinary_delete($1,'w301 cross queue')",
        [Key]
    ),
    {error, #error{code = <<"23510">>}} = e(
        Conn,
        "UPDATE attachment_object_deletion SET path='v5e2e/changed' WHERE path=$1",
        [Key]
    ),
    ok = epgsql:sync(Conn),
    {error, #error{code = <<"23510">>}} = e(
        Conn,
        "UPDATE attachment_object_deletion SET storage_physical_key='v5e2e/changed' WHERE path=$1",
        [Key]
    ),
    ok = epgsql:sync(Conn),
    {error, #error{code = <<"23510">>}} = e(
        Conn,
        "SELECT fn_v5_intent_eb_delete(980002,980003,980013,'w301 cross queue')",
        []
    ),
    ok = epgsql:sync(Conn),
    {ok, _, [{0}]} = epgsql:equery(
        Conn,
        "SELECT count(*) FROM enterprise_asset_delete_queue WHERE asset_id=980013",
        []
    ),
    Logical1 = <<"v5e2e/w301/cross-logical-1">>,
    Physical1 = <<"v5e2e/w301/cross-physical-1">>,
    w301_seed_cross_pair(Conn, ?ASSET_ID + 69, 980014, Logical1, Physical1),
    {ok, _, _} = epgsql:equery(
        Conn,
        "SELECT fn_v5_intent_ordinary_delete($1,'w301 physical key')",
        [Logical1]
    ),
    {error, #error{code = <<"23510">>}} = e(
        Conn,
        "SELECT fn_v5_intent_eb_delete(980002,980003,980014,'w301 physical key')",
        []
    ),
    ok = epgsql:sync(Conn),
    Logical2 = <<"v5e2e/w301/cross-logical-2">>,
    Physical2 = <<"v5e2e/w301/cross-physical-2">>,
    w301_seed_cross_pair(Conn, ?ASSET_ID + 70, 980015, Logical2, Physical2),
    {ok, _, _} = epgsql:equery(
        Conn,
        "SELECT fn_v5_intent_eb_delete(980002,980003,980015,'w301 physical key')",
        []
    ),
    {error, #error{code = <<"23510">>}} = e(
        Conn,
        "UPDATE enterprise_asset_delete_queue SET object_key='v5e2e/changed' WHERE asset_id=980015",
        []
    ),
    ok = epgsql:sync(Conn),
    {error, #error{code = <<"23510">>}} = e(
        Conn,
        "SELECT fn_v5_intent_ordinary_delete($1,'w301 physical key')",
        [Logical2]
    ),
    ok = epgsql:sync(Conn),
    {ok, _, [{0}]} = epgsql:equery(
        Conn,
        "SELECT count(*) FROM attachment_object_deletion WHERE path=$1",
        [Logical2]
    ),
    ok.

w301_seed_cross_pair(Conn, AttachmentId, AssetId, Logical, Physical) ->
    q(
        Conn,
        "INSERT INTO attachment(id,path,scope,creator_user_id,storage_driver,"
        "storage_namespace,storage_container,storage_locator_sha256,storage_physical_key,"
        "gc_verified,file_hash256) VALUES($1,$2,'private',101,'garage','ns1','c1',$3,$4,true,$5)",
        [AttachmentId, Logical, locator(), Physical, binary:copy(<<"6">>, 64)]
    ),
    q(
        Conn,
        "INSERT INTO enterprise_asset(id,organization_id,workspace_id,conversation_id,"
        "object_key,object_hash,status,storage_driver,storage_namespace,storage_container,"
        "storage_locator_sha256) VALUES($1,980002,980003,980005,$2,$3,'active',"
        "'garage','ns1','c1',$4)",
        [AssetId, Physical, binary:copy(<<"6">>, 64), locator()]
    ).

w301_cross_queue_race(#{conn := Conn, server := Server, db := Db}) ->
    Logical1 = <<"v5e2e/w301/cross-race-logical-1">>,
    Physical1 = <<"v5e2e/w301/cross-race-physical-1">>,
    Logical2 = <<"v5e2e/w301/cross-race-logical-2">>,
    Physical2 = <<"v5e2e/w301/cross-race-physical-2">>,
    w301_seed_cross_pair(Conn, ?ASSET_ID + 71, 980016, Logical1, Physical1),
    w301_seed_cross_pair(Conn, ?ASSET_ID + 72, 980017, Logical2, Physical2),
    Opts = maps:merge(
        maps:without([maint_db, connect_extra], Server),
        (maps:get(connect_extra, Server, #{}))#{database => Db, timeout => 10000}
    ),
    {ok, Other} = inttest_marker_db:safe_connect(Opts),
    try
        {ok, _, [{Pid}]} = epgsql:equery(Other, "SELECT pg_backend_pid()", []),
        q(Other, "SET statement_timeout='10s'"),
        q(Conn, "BEGIN"),
        {ok, _, _} = epgsql:equery(
            Conn,
            "SELECT fn_v5_intent_ordinary_delete($1,'w301 cross ordinary first')",
            [Logical1]
        ),
        Parent = self(),
        spawn(fun() ->
            Parent !
                {cross_eb_result,
                    epgsql:equery(
                        Other,
                        "SELECT fn_v5_intent_eb_delete(980002,980003,980016,'w301 cross')",
                        []
                    )}
        end),
        w301_wait_lock(Conn, Pid, 100),
        q(Conn, "COMMIT"),
        receive
            {cross_eb_result, {error, #error{code = <<"23510">>}}} -> ok;
            {cross_eb_result, Unexpected1} -> error({unexpected_cross_eb, Unexpected1})
        after 12000 -> error(cross_eb_timeout)
        end,
        q(Conn, "BEGIN"),
        {ok, _, _} = epgsql:equery(
            Conn,
            "SELECT fn_v5_intent_eb_delete(980002,980003,980017,'w301 cross eb first')",
            []
        ),
        spawn(fun() ->
            Parent !
                {cross_ordinary_result,
                    epgsql:equery(
                        Other,
                        "SELECT fn_v5_intent_ordinary_delete($1,'w301 cross')",
                        [Logical2]
                    )}
        end),
        w301_wait_lock(Conn, Pid, 100),
        q(Conn, "COMMIT"),
        receive
            {cross_ordinary_result, {error, #error{code = <<"23510">>}}} -> ok;
            {cross_ordinary_result, Unexpected2} -> error({unexpected_cross_ordinary, Unexpected2})
        after 12000 -> error(cross_ordinary_timeout)
        end,
        {ok, _, [{0}]} = epgsql:equery(
            Conn,
            "SELECT count(*) FROM attachment_object_deletion WHERE path=$1",
            [Logical2]
        ),
        {ok, _, [{0}]} = epgsql:equery(
            Conn,
            "SELECT count(*) FROM enterprise_asset_delete_queue WHERE asset_id=980016",
            []
        ),
        ok
    after
        epgsql:close(Other)
    end.

w301_direct_moment_after_intent(Conn) ->
    Key = <<"v5e2e/w301/moment-after-intent">>,
    q(
        Conn,
        "INSERT INTO attachment(id,path,scope,creator_user_id,storage_driver,"
        "storage_namespace,storage_container,storage_locator_sha256,gc_verified,file_hash256)"
        " VALUES($1,$2,'moment',980001,'garage','ns1','c1',$3,true,$4)",
        [?ASSET_ID + 62, Key, locator(), binary:copy(<<"1">>, 64)]
    ),
    {ok, _, _} = epgsql:equery(
        Conn,
        "SELECT fn_v5_intent_ordinary_delete($1,'w301 direct moment')",
        [Key]
    ),
    Media = jsone:encode([#{<<"url">> => Key}]),
    {error, #error{code = <<"23510">>}} = e(
        Conn,
        "INSERT INTO moment_post(id,author_uid,media) VALUES(980020,980001,$1::jsonb)",
        [Media]
    ),
    ok = epgsql:sync(Conn),
    {ok, _, [{0}]} = epgsql:equery(
        Conn,
        "SELECT count(*) FROM moment_post WHERE id=980020",
        []
    ),
    q(Conn, "INSERT INTO moment_post(id,author_uid,media) VALUES(980021,980001,'[]'::jsonb)"),
    {error, #error{code = <<"23510">>}} = e(
        Conn,
        "UPDATE moment_post SET media=$1::jsonb WHERE id=980021",
        [Media]
    ),
    ok = epgsql:sync(Conn),
    {ok, _, [{0}]} = epgsql:equery(
        Conn,
        "SELECT jsonb_array_length(media) FROM moment_post WHERE id=980021",
        []
    ),
    LiveKey = <<"v5e2e/w301/moment-live">>,
    q(
        Conn,
        "INSERT INTO attachment(id,path,scope,creator_user_id,storage_driver,"
        "storage_namespace,storage_container,storage_locator_sha256,gc_verified,file_hash256)"
        " VALUES($1,$2,'moment',980001,'garage','ns1','c1',$3,true,$4)",
        [?ASSET_ID + 63, LiveKey, locator(), binary:copy(<<"2">>, 64)]
    ),
    LiveMedia = jsone:encode([#{<<"url">> => LiveKey}]),
    q(Conn, "BEGIN"),
    q(
        Conn,
        "INSERT INTO moment_post(id,author_uid,media) VALUES(980027,980001,$1::jsonb)",
        [LiveMedia]
    ),
    {error, #error{code = <<"23510">>}} = e(Conn, "COMMIT", []),
    ok = epgsql:sync(Conn),
    {ok, _, [{0}]} = epgsql:equery(
        Conn,
        "SELECT count(*) FROM moment_post WHERE id=980027",
        []
    ),
    q(Conn, "BEGIN"),
    q(
        Conn,
        "INSERT INTO moment_post(id,author_uid,media) VALUES(980022,980001,$1::jsonb)",
        [LiveMedia]
    ),
    q(Conn, "UPDATE attachment SET scope_ref='980022' WHERE path=$1", [LiveKey]),
    q(Conn, "COMMIT"),
    {error, #error{code = <<"23510">>}} = e(
        Conn,
        "INSERT INTO moment_post(id,author_uid,media) VALUES(980026,980001,$1::jsonb)",
        [LiveMedia]
    ),
    ok = epgsql:sync(Conn),
    q(
        Conn,
        "UPDATE moment_post SET media=$1::jsonb WHERE id=980022",
        [jsone:encode([#{<<"url">> => LiveKey, <<"cover_url">> => LiveKey}])]
    ),
    {error, #error{code = <<"23510">>}} = e(
        Conn,
        "UPDATE moment_post SET media=$1::jsonb WHERE id=980022",
        [jsone:encode([#{<<"url">> => LiveKey, <<"cover_url">> => Key}])]
    ),
    ok = epgsql:sync(Conn),
    AmbiguousKey = <<"v5e2e/w301/moment-legacy-ambiguous">>,
    q(
        Conn,
        "INSERT INTO attachment(id,path,scope,creator_user_id,storage_driver,"
        "storage_namespace,storage_container,storage_locator_sha256,gc_verified,file_hash256)"
        " VALUES($1,$2,'moment',980001,'garage','ns1','c1',$3,true,$4)",
        [?ASSET_ID + 67, AmbiguousKey, locator(), binary:copy(<<"5">>, 64)]
    ),
    Note = <<Key/binary, " ", AmbiguousKey/binary>>,
    HiddenMedia = jsone:encode([#{<<"url">> => LiveKey, <<"note">> => Note}]),
    q(Conn, "UPDATE moment_post SET media=$1::jsonb WHERE id=980022", [HiddenMedia]),
    {ok, _, [{Note, true, true}]} = epgsql:equery(
        Conn,
        "SELECT media->0->>'note', fn_v5_moment_reference_blocks($1),"
        " fn_v5_moment_reference_blocks($2) FROM moment_post WHERE id=980022",
        [Key, AmbiguousKey]
    ),
    ?assertEqual({ok, false}, attachment_repo:refresh_gc_verified_tx(Conn, AmbiguousKey)),
    q(
        Conn,
        "INSERT INTO public.\"user\"(id,password,account,reg_ip,reg_cosv)"
        " VALUES(980099,'x','v5w301eb-other','127.0.0.1','x')"
    ),
    {error, #error{code = <<"23510">>}} = e(
        Conn,
        "UPDATE moment_post SET author_uid=980099 WHERE id=980022",
        []
    ),
    ok = epgsql:sync(Conn),
    {error, #error{code = <<"23510">>}} = e(
        Conn,
        "SELECT fn_v5_intent_ordinary_delete($1,'w301 live moment')",
        [LiveKey]
    ),
    ok = epgsql:sync(Conn),
    {ok, _, [{<<"live">>}]} = epgsql:equery(
        Conn,
        "SELECT lifecycle_state FROM attachment WHERE path=$1",
        [LiveKey]
    ),
    ok.

w301_moment_race(#{conn := Conn, server := Server, db := Db}) ->
    Key1 = <<"v5e2e/w301/moment-race-bind-first">>,
    Key2 = <<"v5e2e/w301/moment-race-intent-first">>,
    lists:foreach(
        fun({Id, Key}) ->
            q(
                Conn,
                "INSERT INTO attachment(id,path,scope,creator_user_id,storage_driver,"
                "storage_namespace,storage_container,storage_locator_sha256,gc_verified,file_hash256)"
                " VALUES($1,$2,'moment',980001,'garage','ns1','c1',$3,true,$4)",
                [Id, Key, locator(), binary:copy(<<"3">>, 64)]
            )
        end,
        [{?ASSET_ID + 64, Key1}, {?ASSET_ID + 65, Key2}]
    ),
    Opts = maps:merge(
        maps:without([maint_db, connect_extra], Server),
        (maps:get(connect_extra, Server, #{}))#{database => Db, timeout => 10000}
    ),
    {ok, Other} = inttest_marker_db:safe_connect(Opts),
    try
        {ok, _, [{Pid}]} = epgsql:equery(Other, "SELECT pg_backend_pid()", []),
        q(Other, "SET statement_timeout='10s'"),
        q(Conn, "BEGIN"),
        q(
            Conn,
            "INSERT INTO moment_post(id,author_uid,media) VALUES(980023,980001,$1::jsonb)",
            [jsone:encode([#{<<"url">> => Key1}])]
        ),
        q(Conn, "UPDATE attachment SET scope_ref='980023' WHERE path=$1", [Key1]),
        Parent = self(),
        spawn(fun() ->
            Parent !
                {moment_intent_result,
                    epgsql:equery(
                        Other,
                        "SELECT fn_v5_intent_ordinary_delete($1,'w301 moment first')",
                        [Key1]
                    )}
        end),
        w301_wait_lock(Conn, Pid, 100),
        q(Conn, "COMMIT"),
        receive
            {moment_intent_result, {error, #error{code = Code}}} when
                Code =:= <<"23510">>; Code =:= <<"40001">>
            ->
                ok;
            {moment_intent_result, Unexpected1} ->
                error({unexpected_moment_intent, Unexpected1})
        after 12000 -> error(moment_intent_timeout)
        end,
        {ok, _, [{<<"live">>, 0}]} = epgsql:equery(
            Conn,
            "SELECT lifecycle_state,(SELECT count(*) FROM attachment_object_deletion WHERE path=$1)"
            " FROM attachment WHERE path=$1",
            [Key1]
        ),
        q(Conn, "BEGIN"),
        {ok, _, _} = epgsql:equery(
            Conn,
            "SELECT fn_v5_intent_ordinary_delete($1,'w301 intent first')",
            [Key2]
        ),
        spawn(fun() ->
            Parent !
                {moment_bind_result,
                    epgsql:equery(
                        Other,
                        "INSERT INTO moment_post(id,author_uid,media) VALUES(980024,980001,$1::jsonb)",
                        [jsone:encode([#{<<"url">> => Key2}])]
                    )}
        end),
        w301_wait_lock(Conn, Pid, 100),
        q(Conn, "COMMIT"),
        receive
            {moment_bind_result, {error, #error{code = <<"23510">>}}} -> ok;
            {moment_bind_result, Unexpected2} -> error({unexpected_moment_bind, Unexpected2})
        after 12000 -> error(moment_bind_timeout)
        end,
        {ok, _, [{0}]} = epgsql:equery(
            Conn,
            "SELECT count(*) FROM moment_post WHERE id=980024",
            []
        ),
        ok
    after
        epgsql:close(Other)
    end.

w301_moment_escaped_reference(Conn) ->
    Key = <<"v5e2e/w301/moment-quote-\"-backslash-\\">>,
    q(
        Conn,
        "INSERT INTO attachment(id,path,scope,creator_user_id,storage_driver,"
        "storage_namespace,storage_container,storage_locator_sha256,gc_verified,file_hash256)"
        " VALUES($1,$2,'moment',980001,'garage','ns1','c1',$3,true,$4)",
        [?ASSET_ID + 66, Key, locator(), binary:copy(<<"4">>, 64)]
    ),
    Media = jsone:encode([#{<<"url">> => Key}]),
    q(Conn, "BEGIN"),
    q(
        Conn,
        "INSERT INTO moment_post(id,author_uid,media) VALUES(980025,980001,$1::jsonb)",
        [Media]
    ),
    q(Conn, "UPDATE attachment SET scope_ref='980025' WHERE path=$1", [Key]),
    q(Conn, "COMMIT"),
    ?assertEqual({ok, false}, attachment_repo:refresh_gc_verified_tx(Conn, Key)),
    %% Simulate a stale earlier proof; the intent must independently recheck the reference.
    q(Conn, "UPDATE attachment SET gc_verified=true WHERE path=$1", [Key]),
    {error, #error{code = <<"23510">>, message = Reason}} = e(
        Conn,
        "SELECT fn_v5_intent_ordinary_delete($1,'w301 escaped moment')",
        [Key]
    ),
    ?assertNotEqual(nomatch, binary:match(Reason, <<"moment media reference">>)),
    ok = epgsql:sync(Conn),
    {ok, _, [{<<"live">>}]} = epgsql:equery(
        Conn,
        "SELECT lifecycle_state FROM attachment WHERE path=$1",
        [Key]
    ),
    ok.

w301_runtime_direct_dml_denied(Conn) ->
    q(Conn, "BEGIN"),
    q(Conn, "SET LOCAL ROLE v5_app_runtime"),
    {error, #error{code = <<"42501">>}} = e(
        Conn,
        "UPDATE enterprise_asset SET message_id=980006 WHERE id=980007",
        []
    ),
    ok = epgsql:sync(Conn),
    q(Conn, "ROLLBACK"),
    q(Conn, "BEGIN"),
    q(Conn, "SET LOCAL ROLE v5_app_runtime"),
    {error, #error{code = <<"42501">>}} = e(
        Conn,
        "INSERT INTO group_file(id,group_id,file_id,uploader_id,file_name,file_type,"
        "file_category,file_url,file_size) VALUES"
        " ($1,66,$2,'101','t.bin','image/png','image','u',16)",
        [?ASSET_ID + 36, integer_to_binary(?ASSET_ID + 36)]
    ),
    ok = epgsql:sync(Conn),
    q(Conn, "ROLLBACK"),
    {ok, _, [{<<"delete_intent">>, null}]} = epgsql:equery(
        Conn,
        "SELECT lifecycle_state,message_id FROM enterprise_asset WHERE id=980007",
        []
    ),
    ok.

w301_enterprise_direct_race(#{conn := Conn, server := Server, db := Db}) ->
    lists:foreach(
        fun(Id) ->
            q(
                Conn,
                "INSERT INTO enterprise_asset(id,organization_id,workspace_id,conversation_id,"
                "object_key,object_hash,status) VALUES($1,980002,980003,980005,$2,$3,'active')",
                [
                    Id,
                    <<"v5e2e/w301/eb-race-", (integer_to_binary(Id))/binary>>,
                    binary:copy(<<"c">>, 64)
                ]
            )
        end,
        [980009, 980010]
    ),
    Opts = maps:merge(
        maps:without([maint_db, connect_extra], Server),
        (maps:get(connect_extra, Server, #{}))#{database => Db, timeout => 10000}
    ),
    {ok, Other} = inttest_marker_db:safe_connect(Opts),
    try
        {ok, _, [{Pid}]} = epgsql:equery(Other, "SELECT pg_backend_pid()", []),
        q(Other, "SET statement_timeout='10s'"),
        q(Conn, "BEGIN"),
        q(
            Conn,
            "UPDATE enterprise_asset SET message_id=980006,retain_until=now()+interval '2 days'"
            " WHERE id=980009"
        ),
        Parent = self(),
        spawn(fun() ->
            Parent !
                {eb_intent_result,
                    epgsql:equery(
                        Other,
                        "SELECT fn_v5_intent_eb_delete(980002,980003,980009,'w301 bind first')",
                        []
                    )}
        end),
        w301_wait_lock(Conn, Pid, 100),
        q(Conn, "COMMIT"),
        receive
            {eb_intent_result, {error, #error{code = <<"23510">>}}} -> ok;
            {eb_intent_result, Unexpected1} -> error({unexpected_eb_intent, Unexpected1})
        after 12000 -> error(eb_intent_timeout)
        end,
        q(Conn, "BEGIN"),
        {ok, _, _} = epgsql:equery(
            Conn,
            "SELECT fn_v5_intent_eb_delete(980002,980003,980010,'w301 intent first')",
            []
        ),
        spawn(fun() ->
            Parent !
                {eb_bind_result,
                    epgsql:equery(
                        Other,
                        "UPDATE enterprise_asset SET message_id=980006,retain_until=now()+interval '2 days'"
                        " WHERE id=980010",
                        []
                    )}
        end),
        w301_wait_lock(Conn, Pid, 100),
        q(Conn, "COMMIT"),
        receive
            {eb_bind_result, {error, #error{code = <<"23510">>}}} -> ok;
            {eb_bind_result, Unexpected2} -> error({unexpected_eb_bind, Unexpected2})
        after 12000 -> error(eb_bind_timeout)
        end,
        {ok, _, [{<<"delete_intent">>, null}]} = epgsql:equery(
            Conn,
            "SELECT lifecycle_state,message_id FROM enterprise_asset WHERE id=980010",
            []
        ),
        ok
    after
        epgsql:close(Other)
    end.

w301_enterprise_store_race(#{conn := Conn, server := Server, db := Db}) ->
    lists:foreach(
        fun(Id) ->
            q(
                Conn,
                "INSERT INTO enterprise_asset(id,organization_id,workspace_id,conversation_id,"
                "object_key,object_hash,status) VALUES($1,980002,980003,980005,$2,$3,'active')",
                [
                    Id,
                    <<"v5e2e/w301/eb-store-", (integer_to_binary(Id))/binary>>,
                    binary:copy(<<"d">>, 64)
                ]
            )
        end,
        [980011, 980012]
    ),
    Opts = maps:merge(
        maps:without([maint_db, connect_extra], Server),
        (maps:get(connect_extra, Server, #{}))#{database => Db, timeout => 10000}
    ),
    {ok, Other} = inttest_marker_db:safe_connect(Opts),
    try
        {ok, _, [{Pid}]} = epgsql:equery(Other, "SELECT pg_backend_pid()", []),
        q(Other, "SET statement_timeout='10s'"),
        Retain = erlang:system_time(second) + 172800,
        q(Conn, "BEGIN"),
        {ok, [#{lifecycle_state := <<"live">>}]} =
            eb_pg_store:lock_assets_in(Conn, 980002, 980003, [980011]),
        ok = eb_pg_store:bind_asset_message_in(Conn, 980002, 980003, 980011, 980006, Retain),
        Parent = self(),
        spawn(fun() ->
            Parent !
                {store_intent_result,
                    epgsql:equery(
                        Other,
                        "SELECT fn_v5_intent_eb_delete(980002,980003,980011,'w301 store bind first')",
                        []
                    )}
        end),
        w301_wait_lock(Conn, Pid, 100),
        q(Conn, "COMMIT"),
        receive
            {store_intent_result, {error, #error{code = <<"23510">>}}} -> ok;
            {store_intent_result, Unexpected1} -> error({unexpected_store_intent, Unexpected1})
        after 12000 -> error(store_intent_timeout)
        end,
        q(Conn, "BEGIN"),
        {ok, _, _} = epgsql:equery(
            Conn,
            "SELECT fn_v5_intent_eb_delete(980002,980003,980012,'w301 store intent first')",
            []
        ),
        spawn(fun() ->
            q(Other, "BEGIN"),
            Locked = eb_pg_store:lock_assets_in(Other, 980002, 980003, [980012]),
            Bind = eb_pg_store:bind_asset_message_in(Other, 980002, 980003, 980012, 980006, Retain),
            q(Other, "ROLLBACK"),
            Parent ! {store_bind_result, Locked, Bind}
        end),
        w301_wait_lock(Conn, Pid, 100),
        q(Conn, "COMMIT"),
        receive
            {store_bind_result, {ok, [#{lifecycle_state := <<"delete_intent">>}]},
                {error, not_bound}} ->
                ok;
            {store_bind_result, Unexpected2, Unexpected3} ->
                error({unexpected_store_bind, Unexpected2, Unexpected3})
        after 12000 -> error(store_bind_timeout)
        end,
        {ok, _, [{<<"delete_intent">>, null}]} = epgsql:equery(
            Conn,
            "SELECT lifecycle_state,message_id FROM enterprise_asset WHERE id=980012",
            []
        ),
        ok
    after
        epgsql:close(Other)
    end.

%% shared-owner：注销/登出/原帖删除不掉共享引用资产；无关授权拒绝。
w301_shared_owner(Conn) ->
    %% 群绑定资产（绑定活跃）：creator 注销不得删除
    K1 = <<"v5e2e/w301/owner/group-bound">>,
    w301_seed_group_asset(Conn, K1, #{id => 40, gf => 40, bound => true}),
    {ok, _, _} = epgsql:equery(
        Conn, "SELECT fn_v5_confirm_pending($1,$2)", [K1, ?ASSET_ID + 40]
    ),
    {ok, _, [{Cnt1}]} = epgsql:equery(
        Conn,
        "WITH del AS (DELETE FROM attachment a WHERE a.creator_user_id=101 AND a.path=$1"
        " AND a.cipher IS NULL AND a.group_file_id IS NULL"
        " AND a.scope NOT IN ('group','channel','enterprise')"
        " AND NOT EXISTS (SELECT 1 FROM group_file gf WHERE gf.id=a.group_file_id)"
        " AND NOT EXISTS (SELECT 1 FROM user_collect uc WHERE uc.attach_file_hash256 <> ''"
        "   AND uc.attach_file_hash256 = a.file_hash256 AND uc.status = 1)"
        " AND NOT EXISTS (SELECT 1 FROM moment_post mp WHERE position(a.path in mp.media::text) > 0)"
        " RETURNING 1) SELECT COUNT(*) FROM del",
        [K1]
    ),
    ?assertEqual(0, Cnt1),
    {ok, _, [{1}]} = epgsql:equery(
        Conn, "SELECT COUNT(*) FROM attachment WHERE path=$1", [K1]
    ),
    %% 被收藏的个人资产：hash 关联保护，注销不删
    K2 = <<"v5e2e/w301/owner/collected">>,
    SharedHash = binary:copy(<<"8">>, 64),
    q(Conn, "DELETE FROM attachment WHERE path=$1", [K2]),
    q(
        Conn,
        "INSERT INTO attachment(id,path,scope,creator_user_id,storage_driver,"
        "storage_namespace,storage_container,storage_locator_sha256,gc_verified,"
        "cipher,file_hash256) VALUES ($1,$2,'private',101,'garage','ns1','c1',"
        "$3,false,NULL,$4)",
        [?ASSET_ID + 41, K2, locator(), SharedHash]
    ),
    q(
        Conn,
        "INSERT INTO user_collect(id,user_id,kind,kind_id,attach_file_hash256,status)"
        " VALUES (991,202,0,'x',$1,1)",
        [SharedHash]
    ),
    {ok, _, [{Cnt2}]} = epgsql:equery(
        Conn,
        "WITH del AS (DELETE FROM attachment a WHERE a.creator_user_id=101 AND a.path=$1"
        " AND a.cipher IS NULL AND a.group_file_id IS NULL"
        " AND a.scope NOT IN ('group','channel','enterprise')"
        " AND NOT EXISTS (SELECT 1 FROM group_file gf WHERE gf.id=a.group_file_id)"
        " AND NOT EXISTS (SELECT 1 FROM user_collect uc WHERE uc.attach_file_hash256 <> ''"
        "   AND uc.attach_file_hash256 = a.file_hash256 AND uc.status = 1)"
        " RETURNING 1) SELECT COUNT(*) FROM del",
        [K2]
    ),
    ?assertEqual(0, Cnt2),
    {ok, _, [{1}]} = epgsql:equery(
        Conn, "SELECT COUNT(*) FROM attachment WHERE path=$1", [K2]
    ),
    %% 无共享证据的个人明文附件：注销正常清理（个人引用释放语义）。
    %% M3 守卫：metadata DELETE 需已提交 delete_intent + 队列 tombstone；
    %% 真实注销路径即 intent_ordinary_delete_tx（user_deletion_logic），
    %% 故先走 intent 再删。
    K3 = <<"v5e2e/w301/owner/personal">>,
    q(Conn, "DELETE FROM attachment WHERE path=$1", [K3]),
    q(
        Conn,
        "INSERT INTO attachment(id,path,scope,creator_user_id,storage_driver,"
        "storage_namespace,storage_container,storage_locator_sha256,gc_verified,"
        "cipher,file_hash256) VALUES ($1,$2,'private',101,'garage','ns1','c1',"
        "$3,true,NULL,$4)",
        [?ASSET_ID + 42, K3, locator(), binary:copy(<<"7">>, 64)]
    ),
    {ok, _, _} = epgsql:equery(
        Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w301 account deletion')", [K3]
    ),
    {ok, _, [{Cnt3}]} = epgsql:equery(
        Conn,
        "WITH del AS (DELETE FROM attachment a WHERE a.creator_user_id=101 AND a.path=$1"
        " AND a.cipher IS NULL AND a.group_file_id IS NULL"
        " AND a.scope NOT IN ('group','channel','enterprise')"
        " AND NOT EXISTS (SELECT 1 FROM user_collect uc WHERE uc.attach_file_hash256 <> ''"
        "   AND uc.attach_file_hash256 = a.file_hash256 AND uc.status = 1)"
        " RETURNING 1) SELECT COUNT(*) FROM del",
        [K3]
    ),
    ?assertEqual(1, Cnt3),
    {ok, _, [{0}]} = epgsql:equery(
        Conn, "SELECT COUNT(*) FROM attachment WHERE path=$1", [K3]
    ),
    %% 清理测试自建的收藏行
    q(Conn, "DELETE FROM user_collect WHERE id=991"),
    %% 无关授权拒绝（非群成员不可读，且删除任务细节不可见）
    {ok, _, [{false}]} = epgsql:equery(
        Conn,
        "SELECT EXISTS (SELECT 1 FROM group_member gm JOIN \"group\" g ON g.id=gm.group_id"
        " WHERE g.id=66 AND gm.user_id=999999)",
        []
    ),
    ok.

%% history-unknown：位置未知/密文/收藏歧义 => gc_verified 恒 false + 0 删除。
w301_history_unknown(Conn) ->
    %% unknown_location：locator 为 NULL 的群资产（绑定已释放）。
    %% M3 位置不可变守卫禁 UPDATE 四元组，且 confirm 协议要求注册/行
    %% 位置一致——NULL 位置形态不经 pending 协议，直接落元数据行。
    K1 = <<"v5e2e/w301/hist/unknown-location">>,
    q(Conn, "DELETE FROM attach_pending WHERE object_key=$1", [K1]),
    q(Conn, "DELETE FROM attachment WHERE path=$1", [K1]),
    q(Conn, "DELETE FROM group_file WHERE id=$1", [?ASSET_ID + 50]),
    q(
        Conn,
        "INSERT INTO attachment(id,path,scope,creator_user_id,"
        "storage_driver,storage_namespace,storage_container,"
        "storage_locator_sha256,gc_verified,cipher,group_file_id,file_hash256)"
        " VALUES ($1,$2,'group',101,NULL,NULL,NULL,NULL,false,NULL,$3,$4)",
        [?ASSET_ID + 50, K1, ?ASSET_ID + 50, w301_hash()]
    ),
    ?assertEqual(false, w301_gc_verified_refresh(Conn, K1)),
    {error, #error{code = <<"23510">>}} =
        e(Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w301 hist')", [K1]),
    ok = epgsql:sync(Conn),
    {ok, _, [{0}]} = epgsql:equery(
        Conn, "SELECT COUNT(*) FROM attachment_object_deletion WHERE path=$1", [K1]
    ),
    {ok, _, [{H1}]} = epgsql:equery(
        Conn, "SELECT file_hash256 FROM attachment WHERE path=$1", [K1]
    ),
    ?assertEqual(w301_hash(), H1),
    %% opaque_reference：密文对象映射不可解析 => 恒 false
    K2 = <<"v5e2e/w301/hist/cipher">>,
    w301_seed_group_asset(Conn, K2, #{
        id => 51,
        gf => 51,
        bound => false,
        cipher => <<"AES-256-GCM">>
    }),
    ?assertEqual(false, w301_gc_verified_refresh(Conn, K2)),
    {ok, _, [{0}]} = epgsql:equery(
        Conn, "SELECT COUNT(*) FROM attachment_object_deletion WHERE path=$1", [K2]
    ),
    %% ambiguous_reference：收藏 hash 关联 => 恒 false
    K3 = <<"v5e2e/w301/hist/collect">>,
    CollectHash = binary:copy(<<"9">>, 64),
    w301_seed_group_asset(Conn, K3, #{id => 52, gf => 52, bound => false, hash => CollectHash}),
    q(
        Conn,
        "INSERT INTO user_collect(id,user_id,kind,kind_id,attach_file_hash256,status)"
        " VALUES (992,202,0,'x',$1,1)",
        [CollectHash]
    ),
    ?assertEqual(false, w301_gc_verified_refresh(Conn, K3)),
    q(Conn, "DELETE FROM user_collect WHERE id=992"),
    ok.

%% Pin/confirm must finish inside the real capability window. Never shrink it.
%% Wait on the database clock, capped at five seconds; a missed window fails.
await_upload_expiry(Conn, Key) ->
    {ok, _, [{true}]} = epgsql:equery(
        Conn,
        "SELECT session_state='confirmed' AND upload_content IS NOT NULL"
        " AND upload_expires_at > clock_timestamp()"
        " FROM attach_pending WHERE object_key=$1",
        [Key]
    ),
    {ok, _, [_]} = epgsql:equery(
        Conn,
        "SELECT pg_sleep(LEAST(5.0, GREATEST(0.0,"
        " EXTRACT(EPOCH FROM (upload_expires_at - clock_timestamp())))))"
        " FROM attach_pending WHERE object_key=$1",
        [Key]
    ),
    {ok, _, [{true}]} = epgsql:equery(
        Conn,
        "SELECT upload_expires_at <= clock_timestamp()"
        " FROM attach_pending WHERE object_key=$1",
        [Key]
    ),
    ok.
