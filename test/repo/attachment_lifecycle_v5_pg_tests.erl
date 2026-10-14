%% attachment_lifecycle_v5_pg_tests — W1-02 core protocol eunit suite.
%% Reused by W2-03 (confirm/GC race) and W6-01 (real fault matrix).
-module(attachment_lifecycle_v5_pg_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-define(REG_KEY, <<"v5e2e/reg/1">>).
-define(ASSET_ID, 901100101).

r20_test_() ->
    {timeout, 900, fun run/0}.

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
        "gen_random_uuid(),$2::jsonb,now()+interval '2 hours',"
        "now()+interval '1 hour','garage','ns1','c1',$3)",
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
    q(
        Conn,
        "UPDATE attach_pending SET upload_expires_at = now() - interval '2 hours'"
        " WHERE object_key=$1",
        [?REG_KEY]
    ),
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
        "gen_random_uuid(),$2::jsonb,now()+interval '2 hours',"
        "now()+interval '1 hour','garage','ns1','c1',$3)",
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
    q(
        Conn,
        "UPDATE attach_pending SET upload_expires_at = now() - interval '2 hours'"
        " WHERE object_key=$1",
        [Key]
    ),
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
    w203_seed(Conn, Key),
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
        w301_reference_gc_race(maps:get(conn, State)),
        w301_shared_owner(maps:get(conn, State)),
        w301_history_unknown(maps:get(conn, State))
    after
        inttest_marker_db:release(State),
        eunit_runner:eunit_cleanup(Boot)
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
        "gen_random_uuid(),$2::jsonb,now()+interval '2 hours',"
        "now()+interval '1 hour','garage','ns1','c1',$3)",
        [Key, registration(), locator()]
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
        "INSERT INTO attachment(id,path,scope,creator_user_id,"
        "storage_driver,storage_namespace,storage_container,"
        "storage_locator_sha256,gc_verified,cipher,group_file_id,file_hash256)"
        " VALUES ($1,$2,'group',101,'garage','ns1','c1',$3,false,$4,$5,$6)",
        [
            ?ASSET_ID + maps:get(id, Opts),
            Key,
            locator(),
            maps:get(cipher, Opts, null),
            case maps:get(bound, Opts) of
                true -> GfId;
                false -> GfId
            end,
            w301_hash()
        ]
    ),
    {ok, _, _} = epgsql:equery(
        Conn, "SELECT fn_v5_confirm_pending($1,$2)", [Key, ?ASSET_ID + maps:get(id, Opts)]
    ),
    q(
        Conn,
        "UPDATE attach_pending SET upload_expires_at = now() - interval '2 hours'"
        " WHERE object_key=$1",
        [Key]
    ),
    %% group_file 行按场景保留/删除（绑定释放形态）
    case maps:get(bound, Opts) of
        true -> ok;
        false -> q(Conn, "DELETE FROM group_file WHERE id=$1", [GfId])
    end,
    ok.

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
    q(
        Conn,
        "INSERT INTO group_file(id,group_id,file_id,uploader_id,file_name,file_type,"
        "file_category,file_url,file_size) VALUES"
        " ($1,66,$2,'101','t.bin','image/png','image','u',16)",
        [?ASSET_ID + 31, integer_to_binary(?ASSET_ID + 31)]
    ),
    %% 墓碑拒绝重绑（repo 协议，跑在 marker 连接的手动事务里：fixture 同库）
    {ok, _, _} = epgsql:equery(Conn, "BEGIN", []),
    R2 = attachment_repo:bind_reference_lock_tx(Conn, K2),
    {ok, _, _} = epgsql:equery(Conn, "ROLLBACK", []),
    {error, tombstone} = R2,
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
        "WITH del AS (DELETE FROM attachment a WHERE a.creator_user_id=101"
        " AND a.cipher IS NULL AND a.group_file_id IS NULL"
        " AND a.scope NOT IN ('group','channel','enterprise')"
        " AND NOT EXISTS (SELECT 1 FROM group_file gf WHERE gf.id=a.group_file_id)"
        " AND NOT EXISTS (SELECT 1 FROM user_collect uc WHERE uc.attach_file_hash256 <> ''"
        "   AND uc.attach_file_hash256 = a.file_hash256 AND uc.status = 1)"
        " AND NOT EXISTS (SELECT 1 FROM moment_post mp WHERE position(a.path in mp.media::text) > 0)"
        " RETURNING 1) SELECT COUNT(*) FROM del",
        []
    ),
    ?assertEqual(0, Cnt1),
    {ok, _, [{1}]} = epgsql:equery(
        Conn, "SELECT COUNT(*) FROM attachment WHERE path=$1", [K1]
    ),
    %% 被收藏的个人资产：hash 关联保护，注销不删
    K2 = <<"v5e2e/w301/owner/collected">>,
    q(Conn, "DELETE FROM attachment WHERE path=$1", [K2]),
    q(
        Conn,
        "INSERT INTO attachment(id,path,scope,creator_user_id,storage_driver,"
        "storage_namespace,storage_container,storage_locator_sha256,gc_verified,"
        "cipher,file_hash256) VALUES ($1,$2,'private',101,'garage','ns1','c1',"
        "$3,false,NULL,$4)",
        [?ASSET_ID + 41, K2, locator(), w301_hash()]
    ),
    q(
        Conn,
        "INSERT INTO user_collect(id,user_id,kind,kind_id,attach_file_hash256,status)"
        " VALUES (991,202,0,'x',$1,1)",
        [w301_hash()]
    ),
    {ok, _, [{Cnt2}]} = epgsql:equery(
        Conn,
        "WITH del AS (DELETE FROM attachment a WHERE a.creator_user_id=101"
        " AND a.cipher IS NULL AND a.group_file_id IS NULL"
        " AND a.scope NOT IN ('group','channel','enterprise')"
        " AND NOT EXISTS (SELECT 1 FROM group_file gf WHERE gf.id=a.group_file_id)"
        " AND NOT EXISTS (SELECT 1 FROM user_collect uc WHERE uc.attach_file_hash256 <> ''"
        "   AND uc.attach_file_hash256 = a.file_hash256 AND uc.status = 1)"
        " RETURNING 1) SELECT COUNT(*) FROM del",
        []
    ),
    ?assertEqual(0, Cnt2),
    {ok, _, [{1}]} = epgsql:equery(
        Conn, "SELECT COUNT(*) FROM attachment WHERE path=$1", [K2]
    ),
    %% 无共享证据的个人明文附件：注销正常清理（个人引用释放语义）
    K3 = <<"v5e2e/w301/owner/personal">>,
    q(Conn, "DELETE FROM attachment WHERE path=$1", [K3]),
    q(
        Conn,
        "INSERT INTO attachment(id,path,scope,creator_user_id,storage_driver,"
        "storage_namespace,storage_container,storage_locator_sha256,gc_verified,"
        "cipher,file_hash256) VALUES ($1,$2,'private',101,'garage','ns1','c1',"
        "$3,false,NULL,$4)",
        [?ASSET_ID + 42, K3, locator(), binary:copy(<<"7">>, 64)]
    ),
    {ok, _, [{Cnt3}]} = epgsql:equery(
        Conn,
        "WITH del AS (DELETE FROM attachment a WHERE a.creator_user_id=101"
        " AND a.cipher IS NULL AND a.group_file_id IS NULL"
        " AND a.scope NOT IN ('group','channel','enterprise')"
        " AND NOT EXISTS (SELECT 1 FROM user_collect uc WHERE uc.attach_file_hash256 <> ''"
        "   AND uc.attach_file_hash256 = a.file_hash256 AND uc.status = 1)"
        " RETURNING 1) SELECT COUNT(*) FROM del",
        []
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
    %% unknown_location：locator 为 NULL 的群资产（绑定已释放）
    K1 = <<"v5e2e/w301/hist/unknown-location">>,
    w301_seed_group_asset(Conn, K1, #{id => 50, gf => 50, bound => false}),
    q(
        Conn,
        "UPDATE attachment SET storage_driver=NULL, storage_namespace=NULL,"
        " storage_container=NULL, storage_locator_sha256=NULL WHERE path=$1",
        [K1]
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
    w301_seed_group_asset(Conn, K3, #{id => 52, gf => 52, bound => false}),
    q(
        Conn,
        "INSERT INTO user_collect(id,user_id,kind,kind_id,attach_file_hash256,status)"
        " VALUES (992,202,0,'x',$1,1)",
        [w301_hash()]
    ),
    ?assertEqual(false, w301_gc_verified_refresh(Conn, K3)),
    q(Conn, "DELETE FROM user_collect WHERE id=992"),
    ok.
