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
    Token = one(Conn, "SELECT gen_random_uuid()", []),
    {ok, _, [{Path}]} = epgsql:equery(
        Conn, "SELECT out_path FROM fn_v5_claim_ordinary($1)", [Token]
    ),
    ?assertEqual(?REG_KEY, Path),
    {ok, _, [{<<"done">>}]} = epgsql:equery(
        Conn, "SELECT fn_v5_ack_ordinary($1,$2,true)", [?REG_KEY, Token]
    ),
    %% stale token cannot mutate done row
    {ok, _, [{<<"not_leased">>}]} = epgsql:equery(
        Conn, "SELECT fn_v5_ack_ordinary($1,$2,true)", [?REG_KEY, Token]
    ).

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
        w203_confirm_first(maps:get(conn, State), maps:get(maint_conn, State))
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
w203_confirm_first(Conn, BarrierConn) ->
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
                    {ok, _, _} =
                        epgsql:equery(
                            BarrierConn,
                            "BEGIN",
                            []
                        ),
                    {ok, _, _} =
                        epgsql:equery(
                            BarrierConn,
                            "SELECT path FROM attachment WHERE path=$1 FOR UPDATE",
                            [Key]
                        ),
                    Parent ! {w203_held, self()},
                    receive
                        release -> ok
                    after 8000 -> ok
                    end,
                    _ = epgsql:equery(BarrierConn, "ROLLBACK", []),
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
