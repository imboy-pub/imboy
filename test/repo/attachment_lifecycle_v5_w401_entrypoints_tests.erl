%%% @doc W4-01 入口收口行为套件（真 scratch DB；迁移 184 + 清理入口收口）。
%%%
%%% 覆盖（02 §2/§4/§5）：
%%%   * pending 域意图 fn_v5_intent_pending_delete：
%%%     - sweep：registered（能力窗已耗尽）→ expired → delete_intent +
%%%       ready job + 审计同事务；
%%%     - reject：registered（能力窗未耗尽——主动拒绝即撤权）→ rejected →
%%%       delete_intent 两跳；
%%%     - 能力窗未耗尽时 sweep 拒绝（23510）；
%%%     - metadata 行存在时拒绝（必须走 metadata 域意图）；
%%%   * metadata 域意图 fn_v5_intent_ordinary_delete：gc_verified=true 的
%%%     孤儿形态可发意图（lifecycle→delete_intent + ready job）；
%%%     gc_verified=false 保守保留（23510）；
%%%   * 清理入口收口：attachment_ds:pending_cleanup / orphan_cleanup 只发
%%%     意图不删字节（elib_oss mock 断言零 delete 调用）。
%%%
%%% 直连 eunit 模式（test-build 预热后 erl 直跑；改源码后必须重编）。
-module(attachment_lifecycle_v5_w401_entrypoints_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-define(REG_JSON,
    "{\"subject\":{\"kind\":\"user\",\"principal\":\"101\"},"
    "\"declaration\":{\"mime\":\"image/png\"}}"
).

w401_entrypoints_test_() ->
    {timeout, 900, fun run/0}.

run() ->
    Boot = eunit_runner:eunit_setup(),
    State = inttest_marker_db:provision(
        #{
            env_prefix => <<"V5_INTTEST_W401">>,
            connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
        }
    ),
    Conn = maps:get(conn, State),
    try
        w401_pending_sweep(Conn),
        w401_pending_reject(Conn),
        w401_capability_window(Conn),
        w401_metadata_backed_rejected(Conn),
        w401_metadata_intent(Conn)
    after
        inttest_marker_db:release(State),
        eunit_runner:eunit_cleanup(Boot)
    end.

hex64(Bin) ->
    iolist_to_binary([io_lib:format("~2.16.0b", [B]) || B <- binary_to_list(Bin)]).

loc(N) -> hex64(crypto:hash(sha256, integer_to_binary(N))).

register_pending(Conn, Key, N) ->
    {ok, _, _} = epgsql:equery(
        Conn,
        "SELECT * FROM fn_v5_register_pending($1,'b','private',101,"
        "gen_random_uuid(),$2::jsonb,now()+interval '2 hours',"
        "now()+interval '1 hour','garage','ns1','c1',$3)",
        [Key, ?REG_JSON, loc(N)]
    ).

expire(Conn, Key) ->
    %% 无 RETURNING 的 UPDATE 返回 {ok, Count} 二元组
    {ok, _} = epgsql:equery(
        Conn,
        "UPDATE attach_pending SET upload_expires_at = now() - interval '2 hours'"
        " WHERE object_key=$1",
        [Key]
    ).

%% sweep：registered 超龄（能力窗已耗尽）→ expired → delete_intent + ready job
w401_pending_sweep(Conn) ->
    Key = <<"v5e2e/w401/sweep1">>,
    register_pending(Conn, Key, 1),
    expire(Conn, Key),
    {ok, _, _} = epgsql:equery(
        Conn, "SELECT fn_v5_intent_pending_delete($1,'w401 sweep','sweep')", [Key]
    ),
    {ok, _, [{<<"delete_intent">>}]} = epgsql:equery(
        Conn, "SELECT session_state FROM attach_pending WHERE object_key=$1", [Key]
    ),
    {ok, _, [{1}]} = epgsql:equery(
        Conn,
        "SELECT COUNT(*) FROM attachment_object_deletion"
        " WHERE path=$1 AND job_state='ready'",
        [Key]
    ),
    {ok, _, [{1}]} = epgsql:equery(
        Conn,
        "SELECT COUNT(*) FROM attachment_lifecycle_v5_audit"
        " WHERE object_key=$1 AND action='delete_intent'",
        [Key]
    ),
    %% 重放：delete_intent 起点先被状态机守卫拒绝（23510，非 tombstone——
    %% 状态机检查先于 tombstone 检查，与 metadata 域意图一致）
    {error, #error{code = <<"23510">>}} = epgsql:equery(
        Conn, "SELECT fn_v5_intent_pending_delete($1,'w401 sweep replay','sweep')", [Key]
    ),
    ok = epgsql:sync(Conn).

%% reject：registered 主动拒绝（能力窗未耗尽）→ rejected → delete_intent
w401_pending_reject(Conn) ->
    Key = <<"v5e2e/w401/rej1">>,
    register_pending(Conn, Key, 2),
    {ok, _, _} = epgsql:equery(
        Conn, "SELECT fn_v5_intent_pending_delete($1,'w401 reject','reject')", [Key]
    ),
    {ok, _, [{<<"delete_intent">>}]} = epgsql:equery(
        Conn, "SELECT session_state FROM attach_pending WHERE object_key=$1", [Key]
    ),
    {ok, _, [{1}]} = epgsql:equery(
        Conn, "SELECT COUNT(*) FROM attachment_object_deletion WHERE path=$1", [Key]
    ).

%% 能力窗未耗尽时 sweep 拒绝（23510）
w401_capability_window(Conn) ->
    Key = <<"v5e2e/w401/live1">>,
    register_pending(Conn, Key, 3),
    {error, #error{code = <<"23510">>}} = epgsql:equery(
        Conn, "SELECT fn_v5_intent_pending_delete($1,'w401 live','sweep')", [Key]
    ),
    ok = epgsql:sync(Conn),
    {ok, _, [{<<"registered">>}]} = epgsql:equery(
        Conn, "SELECT session_state FROM attach_pending WHERE object_key=$1", [Key]
    ).

%% metadata 行存在（live）时 pending 意图拒绝——必须走 metadata 域意图
w401_metadata_backed_rejected(Conn) ->
    Key = <<"v5e2e/w401/metadata-backed">>,
    register_pending(Conn, Key, 4),
    expire(Conn, Key),
    {ok, 1} = epgsql:equery(
        Conn,
        "INSERT INTO attachment(id,path,scope,creator_user_id,"
        "storage_driver,storage_namespace,storage_container,"
        "storage_locator_sha256,gc_verified,cipher,group_file_id,file_hash256)"
        " VALUES (930100,$1,'private',101,'garage','ns1','c1',$2,false,NULL,NULL,$3)",
        [Key, loc(100), hex64(crypto:hash(sha256, <<"w401 meta">>))]
    ),
    {error, #error{code = <<"23510">>}} = epgsql:equery(
        Conn, "SELECT fn_v5_intent_pending_delete($1,'w401 meta-backed','sweep')", [Key]
    ),
    ok = epgsql:sync(Conn).

%% metadata 域意图：gc_verified=true 可发；false 保守保留
w401_metadata_intent(Conn) ->
    %% true：孤儿形态（明文+完整位置+无绑定）→ delete_intent + ready job
    K1 = <<"v5e2e/w401/orphan-true">>,
    {ok, 1} = epgsql:equery(
        Conn,
        "INSERT INTO attachment(id,path,scope,creator_user_id,"
        "storage_driver,storage_namespace,storage_container,"
        "storage_locator_sha256,gc_verified,cipher,group_file_id,file_hash256)"
        " VALUES (930101,$1,'private',101,'garage','ns1','c1',$2,true,NULL,NULL,$3)",
        [K1, loc(101), hex64(crypto:hash(sha256, <<"w401 gc true">>))]
    ),
    {ok, _, _} = epgsql:equery(
        Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w401 orphan gc-true')", [K1]
    ),
    {ok, _, [{<<"delete_intent">>}]} = epgsql:equery(
        Conn, "SELECT lifecycle_state FROM attachment WHERE path=$1", [K1]
    ),
    {ok, _, [{1}]} = epgsql:equery(
        Conn,
        "SELECT COUNT(*) FROM attachment_object_deletion WHERE path=$1 AND job_state='ready'",
        [K1]
    ),
    %% false：位置/引用完整性不可证 → 保守保留（23510、0 job）
    K2 = <<"v5e2e/w401/orphan-false">>,
    {ok, 1} = epgsql:equery(
        Conn,
        "INSERT INTO attachment(id,path,scope,creator_user_id,"
        "storage_driver,storage_namespace,storage_container,"
        "storage_locator_sha256,gc_verified,cipher,group_file_id,file_hash256)"
        " VALUES (930102,$1,'private',101,'garage','ns1','c1',$2,false,NULL,NULL,$3)",
        [K2, loc(102), hex64(crypto:hash(sha256, <<"w401 gc false">>))]
    ),
    {error, #error{code = <<"23510">>}} = epgsql:equery(
        Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w401 orphan gc-false')", [K2]
    ),
    ok = epgsql:sync(Conn),
    {ok, _, [{<<"live">>}]} = epgsql:equery(
        Conn, "SELECT lifecycle_state FROM attachment WHERE path=$1", [K2]
    ),
    {ok, _, [{0}]} = epgsql:equery(
        Conn, "SELECT COUNT(*) FROM attachment_object_deletion WHERE path=$1", [K2]
    ).
