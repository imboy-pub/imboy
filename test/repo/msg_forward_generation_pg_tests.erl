-module(msg_forward_generation_pg_tests).
-include_lib("eunit/include/eunit.hrl").

forward_generation_test_() ->
    case os:getenv("C08_FORWARD_PG_DB") of
        false -> [];
        _ -> {timeout, 30, fun forward_generation/0}
    end.

forward_generation() ->
    Db = os:getenv("C08_FORWARD_PG_DB"),
    true = lists:prefix("c08_clean_", Db),
    {ok, _} = application:ensure_all_started(epgsql),
    {ok, Conn} = epgsql:connect(connection_opts(Db)),
    try
        {ok, _, [{<<"c08-clean-candidate-owned">>}]} = epgsql:squery(
            Conn,
            "SELECT shobj_description(oid,'pg_database') FROM pg_database WHERE datname=current_database()"
        ),
        {ok, [], []} = epgsql:squery(Conn, "BEGIN"),
        {ok, [], []} = epgsql:squery(Conn, "SET LOCAL session_replication_role=replica"),
        run_fixture(Conn)
    after
        epgsql:squery(Conn, "ROLLBACK"),
        epgsql:close(Conn)
    end.

run_fixture(C) ->
    Base = 9000000000000 + binary:decode_unsigned(crypto:strong_rand_bytes(5)),
    Gid = Base + 1,
    Uid = Base + 2,
    Msg = <<"c08-generation-", (integer_to_binary(Base))/binary>>,
    {ok, _} = elib_pg:query(
        C,
        <<"INSERT INTO public.\"group\"(id,owner_uid,creator_uid,scope) VALUES($1,$2,$2,'personal')">>,
        [Gid, Uid]
    ),
    {ok, _} = elib_pg:query(
        C,
        <<"INSERT INTO public.group_member(id,group_id,user_id,role,is_join,status) VALUES($1,$2,$3,1,true,1)">>,
        [Base + 3, Gid, Uid]
    ),
    {ok, _} = elib_pg:query(
        C,
        <<"INSERT INTO public.group_member_generation(group_id,user_id,start_seq) VALUES($1,$2,5)">>,
        [Gid, Uid]
    ),
    {ok, _} = elib_pg:query(
        C,
        <<"INSERT INTO public.msg_c2g(id,from_id,to_id,msg_id,msg_type,payload) VALUES($1,$2,$3,$4,'text','{}'::jsonb)">>,
        [Base + 5, Uid, Gid, Msg]
    ),
    {ok, _} = elib_pg:query(
        C,
        <<"INSERT INTO public.msg_c2g_recipient_snapshot(msg_id,from_id,to_gid,conv_seq,recipient_uids,created_at) VALUES($1,$2,$3,8,ARRAY[$2::bigint],now())">>,
        [Msg, Uid, Gid]
    ),
    ?assert(msg_forward_repo:source_group_visible_tx(C, Msg, Gid, Uid)),
    ?assertEqual({error, forbidden}, forward_attachment_source_repo:c2g_tx(C, Msg, Gid, Uid)),
    {ok, _} = elib_pg:query(
        C,
        <<"INSERT INTO public.c2g_forward_source_fact(msg_id,group_id,sender_id,conv_seq) VALUES($1,$2,$3,8)">>,
        [Msg, Gid, Uid]
    ),
    ?assertEqual(ok, forward_attachment_source_repo:c2g_tx(C, Msg, Gid, Uid)),
    lock_excluded(<<"c2g_forward_revoke:", (integer_to_binary(Gid))/binary, ":", Msg/binary>>),
    {ok, _} = elib_pg:query(
        C,
        <<"INSERT INTO public.c2g_revoke_fact(group_id,original_msg_id,action_msg_id,actor_uid,accepted_seq) VALUES($1,$2,$3,$4,9)">>,
        [Gid, Msg, <<"c08-test-revoke">>, Uid]
    ),
    ?assertNot(msg_forward_repo:source_group_visible_tx(C, Msg, Gid, Uid)),
    ?assertEqual({error, forbidden}, forward_attachment_source_repo:c2g_tx(C, Msg, Gid, Uid)),
    {ok, _} = elib_pg:query(
        C, <<"DELETE FROM public.c2g_revoke_fact WHERE group_id=$1 AND original_msg_id=$2">>, [
            Gid, Msg
        ]
    ),
    {ok, _} = elib_pg:query(
        C,
        <<"UPDATE public.msg_c2g SET expire_at=now()-interval '1 second' WHERE msg_id=$1">>,
        [Msg]
    ),
    ?assertNot(msg_forward_repo:source_group_visible_tx(C, Msg, Gid, Uid)),
    {ok, _} = elib_pg:query(
        C,
        <<"UPDATE public.msg_c2g SET expire_at=NULL WHERE msg_id=$1">>,
        [Msg]
    ),
    {ok, _} = elib_pg:query(
        C,
        <<"UPDATE public.group_member_generation SET start_seq=9 WHERE group_id=$1 AND user_id=$2">>,
        [Gid, Uid]
    ),
    ?assertNot(msg_forward_repo:source_group_visible_tx(C, Msg, Gid, Uid)),
    {ok, _} = elib_pg:query(
        C,
        <<"UPDATE public.group_member_generation SET start_seq=1 WHERE group_id=$1 AND user_id=$2">>,
        [Gid, Uid]
    ),
    {ok, _} = elib_pg:query(
        C,
        <<"UPDATE public.msg_c2g_recipient_snapshot SET recipient_uids=ARRAY[$2::bigint] WHERE msg_id=$1">>,
        [Msg, Base + 4]
    ),
    ?assertNot(msg_forward_repo:source_group_visible_tx(C, Msg, Gid, Uid)),
    c2c_fixture(C, Base, Uid).

c2c_fixture(C, Base, Uid) ->
    Peer = Base + 6,
    Msg = <<"c08-c2c-", (integer_to_binary(Base))/binary>>,
    {ok, _} = elib_pg:query(
        C,
        <<"INSERT INTO public.msg_c2c(id,from_id,to_id,msg_id,msg_type,payload,conv_seq) VALUES($1,$2,$3,$4,'text','{}',1)">>,
        [Base + 7, Uid, Peer, Msg]
    ),
    ?assertEqual({error, forbidden}, forward_attachment_source_repo:c2c_tx(C, Msg, Uid)),
    {ok, _} = elib_pg:query(
        C,
        <<"INSERT INTO public.c2c_forward_source_fact(msg_id,sender_id,recipient_id,conv_seq) VALUES($1,$2,$3,1)">>,
        [Msg, Uid, Peer]
    ),
    ?assertEqual(ok, forward_attachment_source_repo:c2c_tx(C, Msg, Uid)),
    lock_excluded(<<"c2c_forward_revoke:", Msg/binary>>),
    ?assertEqual({error, forbidden}, forward_attachment_source_repo:c2c_tx(C, Msg, Base + 8)),
    {ok, _} = elib_pg:query(
        C,
        <<"INSERT INTO public.c2c_action_receipt(request_id,original_msg_id,actor_uid,peer_uid,action) VALUES($1,$2,$3,$4,'revoke')">>,
        [<<"c08-test-receipt">>, Msg, Uid, Peer]
    ),
    ?assertEqual({error, forbidden}, forward_attachment_source_repo:c2c_tx(C, Msg, Uid)).

lock_excluded(Key) ->
    {ok, Other} = epgsql:connect(connection_opts(os:getenv("C08_FORWARD_PG_DB"))),
    try
        {ok, [#{<<"acquired">> := false}]} = elib_pg:query(
            Other,
            <<"SELECT pg_try_advisory_xact_lock(hashtextextended($1,0)) AS acquired">>,
            [Key]
        )
    after
        epgsql:close(Other)
    end.

connection_opts(Db) ->
    true = lists:prefix("c08_clean_", Db),
    Port = list_to_integer(os:getenv("C08_FORWARD_PG_PORT", "49320")),
    true = Port > 0 andalso Port =< 65535,
    #{
        host => "127.0.0.1",
        port => Port,
        username => os:getenv("C08_FORWARD_PG_USER"),
        password => os:getenv("C08_FORWARD_PG_PASSWORD"),
        database => Db
    }.

%% ---------------------------------------------------------------- M-02 追加
%% 转发请求稳定身份迁移的真库验证（00000281；复占改号 277→281）。
%% 复用 inttest_marker_db 一次性 marker 库配方：隔离库 + 全链 erlang_migrate
%% （strict，与生产/演练同口径）；无 skip 语义。既有 C08 env 门控生成器
%% （forward_generation_test_）零改动——无 C08_FORWARD_PG_DB 时保持空列表。

identity_migration_test_() ->
    {timeout, 1800,
        {setup, fun identity_setup/0, fun identity_cleanup/1, fun(S) ->
            [?_test(identity_full_chain(S)), ?_test(identity_schema_guards(S))]
        end}}.

identity_rehearsal_test_() ->
    {timeout, 1800,
        {setup, fun identity_setup_rehearsal/0, fun identity_cleanup/1, fun(S) ->
            [?_test(identity_up_down_rehearsal(S))]
        end}}.

identity_setup() ->
    inttest_marker_db:provision(#{
        env_prefix => <<"M02FWD">>,
        connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
    }).

%% 演练库停在 280（00000281 未应用）：真实 up → down → up 往返
identity_setup_rehearsal() ->
    inttest_marker_db:provision(#{
        env_prefix => <<"M02FWDRC">>,
        migration_target => 280,
        connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
    }).

identity_cleanup(S) ->
    inttest_marker_db:release(S).

identity_mig_conf(#{conn := Conn}) ->
    #{conn => Conn, dir => "priv/migrations", strict => true}.

identity_current_version(Conn) ->
    case epgsql:equery(Conn, "SELECT max(version)::bigint FROM schema_migrations", []) of
        {ok, _, [{V}]} when is_integer(V) -> V;
        {ok, _, [{null}]} -> 0;
        {error, Reason} -> erlang:error({current_version_failed, Reason})
    end.

identity_table_exists(Conn, Name) ->
    {ok, _, Rows} = epgsql:equery(
        Conn,
        "SELECT 1 FROM information_schema.tables "
        "WHERE table_schema='public' AND table_name=$1",
        [Name]
    ),
    Rows =/= [].

identity_expect_pg_error(Conn, Sql, Params, WantCode) ->
    case epgsql:equery(Conn, Sql, Params) of
        {error, {error, _, WantCode, _, _, _}} -> ok;
        {error, {error, WantCode, _, _, _, _}} -> ok;
        {error, Reason} -> erlang:error({wrong_pg_error, WantCode, Reason});
        Ok -> erlang:error({expected_pg_error, WantCode, Ok})
    end.

%% M-02:LOCAL:01 旧数据不破坏、旧端不伪造新结构
identity_full_chain(#{conn := Conn} = _S) ->
    Head = inttest_marker_db:migration_head(),
    ?assertEqual(Head, identity_current_version(Conn)),
    {ok, _, [{N}]} = epgsql:equery(
        Conn, "SELECT count(*)::bigint FROM schema_migrations_history WHERE version=$1", [281]
    ),
    ?assertEqual(1, N),
    ?assert(identity_table_exists(Conn, <<"forward_request_identity">>)),
    %% 新表零默认行：既有用户/消息不携带任何新结构行
    {ok, _, [{0}]} = epgsql:equery(
        Conn, "SELECT count(*)::bigint FROM public.forward_request_identity", []
    ),
    %% 既有转发域未动（表仍在 + 列集合不变）
    ?assert(identity_table_exists(Conn, <<"forward_attachment_reference">>)),
    ?assert(identity_table_exists(Conn, <<"c2c_forward_source_fact">>)),
    ?assert(identity_table_exists(Conn, <<"c2g_forward_source_fact">>)),
    {ok, _, [{RefCols}]} = epgsql:equery(
        Conn,
        "SELECT count(*)::bigint FROM information_schema.columns "
        "WHERE table_schema='public' AND table_name='forward_attachment_reference'",
        []
    ),
    ?assertEqual(8, RefCols),
    %% 旧端兼容写：旧式 msg_c2c + c2c_forward_source_fact 插入仍通
    Base = erlang:unique_integer([positive]),
    {ok, _} = epgsql:equery(
        Conn,
        "INSERT INTO public.msg_c2c(id,from_id,to_id,msg_id,msg_type,payload,conv_seq) "
        "VALUES($1,101,202,$2,'text','{}',$3)",
        [
            Base,
            <<"m02-legacy-", (integer_to_binary(Base))/binary>>,
            erlang:unique_integer([positive, monotonic])
        ]
    ),
    {ok, _} = epgsql:equery(
        Conn,
        "INSERT INTO public.c2c_forward_source_fact(msg_id,sender_id,recipient_id,conv_seq) "
        "VALUES($1,101,202,$2)",
        [
            <<"m02-legacy-", (integer_to_binary(Base))/binary>>,
            erlang:unique_integer([positive, monotonic])
        ]
    ),
    {comment, <<"fresh chain includes migration 281; legacy forward writes intact">>}.

%% 稳定身份语义守卫：request_id 幂等键（23505）+ 结算事实 CHECK（23514）+ 枚举/哈希格式
identity_schema_guards(#{conn := Conn} = _S) ->
    ReqId = <<"m02-identity-", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    InsSql =
        "INSERT INTO public.forward_request_identity "
        "(request_id,requester_uid,source_type,source_msg_id,target_type,target_id) "
        "VALUES ($1,101,'c2c',$2,'c2g',202)",
    {ok, 1} = epgsql:equery(Conn, InsSql, [ReqId, <<"m02-src-1">>]),
    %% 同 request_id 重试 = 主键冲突（不产生重复转发）
    identity_expect_pg_error(Conn, InsSql, [ReqId, <<"m02-src-1">>], <<"23505">>),
    %% 结算事实 CHECK：只写 settled 或只写 result 均拒绝
    identity_expect_pg_error(
        Conn,
        "UPDATE public.forward_request_identity SET settled_at=clock_timestamp() WHERE request_id=$1",
        [ReqId],
        <<"23514">>
    ),
    identity_expect_pg_error(
        Conn,
        "UPDATE public.forward_request_identity SET result_msg_id='m02-result-1' WHERE request_id=$1",
        [ReqId],
        <<"23514">>
    ),
    {ok, 1} = epgsql:equery(
        Conn,
        "UPDATE public.forward_request_identity SET settled_at=clock_timestamp(), "
        "result_msg_id='m02-result-1' WHERE request_id=$1",
        [ReqId]
    ),
    %% 枚举与哈希格式 CHECK
    BadReq = <<"m02-identity-bad-", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    identity_expect_pg_error(
        Conn,
        "INSERT INTO public.forward_request_identity "
        "(request_id,requester_uid,source_type,source_msg_id,target_type,target_id) "
        "VALUES ($1,101,'c2x',$2,'c2g',202)",
        [BadReq, <<"m02-src-2">>],
        <<"23514">>
    ),
    identity_expect_pg_error(
        Conn,
        "INSERT INTO public.forward_request_identity "
        "(request_id,requester_uid,source_type,source_msg_id,target_type,target_id,content_sha256) "
        "VALUES ($1,101,'c2c',$2,'c2g',202,'nothex')",
        [BadReq, <<"m02-src-3">>],
        <<"23514">>
    ),
    {comment, <<"request_id idempotency + settled-fact + CHECK guards verified">>}.

%% M-02:LOCAL:02 up/down/补偿失败有原始证据且不强行重建 DB
identity_up_down_rehearsal(#{conn := Conn} = S) ->
    %% 停在 280：新表不存在，既有域完整
    ?assertEqual(280, identity_current_version(Conn)),
    ?assertNot(identity_table_exists(Conn, <<"forward_request_identity">>)),
    ?assert(identity_table_exists(Conn, <<"msg_c2c">>)),
    ?assert(identity_table_exists(Conn, <<"forward_attachment_reference">>)),
    %% up：真实应用 00000281
    case erlang_migrate:up(identity_mig_conf(S), 1) of
        ok -> ok;
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({up_281_failed, Reason})
    end,
    ?assertEqual(281, identity_current_version(Conn)),
    ?assert(identity_table_exists(Conn, <<"forward_request_identity">>)),
    %% down 1：真实回退；既有域表不受影响、旧式读写仍通
    case erlang_migrate:down(identity_mig_conf(S), 1) of
        ok -> ok;
        {ok, _} -> ok;
        {error, Reason2} -> erlang:error({down_281_failed, Reason2})
    end,
    ?assertEqual(280, identity_current_version(Conn)),
    ?assertNot(identity_table_exists(Conn, <<"forward_request_identity">>)),
    ?assert(identity_table_exists(Conn, <<"msg_c2c">>)),
    ?assert(identity_table_exists(Conn, <<"forward_attachment_reference">>)),
    Base = erlang:unique_integer([positive]),
    {ok, _} = epgsql:equery(
        Conn,
        "INSERT INTO public.msg_c2c(id,from_id,to_id,msg_id,msg_type,payload,conv_seq) "
        "VALUES($1,101,202,$2,'text','{}',$3)",
        [
            Base,
            <<"m02-postdown-", (integer_to_binary(Base))/binary>>,
            erlang:unique_integer([positive, monotonic])
        ]
    ),
    %% 再 up：恢复 281，新表回归
    case erlang_migrate:up(identity_mig_conf(S), 1) of
        ok -> ok;
        {ok, _} -> ok;
        {error, Reason3} -> erlang:error({reup_281_failed, Reason3})
    end,
    ?assertEqual(281, identity_current_version(Conn)),
    ?assert(identity_table_exists(Conn, <<"forward_request_identity">>)),
    {comment, <<"up->down->up rehearsal on real PG verified">>}.
