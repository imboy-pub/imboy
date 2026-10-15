%% attachment_lifecycle_v5_hold_protocol_tests — W3-02 Retention/Hold
%% 统一资格与 Hold 竞争协议（02 §5，fixture 映射协议级；12 生产入口的
%% 真实改造与整矩阵复验属 W4-01，不作为 G3 条件）。
%%
%% hold-all-entrypoints：12 入口 fixture × 6 治理 fixture
%%（hold_workspace / hold_conversation / hold_message / hold_attachment /
%%  future_retention / permanent_workspace_retention）=> intent 全部 23510。
%% hold-gc-race：hold 先提交 => intent 拒绝；intent 先提交 => 治理操作
%% already_deleting。
-module(attachment_lifecycle_v5_hold_protocol_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-define(ASSET_BASE, 920000000).

hold_protocol_test_() ->
    {timeout, 900, fun run/0}.

run() ->
    State = inttest_marker_db:provision(
        #{
            env_prefix => <<"V5_INTTEST_W302">>,
            connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
        }
    ),
    Conn = maps:get(conn, State),
    try
        q(
            Conn,
            "INSERT INTO public.\"user\" (id, account, nickname, password, status,"
            " account_type, reg_ip, reg_cosv, created_at)"
            " VALUES (101, 'w302-u', 'w302', '', 1, 0, '127.0.0.1', 'w302', now())"
        ),
        q(
            Conn,
            "INSERT INTO organization(id,name,owner_id)"
            " VALUES (9101,'w302-org',101)"
        ),
        q(
            Conn,
            "INSERT INTO workspace(organization_id,id,name,owner_id,branding)"
            " VALUES (9101,9102,'w302-ws',101,'{}')"
        ),
        %% EAR 行（hold_message/hold_attachment/future_retention）引用
        %% (organization_id, application_id)=(9101,9103)，须先种 Application
        q(
            Conn,
            "INSERT INTO enterprise_application"
            " (id, organization_id, application_key, name, status)"
            " VALUES (9103, 9101, 'w302-9103', 'w302 app', 'active')"
            " ON CONFLICT (organization_id, id) DO NOTHING"
        ),
        lists:foreach(
            fun({Ep, ScopeVal, Cipher, N}) ->
                w302_entrypoint_matrix(Conn, Ep, ScopeVal, Cipher, N)
            end,
            entrypoints()
        ),
        w302_hold_gc_race(Conn)
    after
        inttest_marker_db:release(State)
    end.

q(Conn, Sql) -> q(Conn, Sql, []).
q(Conn, Sql, Params) ->
    case epgsql:equery(Conn, Sql, Params) of
        {ok, _} ->
            ok;
        {ok, _, _} ->
            ok;
        {error, Reason} ->
            erlang:error({w302_sql_failed, Reason, Sql})
    end.

e(Conn, Sql, Params) ->
    case epgsql:equery(Conn, Sql, Params) of
        {ok, _, _} = Ok -> Ok;
        {error, _} = Err -> Err
    end.

%% 12 未来删除入口的 fixture 映射（§6 入口表；协议同 fn_v5_intent_ordinary_delete；
%% 密文/历史歧义入口按 02 §5 语义以 cipher 形态表达——资格协议同一 fn）
entrypoints() ->
    [
        {reject_size, <<"group">>, null, 10},
        {reject_mime, <<"group">>, null, 11},
        {reject_teaching, <<"private">>, null, 12},
        {orphan_cleanup, <<"moment">>, null, 13},
        {pending_cleanup, <<"private">>, null, 14},
        {account_deletion, <<"private">>, null, 15},
        {teaching_unbound, <<"private">>, <<"AES-256-GCM">>, 16},
        {app_confirm_reject, <<"enterprise">>, null, 17},
        {app_retention_purge, <<"enterprise">>, null, 18},
        {eb_private_delete, <<"c2c">>, null, 19},
        {eb_pending_cleanup, <<"moment">>, <<"AES-256-GCM">>, 20},
        {eb_purge_drain, <<"private">>, null, 21}
    ].

hex64(Base) ->
    iolist_to_binary([io_lib:format("~2.16.0b", [B]) || B <- binary_to_list(Base)]).

%% 单入口 fixture：confirmed 资产（绑定释放形态 => 引用零可证明）+ 能力窗已过
seed(Conn, Key, ScopeVal, Cipher, N) ->
    q(Conn, "DELETE FROM attach_pending WHERE object_key=$1", [Key]),
    q(Conn, "DELETE FROM attachment WHERE path=$1", [Key]),
    {ok, _, _} = epgsql:equery(
        Conn,
        "SELECT * FROM fn_v5_register_pending($1,'b',$2,101,"
        "gen_random_uuid(),$3::jsonb,now(),"
        "now() - interval '2 hours','garage','ns1','c1',$4)",
        [Key, ScopeVal, registration_fixture(), locator_fixture(N)]
    ),
    {ok, _, _} = epgsql:equery(
        Conn,
        "SELECT fn_v5_pin_pending_content($1,$2::jsonb)",
        [Key, receipt_fixture(N)]
    ),
    q(
        Conn,
        "INSERT INTO attachment(id,path,scope,creator_user_id,"
        "storage_driver,storage_namespace,storage_container,"
        "storage_locator_sha256,gc_verified,cipher,group_file_id,file_hash256)"
        " VALUES ($1,$2,$3,101,'garage','ns1','c1',$4,false,$5,NULL,$6)",
        [
            ?ASSET_BASE + N,
            Key,
            ScopeVal,
            locator_fixture(N),
            Cipher,
            hex64(crypto:hash(sha256, integer_to_binary(N)))
        ]
    ),
    {ok, _, _} = epgsql:equery(
        Conn, "SELECT fn_v5_confirm_pending($1,$2)", [Key, ?ASSET_BASE + N]
    ),
    q(
        Conn,
        "UPDATE attach_pending SET upload_expires_at = now() - interval '2 hours'"
        " WHERE object_key=$1",
        [Key]
    ),
    ok.

registration_fixture() ->
    <<
        "{\"subject\":{\"kind\":\"user\",\"principal\":\"101\"},"
        "\"declaration\":{\"mime\":\"image/png\"}}"
    >>.

receipt_fixture(N) ->
    Hex = hex64(crypto:hash(sha256, integer_to_binary(N))),
    [
        "{\"version\":1,\"sha256\":\"",
        Hex,
        "\",\"size_bytes\":\"16\","
        "\"mime\":\"image/png\"}"
    ].

locator_fixture(N) ->
    hex64(crypto:hash(sha256, integer_to_binary(N))).

%% 治理 fixture：按 scope 安装阻尼行 => 返回 {ok, 清理函数}
install_governance(Conn, AttId, hold_workspace) ->
    q(
        Conn,
        "INSERT INTO workspace_attachment_retention(attachment_id,organization_id,"
        "workspace_id,scope,scope_ref) VALUES ($1,9101,9102,'group','66')"
        " ON CONFLICT (attachment_id) DO UPDATE SET scope = 'group',"
        " scope_ref = '66'",
        [AttId]
    ),
    fun() ->
        q(
            Conn,
            "DELETE FROM workspace_attachment_retention WHERE attachment_id=$1",
            [AttId]
        )
    end;
install_governance(Conn, AttId, hold_conversation) ->
    q(
        Conn,
        "INSERT INTO workspace_attachment_retention(attachment_id,organization_id,"
        "workspace_id,scope,scope_ref) VALUES ($1,9101,9102,'channel','66')"
        " ON CONFLICT (attachment_id) DO UPDATE SET scope = 'channel',"
        " scope_ref = '66'",
        [AttId]
    ),
    fun() ->
        q(
            Conn,
            "DELETE FROM workspace_attachment_retention WHERE attachment_id=$1",
            [AttId]
        )
    end;
install_governance(Conn, AttId, hold_message) ->
    q(
        Conn,
        "INSERT INTO enterprise_attachment_retention(attachment_id,organization_id,"
        " application_id,retention_until,hold_state,hold_set_at,hold_reason)"
        " VALUES ($1,9101,9103,now()+interval '30 days','held',now(),'w302 message hold')",
        [AttId]
    ),
    fun() ->
        q(
            Conn,
            "DELETE FROM enterprise_attachment_retention WHERE attachment_id=$1",
            [AttId]
        )
    end;
install_governance(Conn, AttId, hold_attachment) ->
    q(
        Conn,
        "INSERT INTO enterprise_attachment_retention(attachment_id,organization_id,"
        " application_id,retention_until,hold_state,hold_set_at,hold_reason)"
        " VALUES ($1,9101,9103,now()+interval '30 days','held',now(),'w302 attachment hold')",
        [AttId]
    ),
    fun() ->
        q(
            Conn,
            "DELETE FROM enterprise_attachment_retention WHERE attachment_id=$1",
            [AttId]
        )
    end;
install_governance(Conn, AttId, future_retention) ->
    q(
        Conn,
        "INSERT INTO enterprise_attachment_retention(attachment_id,organization_id,"
        " application_id,retention_until,hold_state,hold_set_at,hold_reason)"
        %% ck_ear_hold_reason：hold_state='none' 时 hold_reason 须为 NULL
        " VALUES ($1,9101,9103,now() + interval '50 years','none',NULL,NULL)",
        [AttId]
    ),
    fun() ->
        q(
            Conn,
            "DELETE FROM enterprise_attachment_retention WHERE attachment_id=$1",
            [AttId]
        )
    end;
install_governance(Conn, AttId, permanent_workspace_retention) ->
    q(
        Conn,
        "INSERT INTO workspace_attachment_retention(attachment_id,organization_id,"
        "workspace_id,scope,scope_ref) VALUES ($1,9101,9102,'group','66-permanent')"
        " ON CONFLICT (attachment_id) DO UPDATE SET scope = 'group',"
        " scope_ref = '66-permanent'",
        [AttId]
    ),
    fun() ->
        q(
            Conn,
            "DELETE FROM workspace_attachment_retention WHERE attachment_id=$1",
            [AttId]
        )
    end.

w302_entrypoint_matrix(Conn, Ep, ScopeVal, Cipher, N) ->
    %% 6 治理 fixture：WAR/EAR 行不可变（迁移触发器）——每个 fixture 用全新
    %% 资产（无复用、无清理），安装后 intent 必须 23510（资格协议拒绝）。
    lists:foreach(
        fun({Idx, Scope}) ->
            Key = [
                "v5e2e/w302/",
                atom_to_binary(Ep, utf8),
                "/",
                integer_to_binary(Idx)
            ],
            N2 = N * 10 + Idx,
            seed(Conn, Key, ScopeVal, Cipher, N2),
            AttId = ?ASSET_BASE + N2,
            install_governance(Conn, AttId, Scope),
            R = e(
                Conn,
                "SELECT fn_v5_intent_ordinary_delete($1,'w302')",
                [Key]
            ),
            ?assertMatch({error, #error{code = <<"23510">>}}, R),
            ok = epgsql:sync(Conn)
        end,
        lists:zip(lists:seq(1, 6), [
            hold_workspace,
            hold_conversation,
            hold_message,
            hold_attachment,
            future_retention,
            permanent_workspace_retention
        ])
    ),
    ok.

%% hold-gc-race：hold 先提交 => intent 拒绝；intent 先提交 => 治理 already_deleting。
w302_hold_gc_race(Conn) ->
    Key = <<"v5e2e/w302/race/hold-first">>,
    seed(Conn, Key, <<"group">>, null, 40),
    AttId = ?ASSET_BASE + 40,
    Cleanup = install_governance(Conn, AttId, hold_message),
    {error, #error{code = <<"23510">>}} =
        e(Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w302 race')", [Key]),
    ok = epgsql:sync(Conn),
    {ok, _, [{1}]} = epgsql:equery(
        Conn, "SELECT COUNT(*) FROM attachment WHERE path=$1", [Key]
    ),
    Cleanup,
    %% intent 先提交 => 治理操作 already_deleting（企业域 resolve→lifecycle 守卫）
    Key2 = <<"v5e2e/w302/race/intent-first">>,
    seed(Conn, Key2, <<"enterprise">>, null, 41),
    %% 企业资产归属治理主体（resolve_attachment 按 scope_ref 解析）
    q(
        Conn,
        "UPDATE attachment SET scope_ref='org:9101/app:9103' WHERE path=$1",
        [Key2]
    ),
    %% 统一资格证明走生产 repo（W3-02 两域统一：无绑定+明文+完整位置
    %% +无收藏/moment/治理行 => true）
    {ok, true} = attachment_repo:refresh_gc_verified_tx(Conn, Key2),
    {ok, _, _} = epgsql:equery(
        Conn, "SELECT fn_v5_intent_ordinary_delete($1,'w302 race intent')", [Key2]
    ),
    Ctx = #{organization_id => 9101, application_id => 9103},
    {ok, _, [{1}]} = epgsql:equery(
        Conn,
        "SELECT COUNT(*) FROM attachment WHERE path=$1 AND lifecycle_state='delete_intent'",
        [Key2]
    ),
    %% 治理经生产 repo 入口（enterprise_asset_retention_logic:governance_tx）；
    %% 裸连接手动事务（本套件无连接池，协议同 W3-01 tombstone 断言）
    {ok, _, _} = epgsql:equery(Conn, "BEGIN", []),
    R = enterprise_asset_retention_logic:governance_tx(Conn, Ctx, #{
        object_key => Key2,
        op => hold,
        hold_reason => <<"w302 late hold">>
    }),
    {ok, _, _} = epgsql:equery(Conn, "ROLLBACK", []),
    {error, {<<"invalid_request">>, already_deleting}} = R,
    ok.
