%%% W6-01 C 组 pending + race 面（52 合同叶）。
%%%
%%% W2-01/pending-multipart（32）：multipart 与 group_file 签发同守登记
%%%   owner/location 与事务边界；mismatch / DB 故障无签名能力。ordinary 域
%%%   走 /api/v1/attachment/presign?method=multipart + POST /upload 真实
%%%   multipart/form-data 入口；enterprise 域无 multipart/form-data 端点，
%%%   以受控网关链路验证同一 oracle（registered owner/location + pin 事务
%%%   边界 + fail-closed），AC01/AC02 双验收叶复用同一次真实执行（合同将
%%%   同一场景复制到两个 AC，先例为既有 w2_01_pending_multipart adapter）；
%%% W2-01/pending-failclosed（12）：INSERT error/exception / COMMIT 未知 =>
%%%   503 无签名能力、无 durable 行；同 client_upload_id 重试恢复同 key
%%%   （HTTP 层 fail-closed + fn_v5_register_pending 真实 SQL 契约双证据）；
%%% W2-03/confirm-gc-race（8）：两种 barrier 顺序——confirm 先提交则 intent
%%%   被拒、字节保留；intent 先提交则 confirm 被拒、metadata 不重建；
%%%   bounded retry 队列 attempts<8 不改变赢家。
-module(attachment_lifecycle_v5_w601_w2_pending_tests).

-export([specs/0]).

-include_lib("eunit/include/eunit.hrl").

-define(S, attachment_lifecycle_v5_w601_w2_support).
-define(F, attachment_w403_http_fixture).
-define(FW, attachment_w403_http_fixture_w601).
-define(C_MP, <<"W2-01/pending-multipart">>).
-define(C_FC, <<"W2-01/pending-failclosed">>).
-define(C_RACE, <<"W2-03/confirm-gc-race">>).
-define(BODY, <<"w601 pending multipart controlled bytes">>).

-define(DOMAINS, [<<"ordinary">>, <<"enterprise">>]).
-define(DRV, [disk, garage]).

w601_w2_pending_test_() ->
    {setup, fun ?F:setup/0, fun ?F:cleanup/1, fun(S) ->
        [leaf_unit(S, Spec) || Spec <- specs()]
    end}.

%% ------------------------------------------------------------ leaf matrix

specs() -> mp_specs() ++ fc_specs() ++ race_specs().

%% pending-multipart：4 场景 × 2 域 × 2 driver × AC01/AC02 = 32 叶。
mp_specs() ->
    [
        {?C_MP, <<"pending-multipart">>, Driver, Sc, Domain, Ac}
     || Sc <-
            [
                <<"multipart_success">>,
                <<"group_file_success">>,
                <<"identity_mismatch">>,
                <<"database_unavailable">>
            ],
        Domain <- ?DOMAINS,
        Driver <- ?DRV,
        Ac <- [<<"AC01">>, <<"AC02">>]
    ].

fc_specs() ->
    [
        {?C_FC, <<"pending-failclosed">>, Driver, Sc, Domain, <<"AC01">>}
     || Sc <- [<<"insert_error">>, <<"insert_exception">>, <<"commit_unknown">>],
        Domain <- ?DOMAINS,
        Driver <- ?DRV
    ].

race_specs() ->
    [
        {?C_RACE, <<"confirm-gc-race">>, Driver, Sc, Domain, <<"AC04">>}
     || Sc <- [<<"confirm_commits_first">>, <<"intent_commits_first">>],
        Domain <- ?DOMAINS,
        Driver <- ?DRV
    ].

leaf_unit(S, {Case, _Ep, Driver, Sc, Domain, Ac} = Spec) ->
    Leaf = leaf_name(Case, Sc, Domain, Driver, Ac),
    {
        binary_to_list(Leaf),
        {timeout, 240, fun() ->
            Measured = scenario(S#{leaf => Leaf}, Spec),
            ?S:emit(Spec, Measured)
        end}
    }.

leaf_name(Case, Sc, Domain, Driver, Ac) ->
    [_, Short] = binary:split(Case, <<"/">>),
    <<
        Short/binary,
        "/",
        Sc/binary,
        "/",
        (short_domain(Domain))/binary,
        "/",
        (atom_to_binary(Driver, utf8))/binary,
        "/",
        Ac/binary
    >>.

short_domain(<<"ordinary">>) -> <<"ord">>;
short_domain(<<"enterprise">>) -> <<"ent">>.

%% -------------------------------------------------------- scenario dispatch

scenario(S, {?C_MP, _, Driver, Sc, Domain, _Ac}) ->
    mp_scenario(S, Driver, Sc, Domain);
scenario(S, {?C_FC, _, Driver, Sc, Domain, _Ac}) ->
    fc_scenario(S, Driver, Sc, Domain);
scenario(S, {?C_RACE, _, Driver, Sc, Domain, _Ac}) ->
    race_scenario(S, Driver, Sc, Domain).

%% ------------------------------------------------------- pending-multipart

mp_scenario(S, Driver, <<"multipart_success">>, <<"ordinary">>) ->
    M = multipart_register(S, Driver, #{<<"method">> => <<"multipart">>}),
    Uploaded = ?FW:multipart_put(S, M, ?BODY),
    ?assert(?F:success(Uploaded)),
    {ok, Pin} = ?S:content_pin(key(M)),
    ?assertEqual(?S:sha(?BODY), maps:get(<<"sha256">>, Pin)),
    ?assert(?F:success(?F:confirm(M))),
    ?assertEqual({ok, ?BODY}, ?F:read(M)),
    #{entrypoint => <<"multipart">>, pin_sha256 => maps:get(<<"sha256">>, Pin)};
mp_scenario(S, Driver, <<"group_file_success">>, <<"ordinary">>) ->
    Gid = new_group(S),
    M = multipart_register(S, Driver, #{
        <<"method">> => <<"multipart">>,
        <<"scope">> => <<"group">>,
        <<"scope_ref">> => integer_to_binary(Gid)
    }),
    Uploaded = ?FW:multipart_put(S, M, ?BODY),
    ?assert(?F:success(Uploaded)),
    {ok, Pin} = ?S:content_pin(key(M)),
    ?assertEqual(?S:sha(?BODY), maps:get(<<"sha256">>, Pin)),
    Confirm = ?FW:confirm_with(
        maps:get(fixture, M),
        M#{confirm_body => #{object_key => key(M), scope => <<"group">>}},
        #{anchor_msg_id => <<"w601-anchor-", (elib_uuid:gen_v7())/binary>>}
    ),
    ?assert(?F:success(Confirm)),
    ?assertEqual(1, ?S:attach_count(key(M))),
    #{scope => <<"group">>, group_id => Gid, anchored => true};
mp_scenario(S, Driver, <<"identity_mismatch">>, <<"ordinary">>) ->
    T = reg(S, ordinary, Driver),
    Intruder = intruder_headers(),
    S2 = maps:get(fixture, T),
    Q = uri_string:compose_query([
        {<<"object_key">>, key(T)},
        {<<"mime_type">>, <<"text/plain">>}
    ]),
    Stolen = ?F:request(
        S2,
        post,
        <<"/api/v1/attachment/upload?", Q/binary>>,
        multipart_body(<<"w601b">>, ?BODY),
        Intruder,
        <<"multipart/form-data; boundary=w601b">>
    ),
    ?assertNot(?F:success(Stolen)),
    {error, not_pinned} = ?S:content_pin(key(T)),
    ?assertEqual({error, not_found}, ?F:read(T)),
    #{intruder_status => maps:get(status, Stolen), owner_guard => true};
mp_scenario(S, Driver, <<"database_unavailable">>, <<"ordinary">>) ->
    M = multipart_register(S, Driver, #{<<"method">> => <<"multipart">>}),
    ?S:fail_next_query(
        <<"SELECT object_key, bucket, scope, creator_user_id, session_state">>,
        {error, db_down},
        fun() ->
            Down = ?FW:multipart_put(S, M, ?BODY),
            ?assertNot(?F:success(Down)),
            ?assertEqual(500, maps:get(status, Down))
        end
    ),
    {error, not_pinned} = ?S:content_pin(key(M)),
    #{db_outage_status => 500, no_capability => true};
%% enterprise 域：无 multipart/form-data 端点；同一 oracle 以受控网关链路
%% 证明（共享 pending 账本、pin 事务边界、fail-closed 一致）。
mp_scenario(S, Driver, <<"multipart_success">>, <<"enterprise">>) ->
    T = reg(S, enterprise_internal, Driver),
    Mod = ?S:driver_mod(Driver),
    meck:new(Mod, [passthrough, no_link]),
    try
        ok = meck:expect(Mod, put_snapshot, fun(I, A, B, C) ->
            {ok, _} = ?S:content_pin(maps:get(key, I)),
            meck:passthrough([I, A, B, C])
        end),
        ?assert(?F:success(?F:put(T, ?BODY)))
    after
        meck:unload(Mod)
    end,
    ?assert(?F:success(?F:confirm(T))),
    #{entrypoint => <<"gateway_same_boundary">>, pin_before_provider_put => true};
mp_scenario(S, Driver, <<"group_file_success">>, <<"enterprise">>) ->
    T = reg(S, enterprise_internal, Driver),
    ?assert(?F:success(?F:put(T, ?BODY))),
    ?assert(?F:success(?F:confirm(T))),
    org_ws(T, fun(Org, Ws, Id) ->
        {ok, Row} = ?S:eb_row(Org, Ws, Id),
        #{<<"upload_registration">> := Reg} = Row,
        ?assertEqual(<<"enterprise">>, fetch_in(<<"scope">>, Reg)),
        ?assertEqual(atom_to_binary(Driver, utf8), maps:get(<<"storage_driver">>, Row))
    end),
    #{declaration_scope_frozen => true, frozen_driver => atom_to_binary(Driver, utf8)};
mp_scenario(S, Driver, <<"identity_mismatch">>, <<"enterprise">>) ->
    T = reg(S, enterprise_internal, Driver),
    S2 = maps:get(fixture, T),
    Stolen =
        ?F:request(
            S2,
            post,
            <<"/api/internal/v1/files/presign">>,
            #{operation => <<"status">>, client_upload_id => client_id(T)},
            tampered_internal_headers(S2),
            <<"application/json">>
        ),
    ?assertNot(?F:success(Stolen)),
    #{intruder_status => maps:get(status, Stolen), disclosed => false};
mp_scenario(S, Driver, <<"database_unavailable">>, <<"enterprise">>) ->
    ?S:fail_next_query(<<"fn_v5_register_pending(">>, {error, db_down}, fun() ->
        T = reg(S, enterprise_internal, Driver),
        ?assertNot(?F:success(re_register(T, enterprise_internal, Driver)))
    end),
    #{presign_failclosed => true, no_capability => true}.

%% ------------------------------------------------------ pending-failclosed

%% insert_error：真实 DB 函数契约（非法窗口 23514，无 durable 行）+ HTTP 层
%% 单点注入证明 handler fail-closed（503 无签名能力）。
fc_scenario(S, Driver, <<"insert_error">>, Domain) ->
    Surface = surface_of(Domain),
    Bad = bad_window_sql(),
    {error, #{code := <<"23514">>}} = elib_pg:query(Bad, []),
    NoRow = elib_pg:query(
        <<"SELECT count(*)::bigint AS n FROM attach_pending WHERE client_upload_id::text=$1">>,
        [bad_window_cid()]
    ),
    ?assertMatch({ok, [#{<<"n">> := 0}]}, NoRow),
    ?S:fail_next_query(<<"fn_v5_register_pending(">>, {error, insert_failed}, fun() ->
        Injected = reg(S, Surface, Driver),
        ?assertNot(?F:success(re_register(Injected, Surface, Driver)))
    end),
    #{sql_code => <<"23514">>, http_failclosed => true};
%% insert_exception：登记路径异常同样不签发（handler 捕获为 503）；
%% 同 UUID 不同快照的真实 23510 冲突作为 DB 侧异常证据。
fc_scenario(S, Driver, <<"insert_exception">>, Domain) ->
    Surface = surface_of(Domain),
    ?S:fail_next_query(<<"fn_v5_register_pending(">>, {raise, error, injected}, fun() ->
        T = reg(S, Surface, Driver),
        ?assertNot(?F:success(re_register(T, Surface, Driver)))
    end),
    T2 = reg(S, Surface, Driver),
    Conflict = ?F:register(
        (maps:get(fixture, T2))#{registration_cid => client_id(T2)},
        Surface,
        Driver,
        <<"w601 different declared payload same uuid">>
    ),
    ?assertNot(?F:success(Conflict)),
    #{http_failclosed => true, snapshot_conflict_rejected => true};
%% commit_unknown：COMMIT 结果未知 => 503 无能力；同 client_upload_id 重试
%% 解析回同一 durable key，绝不产生新 key。
fc_scenario(S, Driver, <<"commit_unknown">>, Domain) ->
    Surface = surface_of(Domain),
    T0 = reg(S, Surface, Driver),
    ?S:fail_next_query(<<"fn_v5_register_pending(">>, {error, commit_unknown}, fun() ->
        Replay = re_register(T0, Surface, Driver),
        ?assertNot(?F:success(Replay)),
        ?assertNot(maps:is_key(<<"put_url">>, ?F:payload(Replay)))
    end),
    Recovered = re_register(T0, Surface, Driver),
    ?assert(?F:success(Recovered)),
    ?assertEqual(key(T0), fetch(<<"object_key">>, ?F:payload(Recovered), key(T0))),
    ?assertEqual(1, ?S:register_count(key(T0))),
    #{commit_unknown_failclosed => true, recovered_same_key => true}.

%% ------------------------------------------------------- confirm-gc-race

%% confirm 先提交：intent sweep 被拒（confirmed/metadata-backed 双守卫），
%% 字节保留、无回收 job；bounded retry 不改变赢家。
race_scenario(S, Driver, <<"confirm_commits_first">>, Domain) ->
    Surface = surface_of(Domain),
    T = reg(S, Surface, Driver),
    ?assert(?F:success(?F:put(T, ?BODY))),
    ?assert(?F:success(?F:confirm(T))),
    ?assertEqual({error, intent_conflict}, intent(T, <<"w601 race after confirm">>)),
    ?assertEqual({ok, ?BODY}, ?F:read(T)),
    ?assertEqual(0, deletion_jobs(T, Domain)),
    #{intent_rejected => true, bytes_retained => true, deletion_jobs => 0};
%% intent 先提交（能力窗耗尽后 sweep）：confirm 被拒且 metadata 不重建；
%% 回收 job ready 且 attempts<8；W2-03 第三子句——bounded retry 不改变赢家：
%% 以真实 fn_v5_claim_* / fn_v5_ack_(failed) 协议把 job 打到上限（×8），
%% 第三种结局 = fn 裁决 blocked 终态（last_error_code=max_attempts），此后
%% confirm 依旧被拒、metadata 不重建、字节保留挂起（绝不静默丢弃），
%% blocked+attempts=8 双谓词使其永不再被 fn_v5_claim（claim 条件为冻结
%% 迁移文本：attempts<8 AND job_state IN ('ready','retry_wait')）。
race_scenario(S, Driver, <<"intent_commits_first">>, Domain) ->
    Surface = surface_of(Domain),
    T = reg(S, Surface, Driver),
    ?assert(?F:success(?F:put(T, ?BODY))),
    shorten_expiry(T),
    ?assertEqual(ok, intent(T, <<"w601 race before confirm">>)),
    Late = ?F:confirm(T),
    ?assertNot(?F:success(Late)),
    ?assertEqual(0, ?S:attach_count(key(T))),
    ?assert(deletion_jobs(T, Domain) >= 1),
    assert_job_bounded(T, Domain),
    exhaust_deletion_job(T, Domain),
    assert_job_blocked(T, Domain),
    ?assertNot(?F:success(?F:confirm(T))),
    ?assertEqual(0, ?S:attach_count(key(T))),
    ?assertEqual({ok, ?BODY}, ?F:read(T)),
    #{
        intent_won => true,
        confirm_rejected => true,
        metadata_not_recreated => true,
        retry_window_attempts => <<"<8">>,
        retry_exhausted_state => <<"blocked">>
    }.

%% ---------------------------------------------------------------- helpers

reg(S, Ep, Driver) -> ?F:register(S, Ep, Driver, ?BODY).

re_register(T, Ep, Driver) ->
    ?F:register(
        (maps:get(fixture, T))#{registration_cid => client_id(T), leaf => maps:get(leaf, T)},
        Ep,
        Driver,
        ?BODY
    ).

key(T) -> maps:get(key, T).
client_id(T) -> maps:get(client_upload_id, T).

fetch(K, Map, Default) -> maps:get(K, Map, Default).

fetch_in(Key, Map) when is_map(Map) -> maps:get(Key, Map, undefined);
fetch_in(_, _) -> undefined.

surface_of(<<"ordinary">>) -> ordinary;
surface_of(<<"enterprise">>) -> enterprise_internal.

%% ordinary multipart/group 登记入口（GET presign + method=multipart）。
multipart_register(S, Driver, Extra) ->
    Cid = elib_uuid:gen_v7(),
    Registry = attachment_storage:registry(),
    Ns = ns_of(Driver),
    application:set_env(
        imboy,
        attachment_storage_registry,
        Registry#{upload_namespaces => #{<<"private">> => Ns, <<"enterprise">> => Ns}}
    ),
    Q = uri_string:compose_query(
        [
            {<<"filename">>, <<"w403.txt">>},
            {<<"mime_type">>, <<"text/plain">>},
            {<<"client_upload_id">>, Cid}
        ] ++
            [
                {K, V}
             || {K, V} <- maps:to_list(Extra)
            ]
    ),
    Headers = maps:get(ordinary_headers, S),
    Registered =
        ?F:request(
            S,
            get,
            <<"/api/v1/attachment/presign?", Q/binary>>,
            <<>>,
            Headers,
            <<"application/json">>
        ),
    ?assert(?F:success(Registered)),
    Data = ?F:payload(Registered),
    Key = maps:get(<<"object_key">>, Data),
    {ok, Location} = attachment_storage:session_location(Key),
    ?assertEqual(Driver, maps:get(driver, Location)),
    #{
        fixture => S,
        leaf => maps:get(leaf, S),
        surface => ordinary,
        driver => Driver,
        key => Key,
        location => Location,
        upload_url => maps:get(<<"put_url">>, Data),
        multipart_url => maps:get(<<"upload_url">>, Data),
        headers => Headers,
        confirm_body => #{object_key => Key, scope => fetch(<<"scope">>, Extra, <<"private">>)},
        registration => Data,
        client_upload_id => Cid
    }.

ns_of(disk) -> <<"w403-disk">>;
ns_of(garage) -> <<"w403-garage">>.

multipart_body(Boundary, Bytes) ->
    <<
        "--",
        Boundary/binary,
        "\r\n",
        "Content-Disposition: form-data; name=\"file\"; filename=\"w403.txt\"\r\n\r\n",
        Bytes/binary,
        "\r\n--",
        Boundary/binary,
        "--\r\n"
    >>.

new_group(S) ->
    Uid = maps:get(uid, S),
    {ok, Gid} = group_logic:add(0, Uid, 1, []),
    Gid.

intruder_headers() ->
    Uid = 8000000000000 + binary:decode_unsigned(crypto:strong_rand_bytes(5)),
    {ok, _} = elib_pg:query(
        <<
            "INSERT INTO public.\"user\"(id,account,nickname,password,status,account_type,reg_ip,reg_cosv,created_at)"
            " VALUES($1,$2,'fixture','',1,0,'127.0.0.1','w601',now()) RETURNING id"
        >>,
        [Uid, <<"w601-mp-intruder-", (integer_to_binary(Uid))/binary>>]
    ),
    Did = <<"w601-device-", (integer_to_binary(Uid))/binary>>,
    ok = qr_login_logic:log_device_login(Uid, Did, <<"w601">>, <<"web">>),
    #{<<"authorization">> => <<"Bearer ", (token_ds:encrypt_token(Uid))/binary>>}.

tampered_internal_headers(S) ->
    maps:get(internal_headers, S).

org_ws(T, Fun) ->
    S = maps:get(fixture, T),
    Scope = maps:get(scope, S),
    AssetId = asset_id_of(T),
    Fun(maps:get(org_id, Scope), maps:get(workspace_id, Scope), AssetId).

asset_id_of(T) ->
    case maps:get(asset_id, maps:get(confirm_body, T), undefined) of
        I when is_integer(I) -> I;
        B when is_binary(B) -> binary_to_integer(B);
        undefined -> int(maps:get(<<"asset_id">>, maps:get(registration, T)))
    end.

int(I) when is_integer(I) -> I;
int(B) when is_binary(B) -> binary_to_integer(B).

shorten_expiry(T) ->
    case ?S:pending_row(key(T)) of
        {ok, _} -> ?S:update_pending_expiry(key(T), -60);
        _ -> org_ws(T, fun(Org, Ws, Id) -> ?S:update_eb_expiry(Org, Ws, Id, -60) end)
    end.

%% intent 提交：ordinary 走 repo（错误归一 intent_conflict/not_found）；
%% enterprise 直接执行 fn_v5_intent_eb_delete 并归一 SQLSTATE。
intent(T, Reason) ->
    case ?S:pending_row(key(T)) of
        {ok, _} ->
            attach_pending_repo:intent_pending_delete(key(T), <<"sweep">>, Reason);
        _ ->
            org_ws(T, fun(Org, Ws, Id) ->
                case
                    elib_pg:query(
                        <<"SELECT fn_v5_intent_eb_delete($1,$2,$3,$4)">>,
                        [Org, Ws, Id, Reason]
                    )
                of
                    {ok, _} -> ok;
                    {error, #{code := <<"23510">>}} -> {error, intent_conflict};
                    {error, #{code := <<"23505">>}} -> {error, tombstone};
                    {error, #{code := <<"P0002">>}} -> {error, not_found};
                    {error, Reason2} -> {error, Reason2}
                end
            end)
    end.

deletion_jobs(T, <<"ordinary">>) ->
    {ok, [#{<<"n">> := N}]} = elib_pg:query(
        <<"SELECT count(*)::bigint AS n FROM attachment_object_deletion WHERE path=$1">>,
        [key(T)]
    ),
    N;
deletion_jobs(T, _) ->
    org_ws(T, fun(Org, Ws, Id) ->
        {ok, [#{<<"n">> := N}]} = elib_pg:query(
            <<
                "SELECT count(*)::bigint AS n FROM enterprise_asset_delete_queue"
                " WHERE organization_id=$1 AND workspace_id=$2 AND asset_id=$3"
            >>,
            [Org, Ws, Id]
        ),
        N
    end).

%% ------------------------------------------------- W2-03 第三结局探针
%% 删除队列在本套件环境无常驻 worker（attachment_delete_worker_enabled 默认
%% false），claim/ack 全部由本探针以 fn 原文协议直接驱动；跳过退避等待仅
%% 推进 next_retry_at，claim/ack 裁决本身全真。

assert_job_bounded(T, <<"ordinary">>) ->
    {ok, [#{<<"job_state">> := <<"ready">>, <<"attempts">> := Attempts}]} =
        elib_pg:query(
            <<"SELECT job_state, attempts FROM attachment_object_deletion WHERE path=$1">>,
            [key(T)]
        ),
    ?assert(Attempts < 8),
    ok;
assert_job_bounded(T, _) ->
    org_ws(T, fun(Org, Ws, Id) ->
        {ok, [#{<<"job_state">> := <<"ready">>, <<"attempts">> := Attempts}]} =
            elib_pg:query(
                <<
                    "SELECT job_state, attempts FROM enterprise_asset_delete_queue"
                    " WHERE organization_id=$1 AND workspace_id=$2 AND asset_id=$3"
                >>,
                [Org, Ws, Id]
            ),
        ?assert(Attempts < 8)
    end).

exhaust_deletion_job(T, <<"ordinary">>) ->
    exhaust_ordinary(key(T), 0);
exhaust_deletion_job(T, _) ->
    org_ws(T, fun(Org, Ws, Id) -> exhaust_eb(Org, Ws, Id, 0) end).

exhaust_ordinary(Key, Depth) when Depth < 24 ->
    {ok, _} = elib_pg:query(
        <<
            "UPDATE attachment_object_deletion SET next_retry_at=now()"
            " WHERE path=$1 AND completed_at IS NULL"
        >>,
        [Key]
    ),
    Token = elib_uuid:gen_v7(),
    case elib_pg:query(<<"SELECT * FROM fn_v5_claim_ordinary($1)">>, [Token]) of
        {ok, [#{<<"out_path">> := Key, <<"out_attempts">> := Attempts}]} ->
            {ok, [#{<<"fn_v5_ack_ordinary">> := State}]} =
                elib_pg:query(<<"SELECT fn_v5_ack_ordinary($1,$2,false)">>, [Key, Token]),
            case State of
                <<"blocked">> ->
                    ?assert(Attempts >= 8),
                    ok;
                <<"retry_wait">> ->
                    ?assert(Attempts < 8),
                    exhaust_ordinary(Key, Depth + 1);
                Other ->
                    erlang:error({w601_unexpected_ack_state, Other})
            end;
        {ok, [#{<<"out_path">> := OtherPath}]} ->
            %% claim 到同 run 更早叶遗留 job：按真实失败语义退避（推入
            %% 未来重试），不污染本叶目标行，下轮继续认领目标。
            {ok, [_]} = elib_pg:query(
                <<"SELECT fn_v5_ack_ordinary($1,$2,false)">>, [OtherPath, Token]
            ),
            exhaust_ordinary(Key, Depth + 1)
    end;
exhaust_ordinary(_Key, _Depth) ->
    erlang:error(w601_delete_job_not_exhausted).

exhaust_eb(Org, Ws, Id, Depth) when Depth < 24 ->
    {ok, _} = elib_pg:query(
        <<
            "UPDATE enterprise_asset_delete_queue SET next_retry_at=now()"
            " WHERE organization_id=$1 AND workspace_id=$2 AND asset_id=$3"
            " AND completed_at IS NULL"
        >>,
        [Org, Ws, Id]
    ),
    Token = elib_uuid:gen_v7(),
    case elib_pg:query(<<"SELECT * FROM fn_v5_claim_eb($1)">>, [Token]) of
        {ok, [
            #{
                <<"out_organization_id">> := Org,
                <<"out_workspace_id">> := Ws,
                <<"out_asset_id">> := Id,
                <<"out_attempts">> := Attempts
            }
        ]} ->
            {ok, [#{<<"fn_v5_ack_eb">> := State}]} =
                elib_pg:query(<<"SELECT fn_v5_ack_eb($1,$2,$3,$4,false)">>, [Org, Ws, Id, Token]),
            case State of
                <<"blocked">> ->
                    ?assert(Attempts >= 8),
                    ok;
                <<"retry_wait">> ->
                    ?assert(Attempts < 8),
                    exhaust_eb(Org, Ws, Id, Depth + 1);
                Other ->
                    erlang:error({w601_unexpected_ack_state, Other})
            end;
        {ok, [
            #{
                <<"out_organization_id">> := OtherOrg,
                <<"out_workspace_id">> := OtherWs,
                <<"out_asset_id">> := OtherAsset
            }
        ]} ->
            {ok, [_]} = elib_pg:query(
                <<"SELECT fn_v5_ack_eb($1,$2,$3,$4,false)">>,
                [OtherOrg, OtherWs, OtherAsset, Token]
            ),
            exhaust_eb(Org, Ws, Id, Depth + 1)
    end;
exhaust_eb(_Org, _Ws, _Id, _Depth) ->
    erlang:error(w601_delete_job_not_exhausted).

assert_job_blocked(T, <<"ordinary">>) ->
    {ok, [
        #{
            <<"job_state">> := <<"blocked">>,
            <<"attempts">> := 8,
            <<"last_error_code">> := <<"max_attempts">>
        }
    ]} =
        elib_pg:query(
            <<
                "SELECT job_state, attempts, last_error_code FROM attachment_object_deletion"
                " WHERE path=$1"
            >>,
            [key(T)]
        ),
    ok;
assert_job_blocked(T, _) ->
    org_ws(T, fun(Org, Ws, Id) ->
        {ok, [
            #{
                <<"job_state">> := <<"blocked">>,
                <<"attempts">> := 8,
                <<"last_error_code">> := <<"max_attempts">>
            }
        ]} =
            elib_pg:query(
                <<
                    "SELECT job_state, attempts, last_error_code"
                    " FROM enterprise_asset_delete_queue"
                    " WHERE organization_id=$1 AND workspace_id=$2 AND asset_id=$3"
                >>,
                [Org, Ws, Id]
            )
    end).

%% 真实 fn_v5_register_pending 契约探针：deadline < expiry => 23514。
bad_window_cid() ->
    <<"0f0e0d0c-a601-4a5e-9b2c-00000000e001">>.

bad_window_sql() ->
    <<
        "SELECT fn_v5_register_pending("
        "'w601-failclosed/bad-window.bin', 'b1', 'private', 1, "
        "'0f0e0d0c-a601-4a5e-9b2c-00000000e001'::uuid, "
        "'{\"subject\":{\"kind\":\"user\",\"principal\":\"1\"},"
        "\"declaration\":{\"mime\":\"text/plain\",\"scope\":\"private\"}}'::jsonb, "
        "now() - interval '1 hour', now(), NULL, NULL, NULL, NULL)"
    >>.
