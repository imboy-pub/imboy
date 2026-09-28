-module(adm_owner_activation_tests).
-compile([nowarn_deprecated_catch]).
-include_lib("eunit/include/eunit.hrl").

%%% 待激活 Owner 全链路（GZAPP-06 / J02；真库 marker 套件）
%%%
%%% 覆盖面（产品决策 D11-D13 + §4.1）：
%%%   * 创建（pending_phone）：预创建不可登录 Human（status=0/account_type=0）+
%%%     org+membership+default ws+invite 全落库；响应脱敏 + TSID string；
%%%     token 只存 digest（SHA-256 hex，全局唯一）；
%%%   * D12 失败不回滚：fake 注入失败（000 前缀）→ invite=sms_failed 但
%%%     org 创建成功；可重发；
%%%   * 30 天 TTL：expires_at ≈ now+30d；到期不删除（expired 谓词），
%%%     reactivate 刷新 TTL 回 pending；
%%%   * 重发：幂等重入 + resend_count++ + token 轮换（旧 token 即时失效）；
%%%   * 激活消费：单次 CAS（consumed_at 恰写一次）+ user status 0→1；
%%%     重复消费 409 / 过期 409 / 无效 token 404；
%%%   * 换 Owner（D13）：恰一 active owner 不量变；目标已注册 → 直接转移；
%%%     目标新手机号 → pending 转移 + 旧 invite superseded；事务失败整体回滚；
%%%   * 手机号 PII：响应/审计（admin_operation_logs.detail）绝不含 mobile 原文。
%%%
%%% 库供给契约：inttest_marker_db 全链迁移（真 PG 127.0.0.1:4323，本卡专用
%%% 一次性 scratch 库，env 前缀 GZAPP06）；SMS 平台固定 fake。

-define(WRITE_UID, 9301).
-define(RO_UID, 9303).

%% ------------------------------------------------------------------
%% Mock 套装（照 adm_organization_create_tests 口径）
%% ------------------------------------------------------------------

-define(MOCK_NO_PLUGIN,
    {imboy_plugin_registry, [
        {'required_feature', 3, fun(_Type, _Handler, _Action) -> undefined end}
    ]}
).

-define(MOCK_RESP,
    {elib_response, [
        {'success', 2, fun(Req, Payload) -> Req#{response_status => 200, payload => Payload} end},
        {'success', 3, fun(Req, Payload, _Msg) ->
            Req#{response_status => 200, payload => Payload}
        end},
        {'error', 3, fun(Req, Msg, Code) -> Req#{response_status => Code, error_msg => Msg} end}
    ]}
).

-define(MOCK_COWBOY,
    {cowboy_req, [
        {'method', 1, fun(Req) -> maps:get(method, Req, <<"GET">>) end},
        {'binding', 3, fun(Key, Req, Default) ->
            maps:get(Key, maps:get(bindings, Req, #{}), Default)
        end},
        {'parse_qs', 1, fun(Req) -> maps:get(qs, Req, []) end},
        {'read_body', 1, fun(Req) -> {ok, maps:get(body, Req, <<>>), Req} end},
        {'path', 1, fun(_Req) -> <<"/api/adm/organizations">> end},
        {'set_resp_cookie', 4, fun(_Name, _Val, Req, _Opts) -> Req end},
        {'reply', 3, fun(Status, _Headers, Req) -> Req#{response_status => Status} end},
        {'reply', 4, fun(Status, _Headers, _Body, Req) -> Req#{response_status => Status} end}
    ]}
).

-define(MOCK_ACL,
    {adm_user_logic, [
        {'find', 3, fun
            (?WRITE_UID, <<"id,role_id">>, _Key) ->
                #{<<"id">> => ?WRITE_UID, <<"role_id">> => [1]};
            (?RO_UID, <<"id,role_id">>, _Key) ->
                #{<<"id">> => ?RO_UID, <<"role_id">> => [3]};
            (_Other, <<"id,role_id">>, _Key) ->
                #{<<"id">> => 0, <<"role_id">> => [99]}
        end}
    ]}
).

-define(MOCK_ROLE_ACL,
    {adm_index_handler, [
        {'role_acl', 1, fun
            (1) ->
                {<<"super_admin">>, [<<"organizations:read">>, <<"organizations:write">>], []};
            (3) ->
                {<<"audit_admin">>, [<<"organizations:read">>], []};
            (_) ->
                {<<"none">>, [], []}
        end}
    ]}
).

-define(MOCK_PEER_IP,
    {elib_req, [
        {'peer_ip', 1, fun(_Req) -> <<"127.0.0.1">> end}
    ]}
).

-define(ADM_MOCKS, [
    ?MOCK_NO_PLUGIN,
    ?MOCK_RESP,
    ?MOCK_COWBOY,
    ?MOCK_ACL,
    ?MOCK_ROLE_ACL,
    ?MOCK_PEER_IP
]).

%% ===================================================================
%% 套件入口（真库 marker 套件）
%% ===================================================================

adm_owner_activation_test_() ->
    {timeout, 900,
        {setup, fun setup_db/0, fun close_db/1, fun(_Db) ->
            {foreach, fun mocks_on/0, fun(_S) -> mocks_off() end, [
                fun(_S) -> create_pending_success_tests() end,
                fun(_S) -> create_sms_failed_tests() end,
                fun(_S) -> create_idempotent_tests() end,
                fun(_S) -> create_conflict_tests() end,
                fun(_S) -> resend_tests() end,
                fun(_S) -> reactivate_ttl_tests() end,
                fun(_S) -> consume_single_use_tests() end,
                fun(_S) -> transfer_by_phone_tests() end,
                fun(_S) -> status_and_acl_tests() end
            ]}
        end}}.

setup_db() ->
    _ = (catch elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3})),
    ok = elib_tsid:register([
        admin_op_log, organization, workspace, user, owner_activation_invite
    ]),
    ensure_depcache(),
    State =
        inttest_marker_db:provision(#{
            env_prefix => <<"GZAPP06">>,
            connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
        }),
    %% 短信平台固定 fake（真实发送 = BLOCKED_EXTERNAL_CONFIRMATION）。
    %% 必须在 provision 之后：provision 内 application:load(imboy) 会以
    %% -config（sys.local 的 sms.platform=aliyun）覆盖先设的 env。
    application:set_env(imboy, sms, [{platform, <<"fake">>}]),
    ok = imboy_sms_fake:outbox_clear(),
    {ok, _} = application:ensure_all_started(pooler),
    #{server := Server, db := Db} = State,
    PoolConf = pgsql_pool_conf(Server, Db),
    case pooler:new_pool(PoolConf) of
        {ok, _Pid} ->
            State;
        {error, {already_started, _}} ->
            %% A1c（CP-TD-A02）：共享 VM 里 app 的 pgsql 池（指向共享库）已就位，
            %% 而产品代码 elib_pg:with_conn 硬编码 take_member(pgsql)——套件池必须
            %% 同名。接管：先停 app 池、换挂本套件 marker 库池；close_db/1 再按
            %% imboy pg_conf 重建 app 池还原共享 VM 状态，后续套件不受影响。
            ok = pool_swap('pgsql', PoolConf, 20),
            State
    end.

%% A1c：rm_pool/new_pool 均有异步窗口（rm 后名称短暂残留 already_present；
%% 成员占用中 rm 返回 running）——重试收敛，杜绝接管竞态。
%% rm_pool/new_pool 均有异步窗口：rm 后名称短暂残留（already_present）、
%% 成员占用中 rm 返回 running——单发必竞态。交替「rm→new」重试直到新池
%% （指向目标 conf）真正建立，杜绝接管/还原竞态（run19 实证）。
pool_swap(_Pool, _Conf, 0) ->
    erlang:error({pool_swap_failed, 'pgsql'});
pool_swap(Pool, Conf, N) ->
    _ = pooler:rm_pool(Pool),
    timer:sleep(200),
    case pooler:new_pool(Conf) of
        {ok, _Pid} ->
            ok;
        {error, {already_started, _}} ->
            timer:sleep(300),
            pool_swap(Pool, Conf, N - 1);
        {error, Other} ->
            erlang:error({pool_swap_failed, Other})
    end.

pgsql_pool_conf(#{host := Host, port := Port, username := User, password := Pass}, Db) ->
    #{
        name => pgsql,
        max_count => 4,
        init_count => 1,
        start_mfa =>
            {epgsql, connect, [
                #{
                    host => Host,
                    port => Port,
                    username => User,
                    password => Pass,
                    database => Db,
                    timeout => 10000,
                    codecs => [{epgsql_codec_rfc3339_bin, []}]
                }
            ]}
    }.

close_db(State) ->
    try pooler:rm_pool(pgsql) catch _:_ -> ok end, %% A1c: rm_pool/1 is the correct API (stop_pool/1 does not exist, the original try/catch had been silently swallowing undef)
    %% A1c（CP-TD-A02）：按 imboy pg_conf 重建 app pgsql 池（还原共享 VM 状态，
    %% 见 setup_db 接管注释），后续套件的 elib_pg 访问不受本套件影响。
    case application:get_env(imboy, pg_conf) of
        {ok, PgConf} when is_map(PgConf) ->
            ok = pool_swap('pgsql', PgConf, 20),
            ok;
        _ ->
            ok
    end,
    application:unset_env(imboy, sms),
    inttest_marker_db:release(State).

mocks_on() ->
    %% 双保险：确保 sms 平台为 fake（防止任何夹具中途改写 env）
    application:set_env(imboy, sms, [{platform, <<"fake">>}]),
    ok = imboy_sms_fake:outbox_clear(),
    lists:foreach(
        fun({Module, Expectations}) ->
            case meck_helper:setup_mock(Module, Expectations) of
                {ok, _} -> ok;
                {error, Reason} -> erlang:error({mock_setup_failed, Module, Reason})
            end
        end,
        ?ADM_MOCKS
    ).

mocks_off() ->
    lists:foreach(fun({Module, _}) -> meck_helper:cleanup_mock(Module) end, ?ADM_MOCKS).

%% ===================================================================
%% 组 1：创建成功路径（含 PII 脱敏 / digest / 审计）
%% ===================================================================

create_pending_success_tests() ->
    {"创建成功：预创建 Human + org/ws/invite 全落库 + 脱敏 + digest + 审计", fun() ->
        Mobile = <<"13811112222">>,
        Name = <<"gz06-create-ok">>,
        RespReq = create_pending_org(#{
            <<"name">> => Name,
            <<"owner_mobile">> => Mobile,
            <<"default_workspace_name">> => <<"默认工作区"/utf8>>
        }),
        ?assertEqual(200, status_of(RespReq)),
        Payload = payload_of(RespReq),
        ?assertEqual(true, maps:get(<<"created">>, Payload)),
        ?assertEqual(true, maps:get(<<"sms_sent">>, Payload)),
        %% PII：整个 payload 不得含 mobile 原文
        ?assertEqual(nomatch, binary:match(jsone:encode(Payload), [Mobile])),
        Activation = maps:get(<<"owner_activation">>, Payload),
        ?assertEqual(<<"pending">>, maps:get(<<"status">>, Activation)),
        ?assertEqual(<<"138****2222">>, maps:get(<<"mobile_masked">>, Activation)),
        Token = maps:get(<<"activation_token">>, Activation),
        ?assert(is_binary(Token) andalso byte_size(Token) =:= 64),
        ?assert(is_binary(maps:get(<<"invite_id">>, Activation))),
        Org = maps:get(<<"organization">>, Payload),
        OrgId = binary_to_integer(maps:get(<<"id">>, Org)),
        OwnerUid = binary_to_integer(maps:get(<<"owner_id">>, Org)),
        %% DB：预创建 Human（status=0 不可登录 / account_type=0）
        ?assertMatch(
            {ok, [#{<<"status">> := 0, <<"account_type">> := 0, <<"mobile">> := Mobile}]},
            q(
                conn(),
                <<"SELECT status, account_type, mobile FROM \"user\" WHERE id = $1">>,
                [OwnerUid]
            )
        ),
        %% DB：恰一 active owner member == org.owner_id（D13 不变量基线）
        ?assertMatch(
            {ok, [#{<<"owner_rows">> := 1, <<"owner_uid">> := OwnerUid}]},
            q(
                conn(),
                <<"SELECT count(*) AS owner_rows, min(user_id) AS owner_uid",
                    " FROM organization_member WHERE organization_id = $1",
                    " AND role = 'owner' AND status = 'active'">>,
                [OrgId]
            )
        ),
        %% DB：invite 只存 digest（sha256 hex）；TTL ≈ 30d
        ExpectedDigest = organization_invitation:token_digest(Token),
        ?assertMatch(
            {ok, [
                #{
                    <<"token_digest">> := ExpectedDigest,
                    <<"status">> := <<"pending">>,
                    <<"resend_count">> := 0,
                    <<"consumed_at">> := null
                }
            ]},
            q(
                conn(),
                <<"SELECT token_digest, status, resend_count, consumed_at,",
                    " extract(epoch from (expires_at - now()))::bigint AS ttl",
                    " FROM owner_activation_invite WHERE organization_id = $1">>,
                [OrgId]
            )
        ),
        {ok, [#{<<"ttl">> := Ttl}]} =
            one(
                conn(),
                <<"SELECT extract(epoch from (expires_at - now()))::bigint AS ttl",
                    " FROM owner_activation_invite WHERE organization_id = $1">>,
                [OrgId]
            ),
        ?assert(Ttl =< 30 * 86400 andalso Ttl > 30 * 86400 - 120),
        %% fake outbox：恰一条待发（mobile + token）
        ?assertMatch(
            [{Mobile, Token, Name, _Ts}], [E || E <- imboy_sms_fake:outbox_snapshot()]
        ),
        %% 审计：organization_create 落行且 detail 不含 mobile 原文（只有 masked）
        {ok, [#{<<"detail">> := DetailText}]} =
            one(
                conn(),
                <<"SELECT detail::text AS detail FROM admin_operation_logs",
                    " WHERE target_id = $1 AND action = 'organization_create'",
                    " ORDER BY id DESC LIMIT 1">>,
                [OrgId]
            ),
        ?assertEqual(nomatch, binary:match(DetailText, [Mobile])),
        %% A1c：审计 detail 契约 = 基础键（action/organization_id/…，见
        %% organization_admin_logic 审计写入口），不落 mobile（原文或 masked
        %% 都不落——masked 只出现在 HTTP 响应的 owner_activation.mobile_masked）。
        %% 断言 detail 是含 create 键的有效负载即可。
        ?assertNotEqual(nomatch, binary:match(DetailText, [<<"organization_id">>])),
        ?assertNotEqual(nomatch, binary:match(DetailText, [<<"action">>])),
        ok
    end}.

%% ===================================================================
%% 组 2：D12 短信失败不回滚 + 可重发
%% ===================================================================

create_sms_failed_tests() ->
    {"fake 注入失败（000 前缀）：org 创建成功 + invite=sms_failed（D12）", fun() ->
        Mobile = <<"00012345678">>,
        RespReq = create_pending_org(#{
            <<"name">> => <<"gz06-sms-failed">>,
            <<"owner_mobile">> => Mobile,
            <<"default_workspace_name">> => <<"ws">>
        }),
        ?assertEqual(200, status_of(RespReq)),
        Payload = payload_of(RespReq),
        ?assertEqual(true, maps:get(<<"created">>, Payload)),
        ?assertEqual(false, maps:get(<<"sms_sent">>, Payload)),
        ?assertEqual(
            <<"sms_failed">>, maps:get(<<"status">>, maps:get(<<"owner_activation">>, Payload))
        ),
        Org = maps:get(<<"organization">>, Payload),
        OrgId = binary_to_integer(maps:get(<<"id">>, Org)),
        ?assertMatch(
            {ok, [#{<<"status">> := <<"sms_failed">>}]},
            q(
                conn(),
                <<"SELECT status FROM owner_activation_invite WHERE organization_id = $1">>,
                [OrgId]
            )
        ),
        %% org/member/ws 均已落库（不回滚）
        ?assertMatch(
            {ok, [#{<<"status">> := <<"active">>}]},
            q(conn(), <<"SELECT status FROM organization WHERE id = $1">>, [OrgId])
        ),
        %% 000 号码不出现在 outbox
        ?assertEqual([], imboy_sms_fake:outbox_snapshot()),
        ok
    end}.

%% ===================================================================
%% 组 3：幂等（同 mobile 同名 → created=false，不重发不吐新 token）
%% ===================================================================

create_idempotent_tests() ->
    {"幂等：同手机号同名变体 created=false，invite 复用，无新 token/无重发", fun() ->
        Mobile = <<"13833334444">>,
        Body1 = #{
            <<"name">> => <<"gz06-idem">>,
            <<"owner_mobile">> => Mobile,
            <<"default_workspace_name">> => <<"ws-first">>
        },
        Resp1 = create_pending_org(Body1),
        ?assertEqual(200, status_of(Resp1)),
        P1 = payload_of(Resp1),
        Token1 = maps:get(<<"activation_token">>, maps:get(<<"owner_activation">>, P1)),
        Variant = Body1#{
            <<"name">> := <<"  GZ06-IDEM ">>,
            <<"default_workspace_name">> := <<"ws-second">>
        },
        Resp2 = create_pending_org(Variant),
        ?assertEqual(200, status_of(Resp2)),
        P2 = payload_of(Resp2),
        ?assertEqual(false, maps:get(<<"created">>, P2)),
        ?assertEqual(false, maps:get(<<"sms_sent">>, P2)),
        ?assertEqual(
            maps:get(<<"id">>, maps:get(<<"organization">>, P1)),
            maps:get(<<"id">>, maps:get(<<"organization">>, P2))
        ),
        %% 幂等视图：live invite 复用，但不携带新 token 键（防旧 token 复出）
        Activation2 = maps:get(<<"owner_activation">>, P2),
        ?assertMatch(#{<<"invite_id">> := _}, Activation2),
        ?assertNot(maps:is_key(<<"activation_token">>, Activation2)),
        %% 库内单 org 单 invite；outbox 恰 1（第二次不重发）
        ?assertMatch(
            {ok, [#{<<"count">> := 1}]},
            one(
                conn(),
                <<"SELECT count(*) AS count FROM owner_activation_invite", " WHERE mobile = $1">>,
                [Mobile]
            )
        ),
        OutboxForMobile = [E || {M, _, _, _} = E <- imboy_sms_fake:outbox_snapshot(), M =:= Mobile],
        ?assertEqual(1, length(OutboxForMobile)),
        ok
    end}.

%% 语义护栏：幂等响应携带 token 键即失败（防旧 token 复出）——见 is_key 断言。

%% ===================================================================
%% 组 4：注册活跃用户手机号冲突 → 409 引导已注册模式
%% ===================================================================

create_conflict_tests() ->
    {"活跃注册用户手机号 → 409 引导已注册模式 + 零残留", fun() ->
        Conn = conn(),
        Mobile = <<"13855556666">>,
        Uid = new_id(),
        ok = seed_registered_user(Conn, Uid, Mobile),
        RespReq = create_pending_org(#{
            <<"name">> => <<"gz06-conflict">>,
            <<"owner_mobile">> => Mobile,
            <<"default_workspace_name">> => <<"ws">>
        }),
        ?assertEqual(409, status_of(RespReq)),
        %% 零残留：该 owner 名下无 org、无 invite
        ?assertMatch(
            {ok, [#{<<"count">> := 0}]},
            one(conn(), <<"SELECT count(*) AS count FROM organization WHERE owner_id = $1">>, [Uid])
        ),
        ?assertMatch(
            {ok, [#{<<"count">> := 1}]},
            one(
                conn(), <<"SELECT count(*) AS count FROM \"user\" WHERE mobile = $1">>, [Mobile]
            )
        ),
        ok
    end}.

%% ===================================================================
%% 组 5：重发（幂等重入 + resend_count++ + token 轮换 + 旧 token 失效）
%% ===================================================================

resend_tests() ->
    {"重发：resend_count++、token 轮换、旧 token 即时失效、无 live invite 404", fun() ->
        Mobile = <<"13877778888">>,
        Resp1 = create_pending_org(#{
            <<"name">> => <<"gz06-resend">>,
            <<"owner_mobile">> => Mobile,
            <<"default_workspace_name">> => <<"ws">>
        }),
        ?assertEqual(200, status_of(Resp1)),
        OrgId = org_id_of(payload_of(Resp1)),
        Token1 = maps:get(
            <<"activation_token">>, maps:get(<<"owner_activation">>, payload_of(Resp1))
        ),
        %% 重发（正常号码 → fake ok）
        Resp2 = call_owner_action(?WRITE_UID, <<"POST">>, OrgId, owner_activation_resend, #{}),
        ?assertEqual(200, status_of(Resp2)),
        P2 = payload_of(Resp2),
        ?assertEqual(true, maps:get(<<"sms_sent">>, P2)),
        Invite2 = maps:get(<<"invite">>, P2),
        ?assertEqual(1, maps:get(<<"resend_count">>, Invite2)),
        ?assertEqual(<<"pending">>, maps:get(<<"status">>, Invite2)),
        Token2 = maps:get(<<"activation_token">>, Invite2),
        ?assert(is_binary(Token2) andalso Token2 =/= Token1),
        %% 库内 digest 已轮换且仍是单行
        {ok, [#{<<"digest">> := DbDigest}]} =
            one(
                conn(),
                <<"SELECT min(token_digest) AS digest FROM owner_activation_invite",
                    " WHERE organization_id = $1">>,
                [OrgId]
            ),
        ?assertEqual(organization_invitation:token_digest(Token2), DbDigest),
        %% 旧 token 消费 → 404（digest 已不在库）
        RespOld = call_owner_action(?WRITE_UID, <<"POST">>, OrgId, owner_activation_consume, #{
            <<"token">> => Token1
        }),
        ?assertEqual(404, status_of(RespOld)),
        %% 新 token 消费 → 200（单次）
        RespNew = call_owner_action(?WRITE_UID, <<"POST">>, OrgId, owner_activation_consume, #{
            <<"token">> => Token2
        }),
        ?assertEqual(200, status_of(RespNew)),
        %% 已 activated 后再重发 → 404（无 live invite）
        Resp3 = call_owner_action(?WRITE_UID, <<"POST">>, OrgId, owner_activation_resend, #{}),
        ?assertEqual(404, status_of(Resp3)),
        ok
    end}.

%% ===================================================================
%% 组 6：30 天 TTL（到期不删除 + reactivate 刷新回 pending）
%% ===================================================================

reactivate_ttl_tests() ->
    {"TTL：到期不删除（expired 谓词）→ reactivate 刷新 TTL 回 pending + token 轮换", fun() ->
        Mobile = <<"13899990000">>,
        Resp1 = create_pending_org(#{
            <<"name">> => <<"gz06-ttl">>,
            <<"owner_mobile">> => Mobile,
            <<"default_workspace_name">> => <<"ws">>
        }),
        ?assertEqual(200, status_of(Resp1)),
        OrgId = org_id_of(payload_of(Resp1)),
        %% 人工把 TTL 拨到过期（模拟 30 天后；行不删除）
        ok = exec(
            conn(),
            <<"UPDATE owner_activation_invite SET expires_at = now() - interval '1 hour'",
                " WHERE organization_id = $1">>,
            [OrgId]
        ),
        %% 状态卡：expired=true / ttl<=0，行仍在
        StatusReq = call_owner_action(?WRITE_UID, <<"GET">>, OrgId, owner_activation_show, #{}),
        ?assertEqual(200, status_of(StatusReq)),
        Status = payload_of(StatusReq),
        Invite = maps:get(<<"invite">>, Status),
        ?assertEqual(true, maps:get(<<"expired">>, Invite)),
        ?assert(maps:get(<<"ttl_remaining_seconds">>, Invite) =< 0),
        ?assertEqual(<<"pending">>, maps:get(<<"status">>, Invite)),
        %% 过期 token 消费 → 409 过期
        TokenOld = maps:get(
            <<"activation_token">>, maps:get(<<"owner_activation">>, payload_of(Resp1))
        ),
        RespExpired = call_owner_action(?WRITE_UID, <<"POST">>, OrgId, owner_activation_consume, #{
            <<"token">> => TokenOld
        }),
        ?assertEqual(409, status_of(RespExpired)),
        %% reactivate：刷新 TTL 回 pending + 轮换 token
        RespRe = call_owner_action(?WRITE_UID, <<"POST">>, OrgId, owner_activation_reactivate, #{}),
        ?assertEqual(200, status_of(RespRe)),
        PRe = payload_of(RespRe),
        ?assertEqual(true, maps:get(<<"sms_sent">>, PRe)),
        InviteRe = maps:get(<<"invite">>, PRe),
        ?assertEqual(<<"pending">>, maps:get(<<"status">>, InviteRe)),
        TokenNew = maps:get(<<"activation_token">>, InviteRe),
        ?assert(TokenNew =/= TokenOld),
        {ok, [#{<<"ttl">> := Ttl}]} =
            one(
                conn(),
                <<"SELECT extract(epoch from (expires_at - now()))::bigint AS ttl",
                    " FROM owner_activation_invite WHERE organization_id = $1">>,
                [OrgId]
            ),
        ?assert(Ttl =< 30 * 86400 andalso Ttl > 30 * 86400 - 120),
        %% 新 token 可消费（重激活链路闭环）
        RespConsume = call_owner_action(?WRITE_UID, <<"POST">>, OrgId, owner_activation_consume, #{
            <<"token">> => TokenNew
        }),
        ?assertEqual(200, status_of(RespConsume)),
        ok
    end}.

%% ===================================================================
%% 组 7：单次消费 CAS（consumed_at 恰写一次 + user 激活）
%% ===================================================================

consume_single_use_tests() ->
    {"消费：单次 CAS + user 0→1 + 重复 409 / 无效 404 / 消费后 owner_activated", fun() ->
        Mobile = <<"13711110001">>,
        Resp1 = create_pending_org(#{
            <<"name">> => <<"gz06-consume">>,
            <<"owner_mobile">> => Mobile,
            <<"default_workspace_name">> => <<"ws">>
        }),
        ?assertEqual(200, status_of(Resp1)),
        Payload = payload_of(Resp1),
        OrgId = org_id_of(Payload),
        OwnerUid = binary_to_integer(
            maps:get(<<"owner_id">>, maps:get(<<"organization">>, Payload))
        ),
        Token = maps:get(<<"activation_token">>, maps:get(<<"owner_activation">>, Payload)),
        %% 消费前：user 不可登录（status=0）
        ?assertMatch(
            {ok, [#{<<"status">> := 0}]},
            q(conn(), <<"SELECT status FROM \"user\" WHERE id = $1">>, [OwnerUid])
        ),
        %% 无效 token → 404
        RespBad = call_owner_action(?WRITE_UID, <<"POST">>, OrgId, owner_activation_consume, #{
            <<"token">> => binary:copy(<<"a">>, 64)
        }),
        ?assertEqual(404, status_of(RespBad)),
        %% 首次消费 → 200 + user 激活 + invite activated/consumed_at
        RespOk = call_owner_action(?WRITE_UID, <<"POST">>, OrgId, owner_activation_consume, #{
            <<"token">> => Token
        }),
        ?assertEqual(200, status_of(RespOk)),
        CP = payload_of(RespOk),
        ?assertEqual(OwnerUid, binary_to_integer(maps:get(<<"owner_user_id">>, CP))),
        ?assertMatch(
            {ok, [#{<<"status">> := 1}]},
            q(conn(), <<"SELECT status FROM \"user\" WHERE id = $1">>, [OwnerUid])
        ),
        ?assertMatch(
            {ok, [#{<<"status">> := <<"activated">>, <<"consumed">> := 1}]},
            q(
                conn(),
                <<"SELECT status, (consumed_at IS NOT NULL)::int AS consumed",
                    " FROM owner_activation_invite WHERE organization_id = $1">>,
                [OrgId]
            )
        ),
        %% 重复消费同一 token → 409
        RespAgain = call_owner_action(?WRITE_UID, <<"POST">>, OrgId, owner_activation_consume, #{
            <<"token">> => Token
        }),
        ?assertEqual(409, status_of(RespAgain)),
        %% 状态卡：owner_activated=true + 最新 invite activated
        StatusReq = call_owner_action(?WRITE_UID, <<"GET">>, OrgId, owner_activation_show, #{}),
        ?assertEqual(200, status_of(StatusReq)),
        Status = payload_of(StatusReq),
        ?assertEqual(true, maps:get(<<"owner_activated">>, Status)),
        ?assertEqual(<<"activated">>, maps:get(<<"status">>, maps:get(<<"invite">>, Status))),
        %% PII：状态卡不含 mobile 原文
        ?assertEqual(nomatch, binary:match(jsone:encode(Status), [Mobile])),
        ok
    end}.

%% ===================================================================
%% 组 8：按手机号换 Owner（D13）
%% ===================================================================

transfer_by_phone_tests() ->
    [
        {"换 Owner（新手机号 pending）：恰一不变量 + 旧 invite superseded + 消费闭环", fun() ->
            transfer_pending_to_pending()
        end},
        {"换 Owner（已注册用户）：direct_transfer 无邀请", fun() ->
            transfer_to_registered()
        end},
        {"换 Owner（当前 Owner 手机号）：400", fun() ->
            transfer_to_self()
        end},
        {"换 Owner（注销中用户）：409", fun() ->
            transfer_to_deleted()
        end},
        {"换 Owner 事务失败整体回滚（D13）", fun() ->
            transfer_rollback()
        end}
    ].

transfer_pending_to_pending() ->
    Mobile1 = <<"13722220001">>,
    Mobile2 = <<"13722220002">>,
    Resp1 = create_pending_org(#{
        <<"name">> => <<"gz06-transfer-p2p">>,
        <<"owner_mobile">> => Mobile1,
        <<"default_workspace_name">> => <<"ws">>
    }),
    ?assertEqual(200, status_of(Resp1)),
    P1 = payload_of(Resp1),
    OrgId = org_id_of(P1),
    OldOwnerUid = binary_to_integer(maps:get(<<"owner_id">>, maps:get(<<"organization">>, P1))),
    OldInviteId =
        binary_to_integer(
            maps:get(<<"invite_id">>, maps:get(<<"owner_activation">>, P1))
        ),
    Resp2 = transfer_by_phone(OrgId, Mobile2),
    ?assertEqual(200, status_of(Resp2)),
    P2 = payload_of(Resp2),
    ?assertEqual(<<"pending_transfer">>, maps:get(<<"mode">>, P2)),
    ?assertEqual(true, maps:get(<<"sms_sent">>, P2)),
    NewOwnerUid = binary_to_integer(maps:get(<<"owner_user_id">>, P2)),
    NewToken = maps:get(<<"activation_token">>, maps:get(<<"invite">>, P2)),
    ?assert(is_binary(NewToken) andalso byte_size(NewToken) =:= 64),
    %% 恰一 active owner == org.owner_id（D13）
    ?assertMatch(
        {ok, [
            #{<<"owner_id">> := NewOwnerUid, <<"owner_rows">> := 1, <<"old_role">> := <<"admin">>}
        ]},
        q(
            conn(),
            <<"SELECT o.owner_id,", " (SELECT count(*) FROM organization_member om2",
                "   WHERE om2.organization_id = o.id AND om2.role = 'owner'",
                "   AND om2.status = 'active') AS owner_rows,",
                " (SELECT om3.role FROM organization_member om3",
                "   WHERE om3.organization_id = o.id AND om3.user_id = $2) AS old_role",
                " FROM organization o WHERE o.id = $1">>,
            [OrgId, OldOwnerUid]
        )
    ),
    %% 旧 invite superseded；新 invite pending 指向新 owner
    ?assertMatch(
        {ok, [#{<<"status">> := <<"superseded">>}]},
        q(
            conn(),
            <<"SELECT status FROM owner_activation_invite WHERE id = $1">>,
            [OldInviteId]
        )
    ),
    ?assertMatch(
        {ok, [#{<<"status">> := <<"pending">>, <<"owner_user_id">> := NewOwnerUid}]},
        q(
            conn(),
            <<"SELECT status, owner_user_id FROM owner_activation_invite",
                " WHERE organization_id = $1 AND status IN ('pending','sms_failed')">>,
            [OrgId]
        )
    ),
    %% PII：转移响应不含任一手机号原文
    ?assertEqual(nomatch, binary:match(jsone:encode(P2), [Mobile1])),
    ?assertEqual(nomatch, binary:match(jsone:encode(P2), [Mobile2])),
    %% 新 token 消费 → 新 owner 激活（闭环）
    RespConsume = call_owner_action(?WRITE_UID, <<"POST">>, OrgId, owner_activation_consume, #{
        <<"token">> => NewToken
    }),
    ?assertEqual(200, status_of(RespConsume)),
    ?assertMatch(
        {ok, [#{<<"status">> := 1}]},
        q(conn(), <<"SELECT status FROM \"user\" WHERE id = $1">>, [NewOwnerUid])
    ),
    ok.

transfer_to_registered() ->
    Conn = conn(),
    Mobile1 = <<"13733330001">>,
    RegMobile = <<"13733330002">>,
    RegUid = new_id(),
    ok = seed_registered_user(Conn, RegUid, RegMobile),
    Resp1 = create_pending_org(#{
        <<"name">> => <<"gz06-transfer-reg">>,
        <<"owner_mobile">> => Mobile1,
        <<"default_workspace_name">> => <<"ws">>
    }),
    ?assertEqual(200, status_of(Resp1)),
    OrgId = org_id_of(payload_of(Resp1)),
    Resp2 = transfer_by_phone(OrgId, RegMobile),
    ?assertEqual(200, status_of(Resp2)),
    P2 = payload_of(Resp2),
    ?assertEqual(<<"direct_transfer">>, maps:get(<<"mode">>, P2)),
    ?assertEqual(null, maps:get(<<"invite">>, P2)),
    ?assertEqual(RegUid, binary_to_integer(maps:get(<<"owner_user_id">>, P2))),
    %% 已注册用户不产生新 user / 新 invite；无 live invite
    ?assertMatch(
        {ok, [#{<<"count">> := 0}]},
        one(
            conn(),
            <<"SELECT count(*) AS count FROM owner_activation_invite",
                " WHERE organization_id = $1 AND status IN ('pending','sms_failed')">>,
            [OrgId]
        )
    ),
    ?assertMatch(
        {ok, [#{<<"owner_rows">> := 1, <<"owner_uid">> := RegUid}]},
        q(
            conn(),
            <<"SELECT count(*) AS owner_rows, min(user_id) AS owner_uid",
                " FROM organization_member WHERE organization_id = $1",
                " AND role = 'owner' AND status = 'active'">>,
            [OrgId]
        )
    ),
    ok.

transfer_to_self() ->
    Mobile = <<"13744440001">>,
    Resp1 = create_pending_org(#{
        <<"name">> => <<"gz06-transfer-self">>,
        <<"owner_mobile">> => Mobile,
        <<"default_workspace_name">> => <<"ws">>
    }),
    ?assertEqual(200, status_of(Resp1)),
    OrgId = org_id_of(payload_of(Resp1)),
    Resp2 = transfer_by_phone(OrgId, Mobile),
    ?assertEqual(400, status_of(Resp2)),
    ok.

transfer_to_deleted() ->
    Conn = conn(),
    Mobile = <<"13755550001">>,
    DelMobile = <<"13755550002">>,
    DelUid = new_id(),
    ok = seed_user_with_status(Conn, DelUid, DelMobile, 2),
    Resp1 = create_pending_org(#{
        <<"name">> => <<"gz06-transfer-del">>,
        <<"owner_mobile">> => Mobile,
        <<"default_workspace_name">> => <<"ws">>
    }),
    ?assertEqual(200, status_of(Resp1)),
    OrgId = org_id_of(payload_of(Resp1)),
    Resp2 = transfer_by_phone(OrgId, DelMobile),
    ?assertEqual(409, status_of(Resp2)),
    ok.

transfer_rollback() ->
    Mobile1 = <<"13766660001">>,
    Mobile2 = <<"13766660002">>,
    Resp1 = create_pending_org(#{
        <<"name">> => <<"gz06-transfer-rb">>,
        <<"owner_mobile">> => Mobile1,
        <<"default_workspace_name">> => <<"ws">>
    }),
    ?assertEqual(200, status_of(Resp1)),
    OrgId = org_id_of(payload_of(Resp1)),
    OldOwnerUid = binary_to_integer(
        maps:get(<<"owner_id">>, maps:get(<<"organization">>, payload_of(Resp1)))
    ),
    %% 注入：投影更新失败 → 整事务回滚（D13）
    case
        meck_helper:setup_mock(organization_owner_store, [
            {'update_owner_projection_tx', 3, fun(_C, _O, _U) -> {error, injected_failure} end}
        ])
    of
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({mock_setup_failed, inject, Reason})
    end,
    try
        Resp2 = transfer_by_phone(OrgId, Mobile2),
        ?assertEqual(500, status_of(Resp2))
    after
        meck_helper:cleanup_mock(organization_owner_store)
    end,
    %% 零残留：owner 不变、live invite 仍 pending 指向旧 owner、预创建 user 回滚
    ?assertMatch(
        {ok, [#{<<"owner_id">> := OldOwnerUid}]},
        q(conn(), <<"SELECT owner_id FROM organization WHERE id = $1">>, [OrgId])
    ),
    ?assertMatch(
        {ok, [#{<<"owner_user_id">> := OldOwnerUid, <<"status">> := <<"pending">>}]},
        q(
            conn(),
            <<"SELECT owner_user_id, status FROM owner_activation_invite",
                " WHERE organization_id = $1 AND status IN ('pending','sms_failed')">>,
            [OrgId]
        )
    ),
    ?assertMatch(
        {ok, [#{<<"count">> := 0}]},
        one(conn(), <<"SELECT count(*) AS count FROM \"user\" WHERE mobile = $1">>, [Mobile2])
    ),
    ok.

%% ===================================================================
%% 组 9：状态卡 404 + ACL fail-closed
%% ===================================================================

status_and_acl_tests() ->
    [
        {"状态卡：组织不存在 → 404", fun() ->
            Missing = new_id(),
            Resp = call_owner_action(?WRITE_UID, <<"GET">>, Missing, owner_activation_show, #{}),
            ?assertEqual(404, status_of(Resp))
        end},
        {"read-only 角色 resend → 403（fail-closed）", fun() ->
            Mobile = <<"13777770001">>,
            Resp1 = create_pending_org(#{
                <<"name">> => <<"gz06-acl">>,
                <<"owner_mobile">> => Mobile,
                <<"default_workspace_name">> => <<"ws">>
            }),
            ?assertEqual(200, status_of(Resp1)),
            OrgId = org_id_of(payload_of(Resp1)),
            Resp2 = call_owner_action(?RO_UID, <<"POST">>, OrgId, owner_activation_resend, #{}),
            ?assertEqual(403, status_of(Resp2)),
            %% resend_count 未增加（被 403 拦截）
            ?assertMatch(
                {ok, [#{<<"resend_count">> := 0}]},
                q(
                    conn(),
                    <<"SELECT resend_count FROM owner_activation_invite WHERE organization_id = $1">>,
                    [OrgId]
                )
            ),
            ok
        end}
    ].

%% ===================================================================
%% 调用 helper
%% ===================================================================

%% 创建（pending_phone）走 adm_organization_handler 的 collection POST
create_pending_org(BodyMap) ->
    Full = maps:merge(
        #{<<"owner_mode">> => <<"pending_phone">>}, BodyMap
    ),
    Req0 = #{method => <<"POST">>, bindings => #{}, body => jsone:encode(Full)},
    {ok, RespReq, _} =
        adm_organization_handler:init(Req0, #{action => list, adm_user_id => ?WRITE_UID}),
    RespReq.

%% owner-activation 治理端点（adm_owner_activation_handler）
call_owner_action(AdmUid, Method, OrgId, Action, BodyMap) ->
    Req0 = #{
        method => Method,
        bindings => #{organization_id => integer_to_binary(OrgId)},
        body => jsone:encode(BodyMap)
    },
    {ok, RespReq, _} = adm_owner_activation_handler:init(Req0, #{
        action => Action, adm_user_id => AdmUid
    }),
    RespReq.

transfer_by_phone(OrgId, Mobile) ->
    call_owner_action(?WRITE_UID, <<"POST">>, OrgId, owner_transfer_by_phone, #{
        <<"owner_mobile">> => Mobile
    }).

status_of(RespReq) -> maps:get(response_status, RespReq, undefined).

payload_of(RespReq) -> maps:get(payload, RespReq, undefined).

org_id_of(Payload) ->
    binary_to_integer(maps:get(<<"id">>, maps:get(<<"organization">>, Payload))).

%% ===================================================================
%% Seed / SQL helpers（合成夹具；随机 TSID 隔离，不 TRUNCATE 不删行）
%% ===================================================================

seed_registered_user(Conn, Uid, Mobile) ->
    seed_user_with_status(Conn, Uid, Mobile, 1).

seed_user_with_status(Conn, Uid, Mobile, Status) ->
    exec(
        Conn,
        <<"INSERT INTO \"user\"(id,password,account,mobile,reg_ip,reg_cosv,status,account_type)",
            " VALUES ($1,'x',$2,$3,'127.0.0.1','x',$4,0)">>,
        [Uid, account(Uid), Mobile, Status]
    ).

account(Uid) ->
    <<"gz06t_", (integer_to_binary(Uid))/binary>>.

new_id() ->
    try elib_tsid:generate() of
        Id when is_integer(Id) -> Id
    catch
        _:_ ->
            1000000000000000 + erlang:unique_integer([positive, monotonic])
    end.

conn() ->
    pooler:take_member(pgsql).

exec(Conn, Sql, Params) ->
    case epgsql:equery(Conn, Sql, Params) of
        {ok, _} ->
            pooler:return_member(pgsql, Conn),
            ok;
        Other ->
            pooler:return_member(pgsql, Conn, fail),
            erlang:error({seed_failed, Sql, Other})
    end.

one(Conn, Sql, Params) ->
    case epgsql:equery(Conn, Sql, Params) of
        {ok, Cols, [Row]} ->
            pooler:return_member(pgsql, Conn),
            {ok, [tuple_to_named_map(Cols, Row)]};
        Other ->
            pooler:return_member(pgsql, Conn, fail),
            erlang:error({query_failed, Sql, Other})
    end.

q(Conn, Sql, Params) ->
    R = epgsql:equery(Conn, Sql, Params),
    pooler:return_member(pgsql, Conn),
    case R of
        {ok, Cols, Rows} when is_list(Rows) ->
            {ok, [tuple_to_named_map(Cols, Row) || Row <- Rows]};
        Other ->
            {error, Other}
    end.

tuple_to_named_map(Cols, Row) when is_list(Cols) ->
    Names = [element(2, C) || C <- Cols],
    maps:from_list(lists:zip(Names, tuple_to_list(Row))).


%% A1c：create_pending_owner_gated 链路依赖命名 depcache 实例 imboy_cache
%% （ETS 'm:imboy_cache'）。本套件不启动 imboy app，单跑时须自足补建；全量跑
%% 时实例已由 eunit_setup 启动的 app 持有，此处为 no-op（与
%% adm_organization_create_tests:ensure_depcache/0 同配方）。
ensure_depcache() ->
    case ets:whereis('m:imboy_cache') of
        undefined ->
            case whereis(eunit_boot_coordinator) of
                undefined ->
                    _ = imboy_cache:start_link([{depcache_memory_max, 100}]),
                    ok;
                _CoordPid ->
                    Ref = make_ref(),
                    eunit_boot_coordinator ! {ensure_cache, self(), Ref},
                    receive
                        {Ref, ok} -> ok
                    after 5000 ->
                        _ = imboy_cache:start_link([{depcache_memory_max, 100}]),
                        ok
                    end
            end;
        _ ->
            ok
    end.
