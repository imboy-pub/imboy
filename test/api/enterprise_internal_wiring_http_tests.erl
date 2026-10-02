-module(enterprise_internal_wiring_http_tests).

%%%
% EPGZ-08 W4 —— 企业 internal 面**路由接线**黑盒测试（真 cowboy listener +
% 真中间件链，不 mock 认证结论）。
%
% 覆盖（plan §6 白名单 / §10 门禁 / A0 control/internal-api-manifest.yaml；
% V2.1 §6.1 扩到 31 条）：
%   ① 冻结路由表 31 条（INT-01..31）；Router（共享路径）已接线 23 条 →
%      19 个 cowboy path（同 path 多方法：INT-02/15、INT-05/06、INT-18/19/21）；
%      INT-24..31 的 handler 由 A2 实现后经 A0 接线，此处对这 8 条断言注册
%      形态（method/path/scope/idempotency=none/rate_bucket）与 match/2 可达；
%      已接线 handler 模块可加载（无 undef 悬挂）；
%   ② **零 /api/open/v1 生产面**：路由表内 0 条 + 真实请求 404；
%   ③ internal 前缀**不在 open()/option()**（匿名不可达）；
%   ④ 未认证请求落 enterprise_internal_middleware 认证链 → 401 invalid_credential
%      信封（**不是** verify_sign 的 902，证明前缀分支顺序正确）；
%   ⑤ 表外 method+path fail-closed → 404 resource_not_found；
%   ⑥ 路径绑定（:group_id / :delivery_id）经中间件收敛为整数进入 handler_opts。
%
% 本套件**不起 imboy app、不连 PG**：④⑤ 只走到认证链的失败分支（无需池化
% 连接），因此可在 eunit 内以真 transport 跑；带业务 oracle 的真库 E2E 归
% EPGZ-09（真节点 + run-owned scratch PG）。
%%%

-include_lib("eunit/include/eunit.hrl").

-define(LISTENER, enterprise_internal_wiring_http_test_listener).

%% 与 imboy_app.erl 的同序中间件链（auth_middleware 必须在 cowboy_router 之后）
-define(MIDDLEWARES, [
    cowboy_router,
    cors_middleware,
    security_headers_middleware,
    auth_middleware,
    feature_gate_middleware,
    throttle_middleware,
    cowboy_handler
]).

%%%===================================================================
%%% 路由表静态面（不启 listener）
%%%===================================================================

route_table_test_() ->
    {timeout, 30, fun route_table/0}.

route_table() ->
    [{_Host, Routes}] = imboy_router:get_routes(),
    Paths = [P || {P, _H, _O} <- Routes],

    %% ①② 冻结表逐条登记 + 零 open 面
    %% 32 条 INT（GZ 14 + FULL-02 新增 8 + FULL-03 新增 1 + V2.1 新增 8 + v1.1.1 新增 1）。
    %% Router（共享路径，A0 接线）当前登记的是已实现 handler 的 23 条——
    %% INT-24..31 的 handler（enterprise_workspace/project/channel_handler、
    %% enterprise_group_handler 扩展）由 A2 实现后经 A0 接线进 imboy_router，
    %% 故此处只断言「已接线面」的登记与形态；注册表全量形态另行断言（下方
    %% manifest_v21_entries）。
    Manifest = enterprise_internal_routes:routes(),
    ?assertEqual(42, length(Manifest)),
    %% Router（共享路径，A0 已于集成接线）：INT-24..31 的 8 条全部进
    %% imboy_router；其中 INT-26/27 复用既有 cowboy path（GET 方法分派在
    %% handler 内），故 31 端点对应 25 条唯一 path（19 既有 + 6 新增）。
    Wired = Manifest,
    ?assertEqual(42, length(Wired)),
    %% 冻结表用 {name} 占位符语法，cowboy 路由用 :name —— 归一后逐条比对。
    WiredPaths = lists:usort([
        cowboy_path(binary_to_list(maps:get(path, R)))
     || R <- Wired
    ]),
    ?assertEqual(28, length(WiredPaths)),
    lists:foreach(
        fun(P) ->
            ?assert(lists:member(P, Paths))
        end,
        WiredPaths
    ),
    ?assertEqual(28, length([P || P <- Paths, lists:prefix("/api/internal/v1/", P)])),

    %% ③ internal 前缀不在匿名白名单；零 open 面
    Open = imboy_router:open(),
    Option = imboy_router:option(),
    ?assertEqual(0, length([P || P <- Open, lists:prefix("/api/internal/v1/", binary_to_list(P))])),
    ?assertEqual(
        0, length([P || P <- Option, lists:prefix("/api/internal/v1/", binary_to_list(P))])
    ),
    ?assertEqual(0, length([P || P <- Paths, lists:prefix("/api/open/v1/", P)])),

    %% 人工签发端点必须在人类 JWT 面（/api/v1/oa/sso/code），且不在 open()
    ?assert(lists:member("/api/v1/oa/sso/code", Paths)),
    ?assertNot(lists:member(<<"/api/v1/oa/sso/code">>, Open)),

    %% 每个 internal 路由的 handler 必须可加载（防 undef 悬挂）
    lists:foreach(
        fun({_P, Handler, _O}) -> ?assertNotEqual(false, code:ensure_loaded(Handler)) end,
        [R || R = {P, _H, _O} <- Routes, lists:prefix("/api/internal/v1/", P)]
    ).

%% V2.1 §6.1/§6.2 冻结的 INT-24..31 注册形态 + INT-18 scope 修正
%% （CON-01 的 A1 侧前置断言：31 unique method+path、scope 全在 14 值枚举、
%%  新增 8 条全为 GET + idempotency=none + internal_read）。
manifest_v21_entries_test_() ->
    {timeout, 15, fun manifest_v21_entries/0}.

manifest_v21_entries() ->
    Manifest = enterprise_internal_routes:routes(),
    %% 31 unique id + 31 unique method+path
    Ids = [maps:get(id, R) || R <- Manifest],
    ?assertEqual(42, length(lists:usort(Ids))),
    MethodPaths = [{maps:get(method, R), maps:get(path, R)} || R <- Manifest],
    ?assertEqual(42, length(lists:usort(MethodPaths))),
    %% 所有注册 scope 都是固定 14 值枚举成员（动态 scope 除外）
    All = enterprise_internal_scope:all(),
    lists:foreach(
        fun(R) ->
            case maps:get(scope, R) of
                {dynamic, _} -> ok;
                S -> ?assert(lists:member(S, All), {scope_not_in_catalog, maps:get(id, R), S})
            end
        end,
        Manifest
    ),
    %% INT-18 scope 修正：读操作降为 groups:read（§6.2/§7）
    ById = #{maps:get(id, R) => R || R <- Manifest},
    ?assertEqual(
        <<"groups:read">>, maps:get(scope, maps:get(<<"INT-18">>, ById))
    ),
    %% INT-24..31 冻结形态：method/path/scope/idempotency
    ExpectedV21 = [
        {<<"INT-24">>, <<"GET">>, <<"/api/internal/v1/workspaces">>, <<"workspaces:read">>},
        {<<"INT-25">>, <<"GET">>, <<"/api/internal/v1/workspaces/{workspace_id}">>,
            <<"workspaces:read">>},
        {<<"INT-26">>, <<"GET">>, <<"/api/internal/v1/groups">>, <<"groups:read">>},
        {<<"INT-27">>, <<"GET">>, <<"/api/internal/v1/groups/{group_id}/members">>,
            <<"groups:read">>},
        {<<"INT-28">>, <<"GET">>, <<"/api/internal/v1/projects">>, <<"projects:read">>},
        {<<"INT-29">>, <<"GET">>, <<"/api/internal/v1/projects/{project_id}">>,
            <<"projects:read">>},
        {<<"INT-30">>, <<"GET">>, <<"/api/internal/v1/channels">>, <<"channels:read">>},
        {<<"INT-31">>, <<"GET">>, <<"/api/internal/v1/channels/{channel_id}">>, <<"channels:read">>}
    ],
    lists:foreach(
        fun({Id, Method, Path, Scope}) ->
            R = maps:get(Id, ById, #{}),
            ?assertEqual(Method, maps:get(method, R, undefined), {Id, method}),
            ?assertEqual(Path, maps:get(path, R, undefined), {Id, path}),
            ?assertEqual(Scope, maps:get(scope, R, undefined), {Id, scope}),
            ?assertEqual(not_required, maps:get(idempotency, R, undefined), {Id, idempotency}),
            ?assertEqual(internal_read, maps:get(rate_bucket, R, undefined), {Id, rate_bucket}),
            %% match/2 对未接线 Router 的路径也可做认证链级匹配（负例可测）
            ?assertMatch({ok, R}, enterprise_internal_routes:match(Method, Path))
        end,
        ExpectedV21
    ),
    %% INT-24..31 的边界规格 kind 语义（§6.2）：列表=covered W（list）或必填
    %% W 过滤（workspace）；详情=workspace
    ?assertMatch(
        {ok, #{kind := list, scope := <<"workspaces:read">>}},
        enterprise_internal_boundary:spec(<<"INT-24">>)
    ),
    ?assertMatch(
        {ok, #{kind := list, scope := <<"groups:read">>}},
        enterprise_internal_boundary:spec(<<"INT-26">>)
    ),
    lists:foreach(
        fun(Id) ->
            ?assertMatch(
                {ok, #{kind := workspace, scope := _}},
                enterprise_internal_boundary:spec(Id)
            )
        end,
        [<<"INT-25">>, <<"INT-27">>, <<"INT-28">>, <<"INT-29">>, <<"INT-30">>, <<"INT-31">>]
    ),
    %% INT-18 边界规格同步降为 groups:read（路由表 ↔ 边界表一致）
    ?assertMatch(
        {ok, #{kind := workspace, scope := <<"groups:read">>}},
        enterprise_internal_boundary:spec(<<"INT-18">>)
    ).

%%%===================================================================
%%% 真 transport 面（真 listener + 真中间件链）
%%%===================================================================

http_wiring_test_() ->
    {timeout, 60, fun http_wiring/0}.

http_wiring() ->
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(ranch),
    Dispatch = cowboy_router:compile(imboy_router:get_routes()),
    {ok, _Pid} = cowboy:start_clear(
        ?LISTENER,
        [{port, 0}],
        #{env => #{dispatch => Dispatch}, middlewares => ?MIDDLEWARES}
    ),
    try
        Port = ranch:get_port(?LISTENER),
        assert_unauthenticated_chain(Port),
        assert_fail_closed(Port)
    after
        ok = cowboy:stop_listener(?LISTENER)
    end.

%% ④ 未认证：必须落 enterprise_internal_middleware 的 credential 链，
%% 回 invalid_credential 信封 + 401；**不得**是 902（签名门）也不是 200。
assert_unauthenticated_chain(Port) ->
    NoAuth = request(Port, <<"GET">>, <<"/api/internal/v1/application">>, []),
    ?assertNotMatch(<<"HTTP/1.1 902", _/binary>>, NoAuth),
    assert_error_envelope(NoAuth, <<"invalid_credential">>),

    %% 形态畸形（非 ib_int_ 前缀 / 无 .secret / 空 secret）→ 同一 stable 码。
    %% 这些都在 parse_bearer 阶段拒绝，**不触碰 PG 池**，故可在无 app 的 eunit
    %% VM 内断言。形状合法但 locator 不存在的凭证必须过 DB，归 EPGZ-09 真节点
    %% E2E（本套件刻意不起 pool，不得用 mock 伪造该分支）。
    lists:foreach(
        fun(Bad) ->
            Resp = request(
                Port,
                <<"GET">>,
                <<"/api/internal/v1/application">>,
                [{<<"authorization">>, <<"Bearer ", Bad/binary>>}]
            ),
            assert_error_envelope(Resp, <<"invalid_credential">>)
        end,
        [<<"nope">>, <<"ib_int_abc">>, <<"ib_int_abc.">>, <<"ib_int_.x">>, <<"ib_int_1.">>]
    ),
    %% 裸 Authorization（无 Bearer 前缀）同样在解析阶段拒绝
    Bare = request(
        Port,
        <<"GET">>,
        <<"/api/internal/v1/application">>,
        [{<<"authorization">>, <<"ib_int_1.deadbeef">>}]
    ),
    assert_error_envelope(Bare, <<"invalid_credential">>).

%% ⑤⑥ 表外组合 fail-closed + 零 open 面真实请求 404
assert_fail_closed(Port) ->
    %% 表外 path：cowboy_router 不命中即停，回**裸 404**（空体，无任何 oracle），
    %% 认证中间件不会被调用。这是期望行为：表外路径既不落人类 API，也不泄露
    %% 内部面的存在性/形状。
    NotFound = request(Port, <<"GET">>, <<"/api/internal/v1/definitely-not-a-route">>, []),
    ?assertMatch(<<"HTTP/1.1 404", _/binary>>, NotFound),
    ?assertEqual(nomatch, binary:match(NotFound, <<"\"code\"">>)),

    %% 表内 path + 表外 method：路由命中（path 匹配）→ 认证中间件运行 →
    %% 冻结表按 method+path 判定 → 同一 stable 码 + A2 信封。
    %% 这条同时是 normalize_code/1 的回归钉子：decide 返回 atom
    %% resource_not_found，不归一就会变成 500 + internal_error（W4 实测红）。
    WrongMethod = request(Port, <<"PUT">>, <<"/api/internal/v1/application">>, []),
    assert_error_envelope(WrongMethod, <<"resource_not_found">>),

    %% 人类面路径不得被 internal 面触达（INV-2）。路径穿越经 cowboy 归一后
    %% 既不落 internal 处理链也不返回 200。
    HumanPath = request(Port, <<"GET">>, <<"/api/internal/v1/../../../api/v1/user/show">>, []),
    ?assertNotMatch(<<"HTTP/1.1 200", _/binary>>, HumanPath),
    ?assertNotMatch(<<"HTTP/1.1 902", _/binary>>, HumanPath),

    %% ② 零 open 面：/api/open/v1/* 无路由（cowboy_router 直接 404）
    OpenResp = request(Port, <<"GET">>, <<"/api/open/v1/anything">>, []),
    ?assertMatch(<<"HTTP/1.1 404", _/binary>>, OpenResp).

%% 冻结表 ↔ 边界规格表双向一致（FULL-02 引入；任一侧漏登记 = 边界失效）
boundary_parity_test_() ->
    {timeout, 15, fun boundary_parity/0}.

boundary_parity() ->
    TableIds = lists:sort([maps:get(id, R) || R <- enterprise_internal_routes:routes()]),
    BoundaryIds = lists:sort(enterprise_internal_boundary:ids()),
    ?assertEqual(TableIds, BoundaryIds),
    %% 每条路由都必须有边界规格（spec 缺失 → error，不可静默放行）
    lists:foreach(
        fun(Id) ->
            ?assertMatch(
                {ok, #{kind := _, scope := _}},
                enterprise_internal_boundary:spec(Id)
            )
        end,
        TableIds
    ).

%% cowboy dispatch 可编译 = path 无重复（cowboy 禁止同 path 重复登记；
%% INT-02/15、INT-05/06、INT-18/19/21 都是同 path 多方法，必须在 handler 内分派）
dispatch_compiles_test_() ->
    {timeout, 30, fun dispatch_compiles/0}.

dispatch_compiles() ->
    [{_Host, Routes}] = imboy_router:get_routes(),
    Paths = [P || {P, _H, _O} <- Routes],
    Dups = Paths -- lists:usort(Paths),
    ?assertEqual([], Dups),
    ?assertNotEqual([], cowboy_router:compile(imboy_router:get_routes())).

%%%===================================================================
%%% 码归一（内部 atom → manifest 二进制）
%%%===================================================================

normalize_code_test_() ->
    {timeout, 15, fun normalize_code/0}.

%% enterprise_internal_auth 内部按 atom 返回错误码，信封只认 manifest 二进制码；
%% normalize_code/1 是两者之间**唯一**的归一。这条测试同时钉住三件事：
%%   ① 13 个 stable 码全覆盖且一一对应；
%%   ② 归一结果幂等（binary 入参原样返回）；
%%   ③ 归一后的码在信封里认得（http_status 不落 500 兜底）。
normalize_code() ->
    Pairs = [
        {invalid_credential, <<"invalid_credential">>},
        {credential_expired, <<"credential_expired">>},
        {application_disabled, <<"application_disabled">>},
        {organization_disabled, <<"organization_disabled">>},
        {insufficient_scope, <<"insufficient_scope">>},
        {resource_not_found, <<"resource_not_found">>},
        {identity_not_mapped, <<"identity_not_mapped">>},
        {organization_boundary_violation, <<"organization_boundary_violation">>},
        {idempotency_conflict, <<"idempotency_conflict">>},
        {rate_limited, <<"rate_limited">>},
        {security_gate_closed, <<"security_gate_closed">>},
        {invalid_request, <<"invalid_request">>},
        {internal_error, <<"internal_error">>},
        {version_conflict, <<"version_conflict">>},
        {resource_conflict, <<"resource_conflict">>},
        {seat_limit_exceeded, <<"seat_limit_exceeded">>}
    ],
    ?assertEqual(
        lists:sort(enterprise_internal_error:codes()),
        lists:sort([Bin || {_Atom, Bin} <- Pairs])
    ),
    lists:foreach(
        fun({Atom, Bin}) ->
            ?assertEqual(Bin, enterprise_internal_middleware:normalize_code(Atom)),
            ?assertEqual(Bin, enterprise_internal_middleware:normalize_code(Bin)),
            %% 归一后的码必须是信封认得的码：信封体里回的就是它本身（未落
            %% "未知码 → fail-safe 成 internal_error" 兜底）。
            ?assertNotEqual(
                nomatch,
                binary:match(
                    enterprise_internal_error:error_body(
                        enterprise_internal_middleware:normalize_code(Atom)
                    ),
                    Bin
                )
            )
        end,
        Pairs
    ).

%%%===================================================================
%%% 断言助手
%%%===================================================================

%%%===================================================================
%%% INT-BE-02 —— 31-operation 真实 HTTP conformance
%%%（disposable PG + 真 Cowboy + synthetic Application credential/Grant；
%%%  harness 细节见 test/api/intbe02_http_support.erl 模块头）
%%%
%%% INT-API-02A 验收口径：
%%%   ① covered_operation_ids 精确等于冻结表 31 个 runtime operation ID
%%%      （本节末 assert_coverage 用 ETS 登记集合 ↔ routes()/0 全量比对）；
%%%   ② 每个 method+path 至少一个预期成功或业务可达响应（本节正例链全部
%%%      走 200/业务语义；禁止裸 404 计入覆盖）；
%%%   ③ mutation（idempotency=required 的 16 条）逐一验证 Idempotency-Key
%%%      重放（同 key 同 body → 同 status + 逐字节同 body；同 key 异 body →
%%%      409）；INT-14 为合同声明的 single_use_code 豁免项，改为验证 code
%%%      一次性（重放已消费 code → 404 resource_not_found）；
%%%   ④ negative 三件套：无 credential=401（31 条逐一）、缺 scope=403
%%%      （RO 应用全 mutation 面 + 零 Grant 应用）、跨 Org / 未覆盖
%%%      Workspace 同体拒绝（404 同体 / 403 organization_boundary_violation）；
%%%   ⑤ envelope 对齐：错误响应状态码与信封 code 按
%%%      enterprise_internal_error 冻结映射（复用本文件既有
%%%      assert_error_envelope/2）；schema 对齐：成功响应按 openapi
%%%      必填键做封闭投影抽查。
%%% ===================================================================

conformance_test_() ->
    {timeout, 900,
        {setup, fun intbe02_http_support:setup_all/0, fun intbe02_http_support:teardown_all/1, fun(
            S
        ) ->
            {timeout, 900, fun() -> run_conformance(S) end}
        end}}.

run_conformance(S) ->
    #{conn := C} = S,
    seed_delivery_row(C, S),
    workspace_channel_limit_http_checks:run(S),
    G = positive_chain(S),
    enterprise_oa_expiry_http_checks:run(S),
    idempotency_matrix(S, G),
    negative_matrix(S),
    enterprise_internal_authorization_http_checks:run(S),
    seat_concurrent_update(S),
    seat_negative_matrix(S),
    enterprise_identity_contract_http_checks:run(S),
    enterprise_channel_write_http_checks:run(S),
    enterprise_workspace_write_http_checks:run(S),
    assert_coverage(),
    ok.

%% INT-13 正例需要一条本 App 归属、终态（success）的 bot_delivery 行。
%% 企业行生而 pending 是 DB 守卫（trg_ewh_delivery_guard）强制，故沿用
%% enterprise_webhook_governance_pg_tests 的「插 pending → 迁 success」两步。
%% webhook host 必须是 with_public_dns 替身认领的 oa.customer.example.com
%% （replay_finalize 会重新 validate_and_pin 当前端点）。
seed_delivery_row(C, S) ->
    AppA = integer_to_binary(maps:get(app_a, S)),
    %% 账本守卫三元一致（migration 00000141）：bot_delivery 归属校验要求数据库
    %% 存在 bot 行（bot.user_id = application.principal_user_id）——先补 PRIN_A
    %% 的 bot 行，再插 pending 投递行、迁移到终态 success。
    ok = intbe02_http_support:sql_exec(C, [
        <<"INSERT INTO bot (user_id, name, username, owner_uid, webhook_url, events,">>,
        <<" is_public, status, created_at, updated_at) VALUES (995014, 'intbe02 bot',">>,
        <<" 'intbe02-bot-995014', 995001, 'https://oa.customer.example.com/intbe02/hook',">>,
        <<" '[]'::jsonb, false, 1, NOW(), NOW())">>
    ]),
    ok = intbe02_http_support:sql_exec(C, [
        <<"INSERT INTO bot_delivery (delivery_id, bot_id, event_type, payload,">>,
        <<" correlation_id, idempotency_key, webhook_url, webhook_host, pinned_ip,">>,
        <<" status, ewh_owner_organization_id, ewh_owner_application_id,">>,
        <<" created_at, updated_at) VALUES ('intbe02-dlv-0001', 'eapp:995014',">>,
        <<" 'file.confirmed', '{}', 'intbe02-corr-0001', 'intbe02-dlv-idem-0001',">>,
        <<" 'https://oa.customer.example.com/intbe02/hook',">>,
        <<" 'oa.customer.example.com', '93.184.216.34', 'pending', 995101, ">>,
        AppA,
        <<", NOW() - INTERVAL '2 days', NOW() - INTERVAL '2 days')">>
    ]),
    ok = intbe02_http_support:sql_exec(
        C, <<"UPDATE bot_delivery SET status = 'success' WHERE delivery_id = 'intbe02-dlv-0001'">>
    ),
    ok.

%% ------------------------------------------------------------------
%% ① 正例链（依赖顺序）：31 条各一个预期成功/业务可达响应
%% ------------------------------------------------------------------

positive_chain(S) ->
    Port = maps:get(port, S),
    C = maps:get(conn, S),
    A = intbe02_http_support:auth(maps:get(cred_a, S)),
    SSO = intbe02_http_support:auth(maps:get(cred_sso, S)),
    H1 = intbe02_http_support:fixture(ext_h1, x),
    H2 = intbe02_http_support:fixture(ext_h2, x),
    H3 = intbe02_http_support:fixture(ext_h3, x),
    WsA1 = intbe02_http_support:fixture(ws_a1, x),
    WsA2 = intbe02_http_support:fixture(ws_a2, x),
    PrjA1 = intbe02_http_support:fixture(prj_a1, x),
    ChnA1 = intbe02_http_support:fixture(chn_a1, x),
    GrpHuman = intbe02_http_support:fixture(grp_human, x),
    Redirect = intbe02_http_support:fixture(redirect, x),
    Nonce = intbe02_http_support:fixture(nonce, x),

    %% ---- INT-01 GET /application（成功；RO 凭证的同端点 200 见 negative_matrix
    %%      的最小 scope 正例，不重复登记覆盖）----
    R01 = intbe02_http_support:http(Port, <<"GET">>, <<"/api/internal/v1/application">>, <<>>, A),
    assert_ok_json(R01),
    cover(<<"INT-01">>),

    %% ---- INT-02 PUT identity-mappings（bind ALICE，重放规格入列）----
    Body02 = #{<<"external_user_id">> => <<"intbe02-ext-h4">>, <<"user_id">> => 995017},
    R02 =
        intbe02_http_support:http(
            Port,
            <<"PUT">>,
            <<"/api/internal/v1/identity-mappings">>,
            Body02,
            maps:merge(A, intbe02_http_support:idem(<<"intbe02-idem-02">>))
        ),
    assert_ok_json(R02),
    cover(<<"INT-02">>),

    %% ---- INT-03 POST resolve：present-only 语义（未知 ext 不出现=跨 Org 不可见）----
    R03 =
        intbe02_http_support:http(
            Port,
            <<"POST">>,
            <<"/api/internal/v1/identity-mappings/resolve">>,
            #{<<"external_user_ids">> => [H1, <<"intbe02-ext-b1-unknown">>]},
            A
        ),
    #{<<"mappings">> := Maps03} = assert_ok_json(R03),
    ?assertEqual([H1], [maps:get(<<"external_user_id">>, M) || M <- Maps03]),
    cover(<<"INT-03">>),

    %% ---- INT-04 POST /groups（成员=已映射且为 WS 成员的 H1/H2）----
    Body04 = #{
        <<"workspace_id">> => WsA1,
        <<"title">> => <<"intbe02 conformance group"/utf8>>,
        <<"members">> => [H1, H2]
    },
    R04 =
        intbe02_http_support:http(
            Port,
            <<"POST">>,
            <<"/api/internal/v1/groups">>,
            Body04,
            maps:merge(A, intbe02_http_support:idem(<<"intbe02-idem-04">>))
        ),
    #{<<"group_id">> := Gid} = assert_ok_json(R04),
    ?assert(is_integer(Gid)),
    cover(<<"INT-04">>),
    GrpPath = iolist_to_binary(["/api/internal/v1/groups/", integer_to_binary(Gid)]),
    GrpMembersPath = iolist_to_binary([GrpPath, "/members"]),

    %% ---- INT-05 PUT members（补 H3 入群）----
    Body05 = #{<<"external_user_ids">> => [H3]},
    R05 =
        intbe02_http_support:http(
            Port,
            <<"PUT">>,
            GrpMembersPath,
            Body05,
            maps:merge(A, intbe02_http_support:idem(<<"intbe02-idem-05">>))
        ),
    assert_ok_json(R05),
    cover(<<"INT-05">>),

    %% ---- INT-20 PUT members/roles（H3 → 嘉宾 role=2；4=群主不对 OA 开放）----
    Body20 = #{<<"roles">> => [#{<<"external_user_id">> => H3, <<"role">> => 2}]},
    R20 =
        intbe02_http_support:http(
            Port,
            <<"PUT">>,
            <<GrpMembersPath/binary, "/roles">>,
            Body20,
            maps:merge(A, intbe02_http_support:idem(<<"intbe02-idem-20">>))
        ),
    assert_ok_json(R20),
    cover(<<"INT-20">>),

    %% ---- INT-18 GET 群详情 ----
    R18 = intbe02_http_support:http(Port, <<"GET">>, GrpPath, <<>>, A),
    #{<<"members">> := _} = assert_ok_json(R18),
    cover(<<"INT-18">>),

    %% ---- INT-19 PATCH 群（改名）----
    Body19 = #{<<"title">> => <<"intbe02 renamed group"/utf8>>},
    R19 =
        intbe02_http_support:http(
            Port,
            <<"PATCH">>,
            GrpPath,
            Body19,
            maps:merge(A, intbe02_http_support:idem(<<"intbe02-idem-19">>))
        ),
    assert_ok_json(R19),
    cover(<<"INT-19">>),

    %% ---- INT-27 GET members（active 成员列表非空）----
    R27 = intbe02_http_support:http(Port, <<"GET">>, GrpMembersPath, <<>>, A),
    #{<<"items">> := Items27} = assert_page(R27),
    ?assertNotEqual([], Items27),
    lists:foreach(
        fun(M) -> ?assert(maps:is_key(<<"user_id">>, M)) end, Items27
    ),
    cover(<<"INT-27">>),

    %% ---- INT-06 DELETE members（移除 H3）----
    Body06 = #{<<"external_user_ids">> => [H3]},
    R06 =
        intbe02_http_support:http(
            Port,
            <<"DELETE">>,
            GrpMembersPath,
            Body06,
            maps:merge(A, intbe02_http_support:idem(<<"intbe02-idem-06">>))
        ),
    assert_ok_json(R06),
    cover(<<"INT-06">>),

    %% ---- INT-07 POST files/presign ----
    Body07 = #{<<"file_name">> => <<"intbe02-report.txt">>, <<"mime_type">> => <<"text/plain">>},
    R07 =
        intbe02_http_support:http(
            Port,
            <<"POST">>,
            <<"/api/internal/v1/files/presign">>,
            Body07,
            maps:merge(A, intbe02_http_support:idem(<<"intbe02-idem-07">>))
        ),
    #{<<"object_key">> := ObjectKey} = assert_ok_json(R07),
    ?assert(is_binary(ObjectKey), ObjectKey),
    ?assertNotEqual(nomatch, binary:match(ObjectKey, <<"eoa/">>)),
    cover(<<"INT-07">>),

    %% ---- INT-08 POST files/confirm（HEAD 替身；server 真值覆盖自报值）----
    Body08 = #{<<"object_key">> => ObjectKey, <<"file_hash256">> => hash256(<<"x">>)},
    R08 =
        intbe02_http_support:with_oss_head(1024, fun() ->
            intbe02_http_support:http(
                Port,
                <<"POST">>,
                <<"/api/internal/v1/files/confirm">>,
                Body08,
                maps:merge(A, intbe02_http_support:idem(<<"intbe02-idem-08">>))
            )
        end),
    #{<<"file_id">> := _} = assert_ok_json(R08),
    cover(<<"INT-08">>),

    %% ---- INT-22 POST files/governance（延长留存至上限=合法延长）----
    Body22 = #{
        <<"op">> => <<"set_retention">>,
        <<"object_key">> => ObjectKey,
        <<"retention_days">> => 3650
    },
    R22 =
        intbe02_http_support:http(
            Port,
            <<"POST">>,
            <<"/api/internal/v1/files/governance">>,
            Body22,
            maps:merge(A, intbe02_http_support:idem(<<"intbe02-idem-22">>))
        ),
    assert_ok_json(R22),
    cover(<<"INT-22">>),

    %% ---- INT-09 POST messages/direct（human 代发；DNS 替身避免事件入箱
    %%      时的真实解析延迟——webhook 入箱失败不阻断业务事务）----
    Body09 = #{
        <<"sender_mode">> => <<"human">>,
        <<"sender_user_id">> => H1,
        <<"recipient_user_id">> => H2,
        <<"msg_type">> => <<"text">>,
        <<"content">> => <<"intbe02 direct hello"/utf8>>
    },
    R09 = with_dns(fun() ->
        intbe02_http_support:http(
            Port,
            <<"POST">>,
            <<"/api/internal/v1/messages/direct">>,
            Body09,
            maps:merge(A, intbe02_http_support:idem(<<"intbe02-idem-09">>))
        )
    end),
    #{<<"msg_id">> := _} = assert_ok_json(R09),
    cover(<<"INT-09">>),

    %% ---- INT-10 POST /groups/{gid}/messages（sender 必须是群成员）----
    Body10 = #{
        <<"sender_mode">> => <<"human">>,
        <<"sender_user_id">> => H1,
        <<"msg_type">> => <<"text">>,
        <<"content">> => <<"intbe02 group hello"/utf8>>
    },
    R10 = with_dns(fun() ->
        intbe02_http_support:http(
            Port,
            <<"POST">>,
            <<GrpPath/binary, "/messages">>,
            Body10,
            maps:merge(A, intbe02_http_support:idem(<<"intbe02-idem-10">>))
        )
    end),
    #{<<"msg_id">> := _} = assert_ok_json(R10),
    cover(<<"INT-10">>),

    %% ---- INT-11 POST friend-requests（H2 → H1 pending）----
    Body11 = #{<<"sender_user_id">> => H2, <<"target_user_id">> => H1},
    R11 =
        intbe02_http_support:http(
            Port,
            <<"POST">>,
            <<"/api/internal/v1/friend-requests">>,
            Body11,
            maps:merge(A, intbe02_http_support:idem(<<"intbe02-idem-11">>))
        ),
    #{<<"request_status">> := <<"pending">>} = assert_ok_json(R11),
    cover(<<"INT-11">>),

    %% ---- INT-12 PUT webhook（SSRF 守卫走 DNS 替身；事件⊂白名单）----
    Body12 = #{
        <<"url">> => <<"https://oa.customer.example.com/intbe02/hook">>,
        <<"events">> => [<<"message.enterprise.accepted">>, <<"file.confirmed">>]
    },
    R12 = with_dns(fun() ->
        intbe02_http_support:http(
            Port,
            <<"PUT">>,
            <<"/api/internal/v1/webhook">>,
            Body12,
            maps:merge(A, intbe02_http_support:idem(<<"intbe02-idem-12">>))
        )
    end),
    #{<<"url">> := _, <<"events">> := _} = assert_ok_json(R12),
    cover(<<"INT-12">>),

    %% ---- INT-23 GET webhook/deliveries（principal 面；有/无行均 200 页形态）----
    R23 = intbe02_http_support:http(
        Port, <<"GET">>, <<"/api/internal/v1/webhook/deliveries">>, <<>>, A
    ),
    assert_ok_json(R23),
    cover(<<"INT-23">>),

    %% ---- INT-13 POST deliveries/{id}/replay（终态行 → 新投递行）----
    R13 = with_dns(fun() ->
        intbe02_http_support:http(
            Port,
            <<"POST">>,
            <<"/api/internal/v1/webhook/deliveries/intbe02-dlv-0001/replay">>,
            #{},
            maps:merge(A, intbe02_http_support:idem(<<"intbe02-idem-13">>))
        )
    end),
    #{<<"replayed">> := true} = assert_ok_json(R13),
    cover(<<"INT-13">>),

    %% ---- INT-32 POST webhook/test-delivery（端点已配置 → ping 入箱）----
    R32 = with_dns(fun() ->
        intbe02_http_support:http(
            Port,
            <<"POST">>,
            <<"/api/internal/v1/webhook/test-delivery">>,
            #{},
            maps:merge(A, intbe02_http_support:idem(<<"intbe02-idem-32">>))
        )
    end),
    #{
        <<"enqueued">> := true,
        <<"event_type">> := <<"webhook.ping">>,
        <<"delivery_id">> := _
    } = assert_ok_json(R32),
    cover(<<"INT-32">>),

    %% ---- INT-14 POST oa/sso/exchange（真签发真消费；code 一次性）----
    {ok, #{<<"code">> := SsoCode}} =
        enterprise_oa_sso_logic:issue_code_tx(C, 995017, #{
            <<"application_key">> => <<"intbe02-oa-sso">>,
            <<"redirect_uri">> => Redirect,
            <<"nonce">> => Nonce
        }),
    Body14 = #{<<"code">> => SsoCode, <<"redirect_uri">> => Redirect, <<"nonce">> => Nonce},
    R14 =
        intbe02_http_support:http(
            Port, <<"POST">>, <<"/api/internal/v1/oa/sso/exchange">>, Body14, SSO
        ),
    #{<<"user_id">> := 995017, <<"external_user_id">> := _} = assert_ok_json(R14),
    cover(<<"INT-14">>),
    %% code 单次消费（合同豁免 Idempotency-Key，一次性由 CAS 保证）：重放同
    %% code → 统一不透明 404（不给存在性/生命周期 oracle）
    R14b =
        intbe02_http_support:http(
            Port, <<"POST">>, <<"/api/internal/v1/oa/sso/exchange">>, Body14, SSO
        ),
    assert_err(R14b, <<"resource_not_found">>),

    %% ---- INT-15 DELETE identity-mappings（撤销 INT-02 绑定）----
    Body15 = #{<<"external_user_id">> => <<"intbe02-ext-h4">>},
    R15 =
        intbe02_http_support:http(
            Port,
            <<"DELETE">>,
            <<"/api/internal/v1/identity-mappings">>,
            Body15,
            maps:merge(A, intbe02_http_support:idem(<<"intbe02-idem-15">>))
        ),
    assert_ok_json(R15),
    cover(<<"INT-15">>),

    %% ---- INT-16 POST identity-mappings/directory（分页页形态）----
    R16 =
        intbe02_http_support:http(
            Port,
            <<"POST">>,
            <<"/api/internal/v1/identity-mappings/directory">>,
            #{<<"page_size">> => 10},
            A
        ),
    #{<<"items">> := Items16} = assert_page(R16),
    ?assertEqual(
        lists:sort([H1, H2, H3]),
        lists:sort([maps:get(<<"external_user_id">>, M) || M <- Items16])
    ),
    cover(<<"INT-16">>),

    %% ---- INT-17 POST directory/users（workspace 过滤 + 最小字段）----
    R17 =
        intbe02_http_support:http(
            Port,
            <<"POST">>,
            <<"/api/internal/v1/directory/users">>,
            #{<<"workspace_id">> => WsA1},
            A
        ),
    #{<<"items">> := Items17} = assert_page(R17),
    ?assertNotEqual([], Items17),
    cover(<<"INT-17">>),

    %% ---- INT-24 GET /workspaces（org 全域 Grant → 两个 active WS 全见）----
    R24 = intbe02_http_support:http(Port, <<"GET">>, <<"/api/internal/v1/workspaces">>, <<>>, A),
    #{<<"items">> := Items24} = assert_page(R24),
    WsIds24 = [maps:get(<<"workspace_id">>, M) || M <- Items24],
    ?assert(lists:member(WsA1, WsIds24)),
    ?assert(lists:member(WsA2, WsIds24)),
    cover(<<"INT-24">>),

    %% ---- INT-25 GET /workspaces/{id} 详情 ----
    WsPath = <<"/api/internal/v1/workspaces/", (integer_to_binary(WsA1))/binary>>,
    R25 = intbe02_http_support:http(Port, <<"GET">>, WsPath, <<>>, A),
    #{<<"workspace_id">> := WsA1} = assert_ok_json(R25),
    cover(<<"INT-25">>),

    %% ---- INT-26 GET /groups（origin=本 App：企业群可见、人类群不可见）----
    R26 = intbe02_http_support:http(Port, <<"GET">>, <<"/api/internal/v1/groups">>, <<>>, A),
    #{<<"items">> := Items26} = assert_page(R26),
    Gids26 = [maps:get(<<"group_id">>, M) || M <- Items26],
    ?assert(lists:member(Gid, Gids26)),
    ?assertNot(lists:member(GrpHuman, Gids26)),
    cover(<<"INT-26">>),

    %% ---- INT-28 GET /projects?workspace_id（W 必填过滤）----
    R28 =
        intbe02_http_support:http(
            Port,
            <<"GET">>,
            <<"/api/internal/v1/projects?workspace_id=", (integer_to_binary(WsA1))/binary>>,
            <<>>,
            A
        ),
    #{<<"items">> := Items28} = assert_page(R28),
    ?assert(lists:member(PrjA1, [maps:get(<<"project_id">>, M) || M <- Items28])),
    cover(<<"INT-28">>),

    %% ---- INT-29 GET /projects/{id} 详情 ----
    PrjPath = <<"/api/internal/v1/projects/", (integer_to_binary(PrjA1))/binary>>,
    R29 = intbe02_http_support:http(Port, <<"GET">>, PrjPath, <<>>, A),
    #{<<"project_id">> := PrjA1} = assert_ok_json(R29),
    cover(<<"INT-29">>),

    %% ---- INT-30 GET /channels?workspace_id（scope=workspace AND status=1）----
    R30 =
        intbe02_http_support:http(
            Port,
            <<"GET">>,
            <<"/api/internal/v1/channels?workspace_id=", (integer_to_binary(WsA1))/binary>>,
            <<>>,
            A
        ),
    #{<<"items">> := Items30} = assert_page(R30),
    ?assertEqual([ChnA1], [maps:get(<<"channel_id">>, M) || M <- Items30]),
    cover(<<"INT-30">>),

    %% ---- INT-31 GET /channels/{id} 详情 ----
    ChnPath = <<"/api/internal/v1/channels/", (integer_to_binary(ChnA1))/binary>>,
    R31 = intbe02_http_support:http(Port, <<"GET">>, ChnPath, <<>>, A),
    #{<<"channel_id">> := ChnA1} = assert_ok_json(R31),
    cover(<<"INT-31">>),

    %% Seat configuration is org-owned, with workspace selected for auditing only.
    SeatPath = <<"/api/internal/v1/customer-service/seats/995701">>,
    Body35 = #{<<"workspace_id">> => WsA1, <<"business_identity_id">> => 995701},
    R35 = intbe02_http_support:http(
        Port,
        <<"POST">>,
        <<"/api/internal/v1/customer-service/seats">>,
        Body35,
        maps:merge(A, intbe02_http_support:idem(<<"intbe02-idem-35">>))
    ),
    #{<<"business_identity_id">> := 995701, <<"version">> := SeatVersion} = assert_ok_json(R35),
    cover(<<"INT-35">>),
    R33 = intbe02_http_support:http(
        Port,
        <<"GET">>,
        <<"/api/internal/v1/customer-service/seats">>,
        <<>>,
        A
    ),
    #{<<"items">> := SeatItems} = assert_page(R33),
    ?assert(
        lists:any(fun(Row) -> maps:get(<<"business_identity_id">>, Row) =:= 995701 end, SeatItems)
    ),
    cover(<<"INT-33">>),
    R34 = intbe02_http_support:http(Port, <<"GET">>, SeatPath, <<>>, A),
    #{<<"business_identity_id">> := 995701} = assert_ok_json(R34),
    cover(<<"INT-34">>),
    Body36 = #{
        <<"workspace_id">> => WsA1,
        <<"expected_version">> => SeatVersion,
        <<"enabled">> => false
    },
    R36 = intbe02_http_support:http(
        Port,
        <<"PATCH">>,
        SeatPath,
        Body36,
        maps:merge(A, intbe02_http_support:idem(<<"intbe02-idem-36">>))
    ),
    #{<<"enabled">> := false, <<"version">> := NextSeatVersion} = assert_ok_json(R36),
    ?assertEqual(SeatVersion + 1, NextSeatVersion),
    cover(<<"INT-36">>),

    %% ---- INT-21 DELETE /groups/{id}（归档终态，最后执行）----
    R21 =
        intbe02_http_support:http(
            Port,
            <<"DELETE">>,
            GrpPath,
            #{},
            maps:merge(A, intbe02_http_support:idem(<<"intbe02-idem-21">>))
        ),
    assert_ok_json(R21),
    cover(<<"INT-21">>),

    %% 重放规格表（幂等矩阵数据源）：{Id, Method, Path, BodyBin, Key, FirstBody}
    [
        {<<"INT-02">>, <<"PUT">>, <<"/api/internal/v1/identity-mappings">>, Body02,
            <<"intbe02-idem-02">>, R02},
        {<<"INT-04">>, <<"POST">>, <<"/api/internal/v1/groups">>, Body04, <<"intbe02-idem-04">>,
            R04},
        {<<"INT-05">>, <<"PUT">>, GrpMembersPath, Body05, <<"intbe02-idem-05">>, R05},
        {<<"INT-20">>, <<"PUT">>, <<GrpMembersPath/binary, "/roles">>, Body20,
            <<"intbe02-idem-20">>, R20},
        {<<"INT-19">>, <<"PATCH">>, GrpPath, Body19, <<"intbe02-idem-19">>, R19},
        {<<"INT-06">>, <<"DELETE">>, GrpMembersPath, Body06, <<"intbe02-idem-06">>, R06},
        {<<"INT-07">>, <<"POST">>, <<"/api/internal/v1/files/presign">>, Body07,
            <<"intbe02-idem-07">>, R07},
        {<<"INT-08">>, <<"POST">>, <<"/api/internal/v1/files/confirm">>, Body08,
            <<"intbe02-idem-08">>, R08},
        {<<"INT-22">>, <<"POST">>, <<"/api/internal/v1/files/governance">>, Body22,
            <<"intbe02-idem-22">>, R22},
        {<<"INT-09">>, <<"POST">>, <<"/api/internal/v1/messages/direct">>, Body09,
            <<"intbe02-idem-09">>, R09},
        {<<"INT-10">>, <<"POST">>, <<GrpPath/binary, "/messages">>, Body10, <<"intbe02-idem-10">>,
            R10},
        {<<"INT-11">>, <<"POST">>, <<"/api/internal/v1/friend-requests">>, Body11,
            <<"intbe02-idem-11">>, R11},
        {<<"INT-12">>, <<"PUT">>, <<"/api/internal/v1/webhook">>, Body12, <<"intbe02-idem-12">>,
            R12},
        {<<"INT-13">>, <<"POST">>,
            <<"/api/internal/v1/webhook/deliveries/intbe02-dlv-0001/replay">>, #{},
            <<"intbe02-idem-13">>, R13},
        {<<"INT-15">>, <<"DELETE">>, <<"/api/internal/v1/identity-mappings">>, Body15,
            <<"intbe02-idem-15">>, R15},
        {<<"INT-21">>, <<"DELETE">>, GrpPath, #{}, <<"intbe02-idem-21">>, R21},
        {<<"INT-32">>, <<"POST">>, <<"/api/internal/v1/webhook/test-delivery">>, #{},
            <<"intbe02-idem-32">>, R32},
        {<<"INT-35">>, <<"POST">>, <<"/api/internal/v1/customer-service/seats">>, Body35,
            <<"intbe02-idem-35">>, R35},
        {<<"INT-36">>, <<"PATCH">>, SeatPath, Body36, <<"intbe02-idem-36">>, R36}
    ].

seat_concurrent_update(S) ->
    Port = maps:get(port, S),
    A = intbe02_http_support:auth(maps:get(cred_a, S)),
    Path = <<"/api/internal/v1/customer-service/seats/995701">>,
    Parent = self(),
    Workers = [
        spawn(fun() ->
            receive
                go ->
                    Response = intbe02_http_support:http(
                        Port,
                        <<"PATCH">>,
                        Path,
                        #{
                            <<"workspace_id">> => 995201,
                            <<"expected_version">> => 2,
                            <<"max_concurrent">> => N
                        },
                        maps:merge(
                            A,
                            intbe02_http_support:idem(
                                <<"seat-concurrent-", (integer_to_binary(N))/binary>>
                            )
                        )
                    ),
                    Parent ! {self(), Response}
            end
        end)
     || N <- [2, 3]
    ],
    [Pid ! go || Pid <- Workers],
    Responses = [
        receive
            {Pid, R} -> R
        after 10000 -> error(seat_concurrency_timeout)
        end
     || Pid <- Workers
    ],
    ?assertEqual([200, 409], lists:sort([maps:get(status, R) || R <- Responses])),
    [assert_err(R, <<"version_conflict">>) || R <- Responses, maps:get(status, R) =:= 409],
    #{<<"version">> := 3} = assert_ok_json(
        intbe02_http_support:http(Port, <<"GET">>, Path, <<>>, A)
    ),
    seat_audit_rollback(S, A, Path).

seat_audit_rollback(S, A, Path) ->
    C = maps:get(conn, S),
    Port = maps:get(port, S),
    ok = intbe02_http_support:sql_exec(
        C,
        <<"ALTER TABLE customer_service_event ADD CONSTRAINT synthetic_seat_audit_failure CHECK (false) NOT VALID">>
    ),
    try
        R = intbe02_http_support:http(
            Port,
            <<"PATCH">>,
            Path,
            #{<<"workspace_id">> => 995201, <<"expected_version">> => 3, <<"max_concurrent">> => 5},
            maps:merge(A, intbe02_http_support:idem(<<"seat-audit-rollback">>))
        ),
        assert_err(R, <<"internal_error">>),
        #{<<"version">> := 3} = assert_ok_json(
            intbe02_http_support:http(Port, <<"GET">>, Path, <<>>, A)
        ),
        #{<<"n">> := 0} = intbe02_http_support:one(
            C,
            <<"SELECT count(*) AS n FROM enterprise_internal_idempotency WHERE idempotency_key='seat-audit-rollback'">>
        )
    after
        ok = intbe02_http_support:sql_exec(
            C, <<"ALTER TABLE customer_service_event DROP CONSTRAINT synthetic_seat_audit_failure">>
        )
    end.

seat_negative_matrix(S) ->
    Port = maps:get(port, S),
    C = maps:get(conn, S),
    A = intbe02_http_support:auth(maps:get(cred_a, S)),
    RO = intbe02_http_support:auth(maps:get(cred_ro, S)),
    Narrow = intbe02_http_support:auth(maps:get(cred_c, S)),
    Seats = <<"/api/internal/v1/customer-service/seats">>,
    Detail = <<Seats/binary, "/995701">>,
    lists:foreach(
        fun({Method, Path}) ->
            assert_err(
                intbe02_http_support:http(
                    Port,
                    Method,
                    Path,
                    #{},
                    maps:merge(RO, intbe02_http_support:idem(<<"seat-missing-scope">>))
                ),
                <<"insufficient_scope">>
            )
        end,
        [
            {<<"GET">>, Seats},
            {<<"GET">>, Detail},
            {<<"POST">>, Seats},
            {<<"PATCH">>, Detail}
        ]
    ),
    assert_err(
        intbe02_http_support:http(Port, <<"GET">>, Seats, <<>>, Narrow),
        <<"organization_boundary_violation">>
    ),
    assert_err(
        intbe02_http_support:http(Port, <<"GET">>, <<Seats/binary, "/995702">>, <<>>, A),
        <<"resource_not_found">>
    ),
    Ws = intbe02_http_support:fixture(ws_a1, x),
    Request = fun(Method, Path, Body, Key) ->
        intbe02_http_support:http(
            Port,
            Method,
            Path,
            Body,
            maps:merge(A, intbe02_http_support:idem(Key))
        )
    end,
    assert_err(
        Request(
            <<"PATCH">>,
            Detail,
            #{<<"workspace_id">> => Ws, <<"expected_version">> => 1, <<"enabled">> => true},
            <<"seat-stale-version">>
        ),
        <<"version_conflict">>
    ),
    assert_err(
        Request(
            <<"POST">>,
            Seats,
            #{<<"workspace_id">> => Ws, <<"business_identity_id">> => 995701},
            <<"seat-duplicate">>
        ),
        <<"resource_conflict">>
    ),
    assert_err(
        Request(
            <<"POST">>,
            Seats,
            #{<<"workspace_id">> => Ws, <<"business_identity_id">> => 995702},
            <<"seat-foreign">>
        ),
        <<"resource_not_found">>
    ),
    AppId = integer_to_binary(maps:get(app_a, S)),
    ok = intbe02_http_support:sql_exec(
        C,
        <<"DELETE FROM enterprise_application_grant_scope s USING enterprise_application_grant g WHERE s.grant_id=g.id AND g.organization_id=995101 AND g.application_id=",
            AppId/binary, " AND s.scope='customer_service:write'">>
    ),
    %% Replaying a committed key must still fail after the write grant is revoked.
    assert_err(
        Request(
            <<"PATCH">>,
            Detail,
            #{<<"workspace_id">> => Ws, <<"expected_version">> => 1, <<"enabled">> => false},
            <<"intbe02-idem-36">>
        ),
        <<"insufficient_scope">>
    ).

%% ------------------------------------------------------------------
%% ② 幂等矩阵：17 条 required mutation 逐一同 key 同 body 精确重放 +
%%    同 key 异 body 409 + 缺/畸形 key 400
%% ------------------------------------------------------------------

idempotency_matrix(S, Specs) ->
    Port = maps:get(port, S),
    A = intbe02_http_support:auth(maps:get(cred_a, S)),
    lists:foreach(
        fun({_Id, Method, Path, Body, Key, First}) ->
            %% 重放命中幂等缓存：不重执行业务（无 HEAD/DNS/副作用），逐字节
            %% 同 body + 同 status（§11 幂等 response 快照语义）
            Replay =
                intbe02_http_support:http(
                    Port,
                    Method,
                    Path,
                    Body,
                    maps:merge(A, intbe02_http_support:idem(Key))
                ),
            ?assertEqual(200, maps:get(status, Replay), {replay_status, Key}),
            ?assertEqual(maps:get(body, First), maps:get(body, Replay), {replay_body, Key}),
            ?assertEqual(
                <<"true">>, intbe02_http_support:json_header_val(Replay, <<"idempotent-replayed">>)
            )
        end,
        Specs
    ),
    %% Newly added mutations: digest conflict and absent/malformed key for each route.
    lists:foreach(
        fun({Id, Method, Path, Body, Key, _First}) ->
            case lists:member(Id, [<<"INT-35">>, <<"INT-36">>]) of
                false ->
                    ok;
                true ->
                    Different = Body#{<<"enabled">> => true, <<"max_concurrent">> => 7},
                    assert_err(
                        intbe02_http_support:http(
                            Port,
                            Method,
                            Path,
                            Different,
                            maps:merge(A, intbe02_http_support:idem(Key))
                        ),
                        <<"idempotency_conflict">>
                    ),
                    assert_err(
                        intbe02_http_support:http(Port, Method, Path, Body, A),
                        <<"invalid_request">>
                    ),
                    assert_err(
                        intbe02_http_support:http(
                            Port,
                            Method,
                            Path,
                            Body,
                            maps:merge(A, intbe02_http_support:idem(binary:copy(<<"k">>, 129)))
                        ),
                        <<"invalid_request">>
                    )
            end
        end,
        Specs
    ),
    %% 同 key 异 payload → 409 idempotency_conflict（digest 冲突）
    Conflict =
        intbe02_http_support:http(
            Port,
            <<"PATCH">>,
            <<"/api/internal/v1/groups/", (grp_id_from(Specs))/binary>>,
            #{<<"title">> => <<"intbe02 conflict probe"/utf8>>},
            maps:merge(A, intbe02_http_support:idem(<<"intbe02-idem-19">>))
        ),
    assert_err(Conflict, <<"idempotency_conflict">>),
    %% mutation 缺 Idempotency-Key → 400 invalid_request（INV-7）
    NoKey =
        intbe02_http_support:http(
            Port,
            <<"PUT">>,
            <<"/api/internal/v1/identity-mappings">>,
            #{<<"external_user_id">> => <<"intbe02-ext-nk">>, <<"user_id">> => 995017},
            A
        ),
    assert_err(NoKey, <<"invalid_request">>),
    %% 畸形 key（>128 可打印 ASCII）→ 同一 stable 码
    LongKey = binary:copy(<<"k">>, 129),
    BadKey =
        intbe02_http_support:http(
            Port,
            <<"PUT">>,
            <<"/api/internal/v1/identity-mappings">>,
            #{<<"external_user_id">> => <<"intbe02-ext-bk">>, <<"user_id">> => 995017},
            maps:merge(A, intbe02_http_support:idem(LongKey))
        ),
    assert_err(BadKey, <<"invalid_request">>).

grp_id_from(Specs) ->
    %% 从 INT-04 的正例响应回收 group_id（幂等矩阵的路径复用）
    {_, _, _, #{<<"workspace_id">> := _}, _, First} =
        lists:keyfind(<<"INT-04">>, 1, Specs),
    #{<<"group_id">> := Gid} = assert_ok_json(First),
    integer_to_binary(Gid).

%% ------------------------------------------------------------------
%% ③ negative 三件套
%% ------------------------------------------------------------------

negative_matrix(S) ->
    Port = maps:get(port, S),
    %% (a) 无 credential：31 条逐一 → 401 invalid_credential（认证链第一关，
    %%     不触碰 PG；路径绑定段用整数 1 保证路由命中）
    lists:foreach(
        fun(R) ->
            Method = maps:get(method, R),
            Path = concrete_path(maps:get(path, R)),
            Resp = intbe02_http_support:http(Port, Method, Path, #{}),
            assert_err(Resp, <<"invalid_credential">>)
        end,
        enterprise_internal_routes:routes()
    ),

    A = intbe02_http_support:auth(maps:get(cred_a, S)),
    RO = intbe02_http_support:auth(maps:get(cred_ro, S)),
    D = intbe02_http_support:auth(maps:get(cred_d, S)),
    Cnarrow = intbe02_http_support:auth(maps:get(cred_c, S)),
    W = intbe02_http_support:auth(maps:get(cred_w, S)),
    WsA1 = intbe02_http_support:fixture(ws_a1, x),
    WsA2 = intbe02_http_support:fixture(ws_a2, x),
    WsB1 = intbe02_http_support:fixture(ws_b1, x),
    GrpB = intbe02_http_support:fixture(grp_b, x),
    H1 = intbe02_http_support:fixture(ext_h1, x),
    GrpBPath = <<"/api/internal/v1/groups/", (integer_to_binary(GrpB))/binary>>,

    %% (b) 缺 scope：RO 应用（仅 application:read）
    %%     GET /application 是它唯一可达端点（最小 scope 正例对照）
    Rro =
        intbe02_http_support:http(Port, <<"GET">>, <<"/api/internal/v1/application">>, <<>>, RO),
    assert_ok_json(Rro),
    lists:foreach(
        fun({Method, Path, Body}) ->
            Resp = intbe02_http_support:http(
                Port,
                Method,
                Path,
                Body,
                maps:merge(RO, intbe02_http_support:idem(<<"intbe02-idem-ro-", Method/binary>>))
            ),
            assert_err(Resp, <<"insufficient_scope">>)
        end,
        [
            {<<"PUT">>, <<"/api/internal/v1/identity-mappings">>, #{
                <<"external_user_id">> => <<"x">>, <<"user_id">> => 995011
            }},
            {<<"POST">>, <<"/api/internal/v1/identity-mappings/resolve">>, #{
                <<"external_user_ids">> => [H1]
            }},
            {<<"POST">>, <<"/api/internal/v1/groups">>, #{
                <<"workspace_id">> => WsA1, <<"title">> => <<"t"/utf8>>, <<"members">> => [H1]
            }},
            {<<"PUT">>, <<"/api/internal/v1/groups/1/members">>, #{<<"external_user_ids">> => [H1]}},
            {<<"DELETE">>, <<"/api/internal/v1/groups/1/members">>, #{
                <<"external_user_ids">> => [H1]
            }},
            {<<"POST">>, <<"/api/internal/v1/files/presign">>, #{
                <<"file_name">> => <<"a.txt">>, <<"mime_type">> => <<"text/plain">>
            }},
            {<<"POST">>, <<"/api/internal/v1/files/confirm">>, #{<<"object_key">> => <<"k">>}},
            {<<"POST">>, <<"/api/internal/v1/files/governance">>, #{
                <<"op">> => <<"hold">>, <<"object_key">> => <<"k">>, <<"reason">> => <<"r"/utf8>>
            }},
            {<<"POST">>, <<"/api/internal/v1/friend-requests">>, #{
                <<"sender_user_id">> => H1, <<"target_user_id">> => <<"y">>
            }},
            {<<"PUT">>, <<"/api/internal/v1/webhook">>, #{
                <<"url">> => <<"https://oa.customer.example.com/h">>,
                <<"events">> => [<<"file.confirmed">>]
            }},
            {<<"POST">>, <<"/api/internal/v1/webhook/deliveries/1/replay">>, #{}},
            {<<"POST">>, <<"/api/internal/v1/oa/sso/exchange">>, #{
                <<"code">> => <<"oa_sso_x">>,
                <<"redirect_uri">> => <<"https://x/y">>,
                <<"nonce">> => <<"nonce_x">>
            }},
            {<<"POST">>, <<"/api/internal/v1/identity-mappings/directory">>, #{}},
            {<<"POST">>, <<"/api/internal/v1/directory/users">>, #{}},
            {<<"GET">>, <<"/api/internal/v1/groups">>, #{}},
            {<<"GET">>, <<"/api/internal/v1/groups/1">>, #{}},
            {<<"PATCH">>, <<"/api/internal/v1/groups/1">>, #{<<"title">> => <<"t"/utf8>>}},
            {<<"PUT">>, <<"/api/internal/v1/groups/1/members/roles">>, #{<<"roles">> => []}},
            {<<"DELETE">>, <<"/api/internal/v1/groups/1">>, #{}},
            {<<"GET">>, <<"/api/internal/v1/groups/1/members">>, #{}},
            {<<"GET">>, <<"/api/internal/v1/workspaces">>, #{}},
            {<<"GET">>, <<"/api/internal/v1/workspaces/1">>, #{}},
            {<<"GET">>, <<"/api/internal/v1/projects?workspace_id=1">>, #{}},
            {<<"GET">>, <<"/api/internal/v1/projects/1">>, #{}},
            {<<"GET">>, <<"/api/internal/v1/channels?workspace_id=1">>, #{}},
            {<<"GET">>, <<"/api/internal/v1/channels/1">>, #{}}
        ]
    ),
    %% 动态 scope 路由（INT-09/10，manifest dynamic_scope_usage）不进静态
    %% scope_gate——由 handler 按 sender_mode 裁决，故单独断言：
    %%   * INT-09 direct：sender_mode 解析先于资源定位 → 403 insufficient_scope；
    Rdyn9 =
        intbe02_http_support:http(
            Port,
            <<"POST">>,
            <<"/api/internal/v1/messages/direct">>,
            #{
                <<"sender_mode">> => <<"application">>,
                <<"recipient_user_id">> => H1,
                <<"msg_type">> => <<"text">>,
                <<"content">> => <<"c"/utf8>>
            },
            maps:merge(RO, intbe02_http_support:idem(<<"intbe02-idem-ro-d9">>))
        ),
    assert_err(Rdyn9, <<"insufficient_scope">>),
    %%   * INT-10 group messages：群定位先于 scope 裁决，deny precedence
    %%     （404 先于 403，与 INT-25 详情「同不存在同体」语义一致）→ 统一 404。
    Rdyn10 =
        intbe02_http_support:http(
            Port,
            <<"POST">>,
            <<"/api/internal/v1/groups/1/messages">>,
            #{
                <<"sender_mode">> => <<"application">>,
                <<"msg_type">> => <<"text">>,
                <<"content">> => <<"c"/utf8>>
            },
            maps:merge(RO, intbe02_http_support:idem(<<"intbe02-idem-ro-d10">>))
        ),
    assert_err(Rdyn10, <<"resource_not_found">>),
    %% (c) 零 Grant：allowed_scopes 有值但无任何生效 Grant → granted=空集 → 403
    Rd =
        intbe02_http_support:http(Port, <<"GET">>, <<"/api/internal/v1/application">>, <<>>, D),
    assert_err(Rd, <<"insufficient_scope">>),

    %% (d) 未覆盖 Workspace（App C 显式 Grant 仅 WS_A1）→ 同体 403
    WsA2Path = <<"/api/internal/v1/workspaces/", (integer_to_binary(WsA2))/binary>>,
    Rwsc =
        intbe02_http_support:http(Port, <<"GET">>, WsA2Path, <<>>, Cnarrow),
    assert_err(Rwsc, <<"organization_boundary_violation">>),
    Rwsc2 =
        intbe02_http_support:http(
            Port,
            <<"GET">>,
            <<"/api/internal/v1/projects?workspace_id=", (integer_to_binary(WsA2))/binary>>,
            <<>>,
            Cnarrow
        ),
    assert_err(Rwsc2, <<"organization_boundary_violation">>),
    Rwsc3 =
        intbe02_http_support:http(
            Port,
            <<"GET">>,
            <<"/api/internal/v1/channels?workspace_id=", (integer_to_binary(WsA2))/binary>>,
            <<>>,
            Cnarrow
        ),
    assert_err(Rwsc3, <<"organization_boundary_violation">>),
    %% scope groups:write 由窄写应用（cred_w）满足——失败只能来自 Grant 覆盖
    Rwsc4 =
        intbe02_http_support:http(
            Port,
            <<"POST">>,
            <<"/api/internal/v1/groups">>,
            #{<<"workspace_id">> => WsA2, <<"title">> => <<"t"/utf8>>, <<"members">> => [H1]},
            maps:merge(W, intbe02_http_support:idem(<<"intbe02-idem-cn">>))
        ),
    assert_err(Rwsc4, <<"organization_boundary_violation">>),
    %% 列表行级收窄（kind=list）：显式 Grant 只见覆盖内 WS
    Rwsc5 = intbe02_http_support:http(
        Port, <<"GET">>, <<"/api/internal/v1/workspaces">>, <<>>, Cnarrow
    ),
    #{<<"items">> := ItemsC} = assert_page(Rwsc5),
    ?assertEqual([WsA1], [maps:get(<<"workspace_id">>, M) || M <- ItemsC]),

    %% (e) 跨 Org 同体拒绝：Org A 凭证访问 Org B 资源
    %%     资源在 org 定位阶段不可见 → 统一 404（不给跨 Org 存在性 oracle）
    WsB1Path = <<"/api/internal/v1/workspaces/", (integer_to_binary(WsB1))/binary>>,
    Rx1 = intbe02_http_support:http(Port, <<"GET">>, WsB1Path, <<>>, A),
    assert_err(Rx1, <<"resource_not_found">>),
    lists:foreach(
        fun({Method, Path, Body}) ->
            Resp = intbe02_http_support:http(
                Port,
                Method,
                Path,
                Body,
                maps:merge(A, intbe02_http_support:idem(<<"intbe02-idem-x-", Method/binary>>))
            ),
            assert_err(Resp, <<"resource_not_found">>)
        end,
        [
            {<<"GET">>, GrpBPath, #{}},
            {<<"PATCH">>, GrpBPath, #{<<"title">> => <<"t"/utf8>>}},
            {<<"DELETE">>, GrpBPath, #{}},
            {<<"PUT">>, <<GrpBPath/binary, "/members">>, #{<<"external_user_ids">> => [H1]}},
            {<<"DELETE">>, <<GrpBPath/binary, "/members">>, #{<<"external_user_ids">> => [H1]}},
            {<<"PUT">>, <<GrpBPath/binary, "/members/roles">>, #{<<"roles">> => []}},
            {<<"POST">>, <<GrpBPath/binary, "/messages">>, #{
                <<"sender_mode">> => <<"application">>,
                <<"msg_type">> => <<"text">>,
                <<"content">> => <<"c"/utf8>>
            }},
            {<<"GET">>, <<GrpBPath/binary, "/members">>, #{}}
        ]
    ),
    %% INT-04 的边界资源在请求体（workspace_id=WS_B1 属 Org B）→ 403 同体
    Rxc =
        intbe02_http_support:http(
            Port,
            <<"POST">>,
            <<"/api/internal/v1/groups">>,
            #{<<"workspace_id">> => WsB1, <<"title">> => <<"t"/utf8>>, <<"members">> => [H1]},
            maps:merge(A, intbe02_http_support:idem(<<"intbe02-idem-xc">>))
        ),
    assert_err(Rxc, <<"organization_boundary_violation">>),
    %% 映射面 org 内收敛：Org B 侧标识在 Org A 不可见 → 422 identity_not_mapped
    Rxd =
        intbe02_http_support:http(
            Port,
            <<"POST">>,
            <<"/api/internal/v1/messages/direct">>,
            #{
                <<"sender_mode">> => <<"application">>,
                <<"recipient_user_id">> => <<"intbe02-ext-b1">>,
                <<"msg_type">> => <<"text">>,
                <<"content">> => <<"c"/utf8>>
            },
            maps:merge(A, intbe02_http_support:idem(<<"intbe02-idem-xd">>))
        ),
    assert_err(Rxd, <<"identity_not_mapped">>),
    ok.

%% ------------------------------------------------------------------
%% ④ covered_operation_ids：登记集合 ↔ 冻结表 32 ID 精确相等
%% ------------------------------------------------------------------

assert_coverage() ->
    Expected =
        lists:sort([maps:get(id, R) || R <- enterprise_internal_routes:routes()]),
    Actual = intbe02_http_support:covered_ids(),
    ?assertEqual(42, length(Expected)),
    ?assertEqual(Expected, Actual, {coverage_gap, Expected -- Actual, Actual -- Expected}),
    write_coverage(Actual).

write_coverage(Actual) ->
    case os:getenv("IMBOY_GATE_RUN_DIR") of
        false ->
            ok;
        Dir ->
            Rows = [
                #{
                    id => maps:get(id, R),
                    method => maps:get(method, R),
                    path => maps:get(path, R),
                    behavior_assertions_passed => true
                }
             || R <- enterprise_internal_routes:routes(), lists:member(maps:get(id, R), Actual)
            ],
            ok = file:write_file(
                filename:join(Dir, "internal-operation-coverage.json"),
                jsone:encode(#{
                    covered_operation_ids => Actual,
                    operations => Rows,
                    external_limits => [
                        <<"object HEAD stub for INT-08">>,
                        <<"public DNS fixture; no real outbound webhook delivery">>
                    ]
                })
            )
    end.

%%%===================================================================
%%% conformance 内部助手
%%%===================================================================

cover(Id) -> ok = intbe02_http_support:cover(Id, ok).

with_dns(Fun) -> intbe02_http_support:with_public_dns(Fun).

%% 错误信封（support:http 的 map 响应形态）：HTTP 状态与信封 code 均按 A2
%% 冻结映射（enterprise_internal_error）；与上方老 wiring 段的
%% assert_error_envelope/2（raw binary 形态）语义一致、载体不同。
assert_err(Resp, Code) ->
    ?assert(is_binary(Code)),
    ?assertEqual(
        enterprise_internal_error:http_status(Code),
        maps:get(status, Resp),
        {status_mismatch, Code, maps:get(status, Resp), maps:get(body, Resp)}
    ),
    Body = maps:get(body, Resp),
    assert_error_json(Body, Code).

%% The previous substring oracle accepted a wrong code if the message matched.
error_envelope_requires_exact_code_test() ->
    Forged = jsone:encode(#{
        <<"error">> => #{
            <<"code">> => <<"invalid_request">>, <<"message">> => <<"invalid_credential">>
        }
    }),
    ?assertException(
        error,
        {assertEqual, _},
        assert_error_json(Forged, <<"invalid_credential">>)
    ).

integration_readme_scope_contract_test() ->
    {ok, Readme} = file:read_file("api/internal/v1/README.md"),
    {match, Captures} = re:run(
        Readme,
        <<"\\| `([a-z_]+:[a-z_]+)` \\|">>,
        [global, {capture, [1], binary}]
    ),
    ?assertEqual(
        lists:sort(enterprise_internal_scope:all()),
        lists:usort([Scope || [Scope] <- Captures])
    ),
    ?assertNotEqual(
        nomatch,
        binary:match(
            Readme,
            <<"Authorization: Bearer <credential_prefix>.<secret>">>
        )
    ).

assert_error_json(Body, Code) ->
    #{<<"error">> := Error} = Json = jsone:decode(Body),
    ?assertEqual([<<"error">>], maps:keys(Json)),
    ?assertEqual([<<"code">>, <<"message">>], lists:sort(maps:keys(Error))),
    ?assertEqual(Code, maps:get(<<"code">>, Error)),
    Message = maps:get(<<"message">>, Error),
    ?assert(is_binary(Message) andalso byte_size(Message) > 0),
    ?assertEqual(nomatch, binary:match(Body, <<"deadbeef">>)).

%% 200 + JSON object（schema 对齐的最低门：能解码且为 map）
assert_ok_json(Resp) ->
    ?assertEqual(200, maps:get(status, Resp), maps:get(body, Resp)),
    case jsone:decode(maps:get(body, Resp)) of
        M when is_map(M) -> M;
        Other -> error({not_json_object, Other, maps:get(body, Resp)})
    end.

%% 列表页封闭信封：两个分页族并存——
%%   * 只读资源页（INT-24/26/28/30，read_page 族）：required
%%     [items, limit, has_more, next_cursor]；
%%   * 目录页（INT-16/17，directory 族）：required
%%     [items, page_size, has_more, next_cursor]。
%% 公共键 items/has_more/next_cursor 在此统一断言，页大小键按族二选一。
assert_page(Resp) ->
    Page = assert_ok_json(Resp),
    lists:foreach(fun(K) -> ?assert(maps:is_key(K, Page), {page_key_missing, K}) end, [
        <<"items">>, <<"has_more">>, <<"next_cursor">>
    ]),
    ?assert(
        maps:is_key(<<"limit">>, Page) orelse maps:is_key(<<"page_size">>, Page),
        {page_key_missing, limit_or_page_size}
    ),
    Page.

%% 冻结表 {name} 段 → 具体整数（negative 401 循环用；绑定在认证链前收敛）
concrete_path(Path) ->
    re:replace(Path, "\\{[a-z_]+\\}", "1", [global, {return, binary}]).

hash256(S) ->
    %% SHA-256 hex（64 字符小写）——valid_hash/1 接受形态
    Bin = crypto:hash(sha256, S),
    <<<<(hex_digit(H)), (hex_digit(L))>> || <<H:4, L:4>> <= Bin>>.

hex_digit(N) when N < 10 -> $0 + N;
hex_digit(N) -> $a + N - 10.

%% 冻结表 {name} 段 → cowboy :name 段（只做这一处语法归一）
cowboy_path(Path) ->
    re:replace(Path, "\\{([a-z_]+)\\}", ":\\1", [global, {return, list}]).

%% 错误信封：HTTP 状态与信封 code 均按 A2 冻结映射（enterprise_internal_error）。
assert_error_envelope(Response, Code) ->
    ?assert(is_binary(Code)),
    {Status, Body} = split_response(Response),
    ?assertEqual(enterprise_internal_error:http_status(Code), Status),
    assert_error_json(Body, Code).

split_response(Response) ->
    [Head, Body] = binary:split(Response, <<"\r\n\r\n">>),
    [StatusLine | _] = binary:split(Head, <<"\r\n">>),
    [_, StatusBin | _] = binary:split(StatusLine, <<" ">>, [global]),
    {binary_to_integer(StatusBin), Body}.

request(Port, Method, Path, Headers) ->
    {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, Port, [binary, {active, false}], 5000),
    HdrLines = [
        [K, <<": ">>, V, <<"\r\n">>]
     || {K, V} <- Headers
    ],
    ok = gen_tcp:send(Socket, [
        Method,
        <<" ">>,
        Path,
        <<" HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n">>,
        HdrLines,
        <<"\r\n">>
    ]),
    receive_all(Socket, []).

receive_all(Socket, Acc) ->
    case gen_tcp:recv(Socket, 0, 5000) of
        {ok, Data} ->
            receive_all(Socket, [Data | Acc]);
        {error, closed} ->
            iolist_to_binary(lists:reverse(Acc));
        {error, timeout} ->
            %% 服务端未按 Connection: close 关连接（如请求进程崩溃）时返回已收
            %% 字节，让断言给出真实差异而不是伪装成 recv 层失败。
            iolist_to_binary(lists:reverse(Acc));
        {error, Reason} ->
            error({recv_failed, Reason, iolist_to_binary(lists:reverse(Acc))})
    end.
