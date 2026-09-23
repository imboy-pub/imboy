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
    %% 31 条 INT（GZ 14 + FULL-02 新增 8 + FULL-03 新增 1 + V2.1 新增 8）。
    %% Router（共享路径，A0 接线）当前登记的是已实现 handler 的 23 条——
    %% INT-24..31 的 handler（enterprise_workspace/project/channel_handler、
    %% enterprise_group_handler 扩展）由 A2 实现后经 A0 接线进 imboy_router，
    %% 故此处只断言「已接线面」的登记与形态；注册表全量形态另行断言（下方
    %% manifest_v21_entries）。
    Manifest = enterprise_internal_routes:routes(),
    ?assertEqual(31, length(Manifest)),
    %% Router（共享路径，A0 已于集成接线）：INT-24..31 的 8 条全部进
    %% imboy_router；其中 INT-26/27 复用既有 cowboy path（GET 方法分派在
    %% handler 内），故 31 端点对应 25 条唯一 path（19 既有 + 6 新增）。
    Wired = Manifest,
    ?assertEqual(31, length(Wired)),
    %% 冻结表用 {name} 占位符语法，cowboy 路由用 :name —— 归一后逐条比对。
    WiredPaths = lists:usort([
        cowboy_path(binary_to_list(maps:get(path, R)))
     || R <- Wired
    ]),
    ?assertEqual(25, length(WiredPaths)),
    lists:foreach(
        fun(P) ->
            ?assert(lists:member(P, Paths))
        end,
        WiredPaths
    ),
    ?assertEqual(25, length([P || P <- Paths, lists:prefix("/api/internal/v1/", P)])),

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
    ?assertEqual(31, length(lists:usort(Ids))),
    MethodPaths = [{maps:get(method, R), maps:get(path, R)} || R <- Manifest],
    ?assertEqual(31, length(lists:usort(MethodPaths))),
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
        {internal_error, <<"internal_error">>}
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

%% 冻结表 {name} 段 → cowboy :name 段（只做这一处语法归一）
cowboy_path(Path) ->
    re:replace(Path, "\\{([a-z_]+)\\}", ":\\1", [global, {return, list}]).

%% 错误信封：HTTP 状态与信封 code 均按 A2 冻结映射（enterprise_internal_error）。
assert_error_envelope(Response, Code) ->
    ?assert(is_binary(Code)),
    {Status, Body} = split_response(Response),
    ?assertEqual(enterprise_internal_error:http_status(Code), Status),
    case binary:match(Body, Code) of
        nomatch -> error({envelope_missing_code, Code, Status, Body});
        _ -> ok
    end,
    %% 信封不得回显 credential / Authorization 值（redaction 红线）
    ?assertEqual(nomatch, binary:match(Body, <<"deadbeef">>)),
    ?assertNotEqual(nomatch, binary:match(Body, <<"\"code\"">>)).

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
