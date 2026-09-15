%%% @doc EB-09 企业**平台运营面** handler 套件（真 HTTP + 真 facade + 真 PG）。
%%%
%%% 依据：EB-09-A01/A02/A03/A04/A05。与租户面同构，差异在两处（都在此逐条实测）：
%%%   * principal 是 `platform_admin`（Admin session，`adm_user_id`）；
%%%   * **租户条件显式且强制**：每条路径都带 `:org_id`，`workspace_id` 也是必填
%%%     ——没有「不带 Org 的全局列举」，也没有「先查资源再比对归属」（A04）。
%%%
%%% 两个装配口径（同租户面）：
%%%   * `real`：`auth_facts => eb_platform_auth_facts`（生产装配，读既有 Admin ACL）。
%%%     Admin 用户的权限来自 **DB 里的角色 ACL**，本套件不造 admin 角色行，因此
%%%     「真装配 + 无 ACL」下的观测是 **403 permission_missing**（fail-closed）；
%%%   * `probe`：`auth_facts => eb09_platform_facts_probe`（把本动作声明的平台权限
%%%     作为**测试装配**注入，其余仍走真 facade + 真库），用于验证授权之后的
%%%     参数门与租户隔离。
-module(eb_platform_handler_tests).

-include_lib("eunit/include/eunit.hrl").

-define(S, eb_handler_test_support).
-define(FIX, eb_pg_test_fixture).
-define(TIMEOUT_S, 60).

platform_suite_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun cases/1}.

setup() ->
    case eunit_runner:eunit_setup_with_db() of
        {ok, Conn} -> {ok, Conn};
        {error, Reason} -> {error, Reason}
    end.

cleanup({ok, Conn}) ->
    eunit_runner:eunit_cleanup_db(Conn);
cleanup(Other) ->
    Other.

cases({ok, _Conn}) ->
    [
        {timeout, ?TIMEOUT_S, fun a03_401_without_admin_session_real/0},
        {timeout, ?TIMEOUT_S, fun a03_403_without_platform_permission_real/0},
        {timeout, ?TIMEOUT_S, fun a04_workspace_id_is_mandatory_probe/0},
        {timeout, ?TIMEOUT_S, fun a04_cross_org_isolation_probe/0},
        {timeout, ?TIMEOUT_S, fun a02_platform_read_returns_tsid_strings_probe/0},
        {timeout, ?TIMEOUT_S, fun a03_405_method_not_allowed_probe/0},
        {timeout, ?TIMEOUT_S, fun a04_write_requires_explicit_actor_probe/0},
        {timeout, ?TIMEOUT_S, fun a04_no_tenant_condition_would_be_caught_probe/0}
    ];
cases(Other) ->
    erlang:error({eb09_platform_suite_db_unavailable, Other}).

%% ===================================================================
%% A03：401 / 403（真装配，读既有 Admin ACL）
%% ===================================================================

a03_401_without_admin_session_real() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        %% 没有 adm_user_id（Admin cookie 未通过）⇒ credential_missing，零调用
        ?S:with_listener(platform, p_contacts, #{adm_user_id => 0}, fun(Port) ->
            Resp = ?S:request(
                Port,
                <<"GET">>,
                qs(?S:path(platform, p_contacts, #{org_id => Org}), [{<<"workspace_id">>, Ws}])
            ),
            ?assertEqual(401, maps:get(status, Resp)),
            ?assertEqual(<<"credential_missing">>, ?S:msg(Resp))
        end)
    after
        ok = eb09_platform_facts_probe:clear(),
        ?FIX:cleanup(Scope)
    end.

%% 真装配（`eb_platform_auth_facts` → `adm_acl:permissions/1`）：合成 adm_user_id
%% 在 DB 里没有角色 ⇒ 权限集为空 ⇒ **403 permission_missing**（fail-closed）。
%% 这同时是「平台权限不可省略」的正向证据：没有权限就没有任何数据通道。
a03_403_without_platform_permission_real() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        ?S:with_listener(platform, p_contacts, session(real, ?FIX:id()), fun(Port) ->
            Resp = ?S:request(
                Port,
                <<"GET">>,
                qs(?S:path(platform, p_contacts, #{org_id => Org}), [{<<"workspace_id">>, Ws}])
            ),
            ?assertEqual(403, maps:get(status, Resp)),
            ?assertNotEqual(nomatch, binary:match(?S:msg(Resp), <<"permission_missing">>))
        end)
    after
        ok = eb09_platform_facts_probe:clear(),
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% A04：平台面同样强制显式 Org / Workspace
%% ===================================================================

a04_workspace_id_is_mandatory_probe() ->
    Scope = ?FIX:new_scope(),
    ok = eb09_platform_facts_probe:grant([<<"enterprise_business:read">>]),
    try
        Org = maps:get(org_id, Scope),
        ?S:with_listener(platform, p_contacts, session(probe_read, 1), fun(Port) ->
            Resp = ?S:request(Port, <<"GET">>, ?S:path(platform, p_contacts, #{org_id => Org})),
            ?assertEqual(422, maps:get(status, Resp)),
            ?assertEqual(<<"missing_workspace_id">>, ?S:msg(Resp))
        end)
    after
        ok = eb09_platform_facts_probe:clear(),
        ?FIX:cleanup(Scope)
    end.

%% @doc 平台面的租户隔离：同一平台权限下，A Org 的读取只返回 A Org 的行；
%% 拿 B Org 的 `org_id` 去读 A Org 的会话消息，得到空页/404，而不是 A 的数据。
a04_cross_org_isolation_probe() ->
    Scope = ?FIX:new_scope(),
    ok = eb09_platform_facts_probe:grant([<<"enterprise_business:read">>]),
    try
        OrgA = maps:get(org_id, Scope),
        WsA = maps:get(workspace_id, Scope),
        OrgB = maps:get(other_org_id, Scope),
        WsB = maps:get(other_workspace_id, Scope),
        ConvA = maps:get(conversation_id, Scope),
        ?S:with_listener(platform, p_identities, session(probe_read, 1), fun(Port) ->
            InA = ?S:request(
                Port,
                <<"GET">>,
                qs(?S:path(platform, p_identities, #{org_id => OrgA}), [
                    {<<"workspace_id">>, WsA}
                ])
            ),
            ?assertEqual(200, maps:get(status, InA)),
            ?assert(length(?S:payload(InA)) >= 2),
            %% B Org 用**它自己的** Workspace 读：只见 B 自己的数据（空）
            InB = ?S:request(
                Port,
                <<"GET">>,
                qs(?S:path(platform, p_identities, #{org_id => OrgB}), [
                    {<<"workspace_id">>, WsB}
                ])
            ),
            ?assertEqual(200, maps:get(status, InB)),
            ?assertEqual([], ?S:payload(InB)),
            %% A 的 Org + B 的 Workspace 组合（伪造成对）：空，而不是 A 的数据
            Mismatch = ?S:request(
                Port,
                <<"GET">>,
                qs(?S:path(platform, p_identities, #{org_id => OrgA}), [
                    {<<"workspace_id">>, WsB}
                ])
            ),
            ?assertEqual(200, maps:get(status, Mismatch)),
            ?assertEqual([], ?S:payload(Mismatch)),
            %% 反向：B Org 路径下拿 A 的会话 id 读消息 ⇒ 404（**不**返回 A 的数据，
            %% 也不区分「不存在」与「跨租户」）
            CrossConv = ?S:request(
                Port,
                <<"GET">>,
                qs(?S:path(platform, p_conversation_messages, #{org_id => OrgB, id => ConvA}), [
                    {<<"workspace_id">>, WsB}
                ])
            ),
            ?assertEqual(404, maps:get(status, CrossConv)),
            ?assertEqual(nomatch, binary:match(?S:raw(CrossConv), integer_to_binary(ConvA)))
        end)
    after
        ok = eb09_platform_facts_probe:clear(),
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% A02：平台面读动作的 TSID 同样是 string（真链路）
%% ===================================================================

a02_platform_read_returns_tsid_strings_probe() ->
    Scope = ?FIX:new_scope(),
    ok = eb09_platform_facts_probe:grant([<<"enterprise_business:read">>]),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        ?S:with_listener(platform, p_contacts, session(probe_read, 1), fun(Port) ->
            Resp = ?S:request(
                Port,
                <<"GET">>,
                qs(?S:path(platform, p_contacts, #{org_id => Org}), [{<<"workspace_id">>, Ws}])
            ),
            ?assertEqual(200, maps:get(status, Resp)),
            Payload = ?S:payload(Resp),
            ?assert(is_list(Payload)),
            ?assertEqual(
                [],
                [Row || Row <- Payload, not is_binary(maps:get(<<"id">>, Row, undefined))]
            ),
            ?assertEqual(
                nomatch,
                re:run(?S:raw(Resp), <<"\"(id|organization_id)\":[0-9]">>, [
                    {capture, none}
                ])
            )
        end)
    after
        ok = eb09_platform_facts_probe:clear(),
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% A03 / A04：方法门与显式 actor
%% ===================================================================

a03_405_method_not_allowed_probe() ->
    Scope = ?FIX:new_scope(),
    ok = eb09_platform_facts_probe:grant([<<"enterprise_business:read">>]),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        ?S:with_listener(platform, p_contacts, session(probe_read, 1), fun(Port) ->
            Resp = ?S:request(
                Port,
                <<"POST">>,
                qs(?S:path(platform, p_contacts, #{org_id => Org}), [{<<"workspace_id">>, Ws}]),
                #{}
            ),
            ?assertEqual(405, maps:get(status, Resp)),
            ?assertEqual(<<"method_not_allowed">>, ?S:msg(Resp))
        end)
    after
        ok = eb09_platform_facts_probe:clear(),
        ?FIX:cleanup(Scope)
    end.

%% 纠错写动作必须显式给出「被代理的组织责任人」：缺 actor ⇒ 422（零调用）。
a04_write_requires_explicit_actor_probe() ->
    Scope = ?FIX:new_scope(),
    ok = eb09_platform_facts_probe:grant([<<"enterprise_business:write">>]),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        Uid = maps:get(peer_user_id, Scope),
        Before = member_status(Org, Uid),
        ?S:with_listener(platform, p_suspend_member, session(probe_write, 1), fun(Port) ->
            Resp = ?S:request(
                Port,
                <<"POST">>,
                qs(?S:path(platform, p_suspend_member, #{org_id => Org, uid => Uid}), [
                    {<<"workspace_id">>, Ws}
                ]),
                #{reason => <<"eb09-platform">>}
            ),
            ?assertEqual(422, maps:get(status, Resp)),
            ?assertEqual(<<"missing_param.actor_user_id">>, ?S:msg(Resp)),
            %% 零副作用：成员状态未变
            ?assertEqual(Before, member_status(Org, Uid))
        end)
    after
        ok = eb09_platform_facts_probe:clear(),
        ?FIX:cleanup(Scope)
    end.

%% @doc A04 的**非真空**证明：平台面的「租户条件」不是文本装饰——把 `org_id` 从
%% 参数/路径里拿掉（或换成常量 0）时，平台面要么路由不存在、要么被参数门拒绝，
%% 且**不可能**返回别的 Org 的数据。这里用真请求证明「拿掉 Org 就取不到数据」。
a04_no_tenant_condition_would_be_caught_probe() ->
    Scope = ?FIX:new_scope(),
    ok = eb09_platform_facts_probe:grant([<<"enterprise_business:read">>]),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        %% 平台面无 `:org_id` 的路径根本不存在（路由表里 0 条）
        Paths = [P || {P, _H, _O} <- ?S:enterprise_routes(platform)],
        ?assertNotEqual([], Paths),
        ?assertEqual([], [P || P <- Paths, binary:match(P, <<":org_id">>) =:= nomatch]),
        %% 用**不存在的 Org** 取值：显式 Org 条件下只得到空集（不越界、也不报 500）
        ?S:with_listener(platform, p_contacts, session(probe_read, 1), fun(Port) ->
            Resp = ?S:request(
                Port,
                <<"GET">>,
                qs(?S:path(platform, p_contacts, #{org_id => ?FIX:id()}), [
                    {<<"workspace_id">>, Ws}
                ])
            ),
            ?assertEqual(200, maps:get(status, Resp)),
            ?assertEqual([], ?S:payload(Resp)),
            %% 正控制：同一请求打到真 Org 时必须能看到该 Org 的客户行
            Ok = ?S:request(
                Port,
                <<"GET">>,
                qs(?S:path(platform, p_contacts, #{org_id => Org}), [{<<"workspace_id">>, Ws}])
            ),
            ?assertEqual(200, maps:get(status, Ok)),
            ?assert(length(?S:payload(Ok)) >= 1)
        end)
    after
        ok = eb09_platform_facts_probe:clear(),
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 内部辅助
%% ===================================================================

session(real, AdmUserId) ->
    %% 平台面的生产装配是 `eb_platform_auth_facts`（读既有 Admin ACL），
    %% **不是**租户面的 `eb_pg_auth_facts`。
    #{adm_user_id => AdmUserId, auth_facts => eb_platform_auth_facts};
session(probe_read, AdmUserId) ->
    #{adm_user_id => AdmUserId, auth_facts => eb09_platform_facts_probe};
session(probe_write, AdmUserId) ->
    #{adm_user_id => AdmUserId, auth_facts => eb09_platform_facts_probe}.

qs(Path, []) ->
    Path;
qs(Path, Params) ->
    Qs = lists:join(
        <<"&">>,
        [<<K/binary, "=", (to_bin(V))/binary>> || {K, V} <- Params]
    ),
    <<Path/binary, "?", (iolist_to_binary(Qs))/binary>>.

to_bin(V) when is_binary(V) -> V;
to_bin(V) when is_integer(V) -> integer_to_binary(V).

member_status(Org, UserId) ->
    ?S:scalar(
        undefined,
        <<"SELECT status FROM organization_member WHERE organization_id=$1 AND user_id=$2">>,
        [Org, UserId]
    ).
