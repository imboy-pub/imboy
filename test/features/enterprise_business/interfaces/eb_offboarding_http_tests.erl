%%% @doc 离职交接**读取面**（offboarding read）的真 HTTP 套件（真 cowboy + 真 facade + 真 PG）。
%%%
%%% 依据：closure 计划 §8（offboarding 需「查询交接 case」）、EB-09-A01/A02/A03/A04
%%% 的同口径实测、EB-08-A04（重试语义）的 HTTP 面合同化。
%%%
%%% 覆盖（全部走 `eb_handler_test_support` 的临时端口真路由）：
%%%   * **正例**：owner（治理角色）列自己 Org 的 case（TSID string、白名单投影、
%%%     创建时间倒序、after_id/limit 键集分页、status 过滤）；详情含 items 子表
%%%     （status/failure_reason/attempt/idempotency_key），`items_status=failed`
%%%     只回失败项；platform_admin 读**显式指定** Org 的 list/detail。
%%%   * **负例**：未登录 401、治理不足 403、跨 Org 404（不区分不存在与跨租户）、
%%%     坏 TSID 400、limit 越界 422（message 先例：缺省 50、上限 200）、
%%%     未知 status/items_status 422、缺 workspace_id 422、方法门 405。
%%%   * **重试合同（EB-08-A04 的 HTTP 合同化）**：对 `failed` case 重新 POST execute
%%%     ——只重试非 success 项、幂等键逐字不变、每次 execute 恰好一条审计。
%%%
%%% 投影红线（§8）：读面响应里**不得**出现 cipher / key_version / plaintext 类键；
%%% 本套件对 list / detail / items 的响应键集做**精确相等**断言（多一个键也红）。
%%% 只使用 `eb_pg_test_fixture` 的合成租户（随机 TSID、无真实账号/联系方式/生产资源）。
-module(eb_offboarding_http_tests).

-include_lib("eunit/include/eunit.hrl").

-define(S, eb_handler_test_support).
-define(FIX, eb_pg_test_fixture).
-define(APP, eb_offboarding_app).
-define(TIMEOUT_S, 120).

%% 投影白名单（与 `eb_offboarding_app` 的投影函数逐字同口径；ORDER.md 同表）。
-define(LIST_KEYS, [
    <<"id">>,
    <<"status">>,
    <<"leaver_user_id">>,
    <<"successor_user_id">>,
    <<"version">>,
    <<"item_total">>,
    <<"item_success">>,
    <<"item_failed">>,
    <<"created_at">>,
    <<"updated_at">>,
    <<"completed_at">>
]).
-define(CASE_KEYS, [
    <<"id">>,
    <<"organization_id">>,
    <<"leaver_user_id">>,
    <<"successor_user_id">>,
    <<"status">>,
    <<"version">>,
    <<"item_total">>,
    <<"item_success">>,
    <<"item_failed">>,
    <<"created_by_user_id">>,
    <<"reason">>,
    <<"created_at">>,
    <<"updated_at">>,
    <<"completed_at">>,
    <<"items">>
]).
-define(ITEM_KEYS, [
    <<"id">>,
    <<"case_id">>,
    <<"business_identity_id">>,
    <<"function_key">>,
    <<"from_user_id">>,
    <<"to_user_id">>,
    <<"status">>,
    <<"idempotency_key">>,
    <<"attempt">>,
    <<"failure_reason">>,
    <<"created_at">>
]).

offboarding_http_test_() ->
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
        {timeout, ?TIMEOUT_S, fun t_401_without_session_real/0},
        {timeout, ?TIMEOUT_S, fun t_403_governance_insufficient_real/0},
        {timeout, ?TIMEOUT_S, fun t_list_projection_and_tsid_strings_probe/0},
        {timeout, ?TIMEOUT_S, fun t_list_desc_order_keyset_and_status_filter_probe/0},
        {timeout, ?TIMEOUT_S, fun t_detail_items_and_failed_filter_probe/0},
        {timeout, ?TIMEOUT_S, fun t_cross_org_detail_is_404_probe/0},
        {timeout, ?TIMEOUT_S, fun t_bad_tsid_and_method_gate_probe/0},
        {timeout, ?TIMEOUT_S, fun t_limit_bounds_probe/0},
        {timeout, ?TIMEOUT_S, fun t_unknown_status_422_probe/0},
        {timeout, ?TIMEOUT_S, fun t_retry_semantics_via_http_contract/0},
        {timeout, ?TIMEOUT_S, fun p_401_without_admin_session_real/0},
        {timeout, ?TIMEOUT_S, fun p_403_without_platform_permission_real/0},
        {timeout, ?TIMEOUT_S, fun p_read_designated_org_with_isolation_probe/0},
        {timeout, ?TIMEOUT_S, fun p_workspace_id_mandatory_probe/0}
    ];
cases(Other) ->
    erlang:error({eb_offboarding_http_suite_db_unavailable, Other}).

%% ===================================================================
%% 租户面：凭证 / 治理门（真装配）
%% ===================================================================

t_401_without_session_real() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        ?S:with_listener(tenant, offboarding_list, #{current_uid => 0}, fun(Port) ->
            Resp = ?S:request(
                Port,
                <<"GET">>,
                qs(path(tenant, offboarding_list, Org), [{<<"workspace_id">>, Ws}])
            ),
            ?assertEqual(401, maps:get(status, Resp)),
            ?assertEqual(<<"credential_missing">>, ?S:msg(Resp))
        end)
    after
        scrub(Scope)
    end.

%% 普通成员（非 owner/admin）读 offboarding ⇒ 403 governance_insufficient（真装配：
%% 治理角色读库，不靠测试补权限——读面与写面同门，见 route metadata 的 governance_auth）。
t_403_governance_insufficient_real() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        Actor = maps:get(actor_user_id, Scope),
        ?S:with_listener(tenant, offboarding_list, session_real(Actor), fun(Port) ->
            Resp = ?S:request(
                Port,
                <<"GET">>,
                qs(path(tenant, offboarding_list, Org), [{<<"workspace_id">>, Ws}])
            ),
            ?assertEqual(403, maps:get(status, Resp)),
            ?assertNotEqual(nomatch, binary:match(?S:msg(Resp), <<"governance_insufficient">>))
        end)
    after
        scrub(Scope)
    end.

%% ===================================================================
%% 租户面：列表投影 / 排序 / 分页 / status 过滤
%% ===================================================================

t_list_projection_and_tsid_strings_probe() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        Owner = maps:get(owner_user_id, Scope),
        Actor = maps:get(actor_user_id, Scope),
        Successor = maps:get(peer_user_id, Scope),
        ok = insert_member(Org, Successor, <<"member">>),
        {ok, _} = open_case(Scope, Actor, Successor),
        ?S:with_listener(tenant, offboarding_list, session_real(Owner), fun(Port) ->
            Resp = ?S:request(
                Port,
                <<"GET">>,
                qs(path(tenant, offboarding_list, Org), [{<<"workspace_id">>, Ws}])
            ),
            ?assertEqual(200, maps:get(status, Resp)),
            Payload = ?S:payload(Resp),
            ?assertEqual(1, length(Payload)),
            [Row] = Payload,
            %% 投影**精确**键集：多一个键（如 reason / organization_id）也红
            ?assertEqual(lists:sort(?LIST_KEYS), lists:sort(maps:keys(Row))),
            %% TSID 一律 string（A02）；计数/版本/时间戳仍是 number
            ?assert(is_binary(maps:get(<<"id">>, Row))),
            ?assert(is_binary(maps:get(<<"leaver_user_id">>, Row))),
            ?assert(is_binary(maps:get(<<"successor_user_id">>, Row))),
            ?assert(is_integer(maps:get(<<"item_total">>, Row))),
            ?assert(is_integer(maps:get(<<"version">>, Row))),
            ?assertEqual(<<"frozen">>, maps:get(<<"status">>, Row)),
            %% §8 投影红线：无任何 cipher / key / plaintext 类键
            ?assertEqual([], leaky_keys(?LIST_KEYS))
        end)
    after
        scrub(Scope)
    end.

t_list_desc_order_keyset_and_status_filter_probe() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        Owner = maps:get(owner_user_id, Scope),
        Actor = maps:get(actor_user_id, Scope),
        Successor = maps:get(peer_user_id, Scope),
        ok = insert_member(Org, Successor, <<"member">>),
        %% 三个不同 leaver 的 case（同 leaver 同时只能有一个未完成 case，DB 唯一索引）
        {ok, C1} = open_case(Scope, Actor, Successor),
        L2 = fresh_leaver(Scope),
        {ok, C2} = open_case(Scope, L2, Successor),
        L3 = fresh_leaver(Scope),
        {ok, C3} = open_case(Scope, L3, Successor),
        Id1 = maps:get(case_id, C1),
        Id2 = maps:get(case_id, C2),
        Id3 = maps:get(case_id, C3),
        ?S:with_listener(tenant, offboarding_list, session_real(Owner), fun(Port) ->
            Path = qs(path(tenant, offboarding_list, Org), [{<<"workspace_id">>, Ws}]),
            All = ?S:request(Port, <<"GET">>, Path),
            ?assertEqual(200, maps:get(status, All)),
            Rows = ?S:payload(All),
            %% 创建时间倒序（id 倒序；TSID 时间有序）
            ?assertEqual([Id3, Id2, Id1], [binary_to_integer(maps:get(<<"id">>, R)) || R <- Rows]),

            %% 键集分页：after_id=Id3 ⇒ 严格 id < Id3 ⇒ [Id2, Id1]；limit 截断
            Page2 = ?S:request(
                Port,
                <<"GET">>,
                qs(path(tenant, offboarding_list, Org), [
                    {<<"workspace_id">>, Ws},
                    {<<"after_id">>, integer_to_binary(Id3)}
                ])
            ),
            ?assertEqual([Id2, Id1], [
                binary_to_integer(maps:get(<<"id">>, R))
             || R <- ?S:payload(Page2)
            ]),
            PageLimit = ?S:request(
                Port,
                <<"GET">>,
                qs(path(tenant, offboarding_list, Org), [
                    {<<"workspace_id">>, Ws},
                    {<<"after_id">>, integer_to_binary(Id3)},
                    {<<"limit">>, <<"1">>}
                ])
            ),
            ?assertEqual([Id2], [
                binary_to_integer(maps:get(<<"id">>, R))
             || R <- ?S:payload(PageLimit)
            ]),

            %% status 过滤：全部是 frozen ⇒ status=failed 空页
            FrozenOnly = ?S:request(
                Port,
                <<"GET">>,
                qs(path(tenant, offboarding_list, Org), [
                    {<<"workspace_id">>, Ws},
                    {<<"status">>, <<"frozen">>}
                ])
            ),
            ?assertEqual(3, length(?S:payload(FrozenOnly))),
            FailedOnly = ?S:request(
                Port,
                <<"GET">>,
                qs(path(tenant, offboarding_list, Org), [
                    {<<"workspace_id">>, Ws},
                    {<<"status">>, <<"failed">>}
                ])
            ),
            ?assertEqual([], ?S:payload(FailedOnly))
        end)
    after
        scrub(Scope)
    end.

%% ===================================================================
%% 租户面：详情 + items + 失败项过滤
%% ===================================================================

t_detail_items_and_failed_filter_probe() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        Owner = maps:get(owner_user_id, Scope),
        Actor = maps:get(actor_user_id, Scope),
        Successor = maps:get(peer_user_id, Scope),
        Third = maps:get(owner_user_id, Scope),
        Identity = maps:get(sales_identity_id, Scope),
        ok = insert_member(Org, Successor, <<"member">>),
        {ok, Opened} = open_case(Scope, Actor, Successor),
        CaseId = maps:get(case_id, Opened),
        %% 注入失败：把 active 经办人换成第三人 ⇒ execute 后该项 failed（EB-08-A04）
        ok = rebind_directly(Scope, Identity, Actor, Third),
        {ok, _} =
            ?APP:execute_offboarding(Org, #{
                workspace_id => Ws,
                case_id => CaseId,
                expected_version => maps:get(version, Opened),
                actor_user_id => Third
            }),
        ?S:with_listener(tenant, offboarding_detail, session_real(Owner), fun(Port) ->
            Path = qs(path(tenant, offboarding_detail, Org, CaseId), [{<<"workspace_id">>, Ws}]),
            Resp = ?S:request(Port, <<"GET">>, Path),
            ?assertEqual(200, maps:get(status, Resp)),
            Case = ?S:payload(Resp),
            ?assertEqual(lists:sort(?CASE_KEYS), lists:sort(maps:keys(Case))),
            ?assertEqual(<<"failed">>, maps:get(<<"status">>, Case)),
            ?assertEqual(1, maps:get(<<"item_failed">>, Case)),
            ?assertEqual(1, length(maps:get(<<"items">>, Case))),
            [Item] = maps:get(<<"items">>, Case),
            ?assertEqual(lists:sort(?ITEM_KEYS), lists:sort(maps:keys(Item))),
            ?assertEqual(<<"failed">>, maps:get(<<"status">>, Item)),
            ?assert(is_binary(maps:get(<<"failure_reason">>, Item))),
            ?assert(is_binary(maps:get(<<"idempotency_key">>, Item))),
            %% 幂等键 = (case_id, identity_id) 的服务端重算载体：在详情可见且非空
            ?assert(byte_size(maps:get(<<"idempotency_key">>, Item)) > 0),

            %% items_status=failed 只回失败项；items_status=success 空表
            FailedOnly = ?S:request(
                Port,
                <<"GET">>,
                qs(path(tenant, offboarding_detail, Org, CaseId), [
                    {<<"workspace_id">>, Ws},
                    {<<"items_status">>, <<"failed">>}
                ])
            ),
            ?assertEqual(1, length(maps:get(<<"items">>, ?S:payload(FailedOnly)))),
            SuccessOnly = ?S:request(
                Port,
                <<"GET">>,
                qs(path(tenant, offboarding_detail, Org, CaseId), [
                    {<<"workspace_id">>, Ws},
                    {<<"items_status">>, <<"success">>}
                ])
            ),
            ?assertEqual([], maps:get(<<"items">>, ?S:payload(SuccessOnly)))
        end)
    after
        scrub(Scope)
    end.

t_cross_org_detail_is_404_probe() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        OtherOrg = maps:get(other_org_id, Scope),
        OtherWs = maps:get(other_workspace_id, Scope),
        Owner = maps:get(owner_user_id, Scope),
        Actor = maps:get(actor_user_id, Scope),
        Successor = maps:get(peer_user_id, Scope),
        ok = insert_member(Org, Successor, <<"member">>),
        {ok, Opened} = open_case(Scope, Actor, Successor),
        CaseId = maps:get(case_id, Opened),
        ?S:with_listener(tenant, offboarding_detail, session_real(Owner), fun(Port) ->
            %% 用其它 Org 的路径与它自己的 Workspace 读 ⇒ 404（不区分不存在与跨租户）
            Cross = ?S:request(
                Port,
                <<"GET">>,
                qs(path(tenant, offboarding_detail, OtherOrg, CaseId), [
                    {<<"workspace_id">>, OtherWs}
                ])
            ),
            ?assertEqual(404, maps:get(status, Cross)),
            ?assertEqual(<<"case_not_found">>, ?S:msg(Cross)),
            %% 正控制：本 Org 能读到
            Ok = ?S:request(
                Port,
                <<"GET">>,
                qs(path(tenant, offboarding_detail, Org, CaseId), [{<<"workspace_id">>, Ws}])
            ),
            ?assertEqual(200, maps:get(status, Ok))
        end),
        %% 列表同样不越租户：其它 Org 列表恒空（单独的 list 路由监听器）
        ?S:with_listener(tenant, offboarding_list, session_real(Owner), fun(Port) ->
            CrossList = ?S:request(
                Port,
                <<"GET">>,
                qs(path(tenant, offboarding_list, OtherOrg), [{<<"workspace_id">>, OtherWs}])
            ),
            ?assertEqual(200, maps:get(status, CrossList)),
            ?assertEqual([], ?S:payload(CrossList))
        end)
    after
        scrub(Scope)
    end.

%% 坏 TSID 是 400 invalid_tsid（既有先例：path 形状错 ⇒ 400）；对列表路由 POST ⇒ 405。
t_bad_tsid_and_method_gate_probe() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        Owner = maps:get(owner_user_id, Scope),
        ?S:with_listener(tenant, offboarding_detail, session_real(Owner), fun(Port) ->
            Bad = ?S:request(
                Port,
                <<"GET">>,
                qs(path(tenant, offboarding_detail, Org, <<"not-a-tsid">>), [
                    {<<"workspace_id">>, Ws}
                ])
            ),
            ?assertEqual(400, maps:get(status, Bad)),
            ?assertEqual(<<"invalid_tsid">>, ?S:msg(Bad))
        end),
        ?S:with_listener(tenant, offboarding_list, session_real(Owner), fun(Port) ->
            WrongMethod = ?S:request(
                Port,
                <<"POST">>,
                qs(path(tenant, offboarding_list, Org), [{<<"workspace_id">>, Ws}]),
                #{}
            ),
            ?assertEqual(405, maps:get(status, WrongMethod)),
            ?assertEqual(<<"method_not_allowed">>, ?S:msg(WrongMethod))
        end)
    after
        scrub(Scope)
    end.

%% limit 越界按 message 先例：422 invalid_limit（缺省 50、1..200；不静默钳制）。
t_limit_bounds_probe() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        Owner = maps:get(owner_user_id, Scope),
        ?S:with_listener(tenant, offboarding_list, session_real(Owner), fun(Port) ->
            Zero = ?S:request(
                Port,
                <<"GET">>,
                qs(path(tenant, offboarding_list, Org), [
                    {<<"workspace_id">>, Ws},
                    {<<"limit">>, <<"0">>}
                ])
            ),
            ?assertEqual(422, maps:get(status, Zero)),
            ?assertEqual(<<"invalid_limit">>, ?S:msg(Zero)),
            Over = ?S:request(
                Port,
                <<"GET">>,
                qs(path(tenant, offboarding_list, Org), [
                    {<<"workspace_id">>, Ws},
                    {<<"limit">>, <<"201">>}
                ])
            ),
            ?assertEqual(422, maps:get(status, Over)),
            ?assertEqual(<<"invalid_limit">>, ?S:msg(Over)),
            %% 边界值合法：200 一页正常返回（空列表也是 200）
            Edge = ?S:request(
                Port,
                <<"GET">>,
                qs(path(tenant, offboarding_list, Org), [
                    {<<"workspace_id">>, Ws},
                    {<<"limit">>, <<"200">>}
                ])
            ),
            ?assertEqual(200, maps:get(status, Edge))
        end)
    after
        scrub(Scope)
    end.

t_unknown_status_422_probe() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        Owner = maps:get(owner_user_id, Scope),
        ?S:with_listener(tenant, offboarding_list, session_real(Owner), fun(Port) ->
            Resp = ?S:request(
                Port,
                <<"GET">>,
                qs(path(tenant, offboarding_list, Org), [
                    {<<"workspace_id">>, Ws},
                    {<<"status">>, <<"bogus">>}
                ])
            ),
            ?assertEqual(422, maps:get(status, Resp)),
            ?assertEqual(<<"invalid_status">>, ?S:msg(Resp))
        end),
        Actor = maps:get(actor_user_id, Scope),
        Successor = maps:get(peer_user_id, Scope),
        ok = insert_member(Org, Successor, <<"member">>),
        {ok, Opened} = open_case(Scope, Actor, Successor),
        CaseId = maps:get(case_id, Opened),
        ?S:with_listener(tenant, offboarding_detail, session_real(Owner), fun(Port) ->
            Resp = ?S:request(
                Port,
                <<"GET">>,
                qs(path(tenant, offboarding_detail, Org, CaseId), [
                    {<<"workspace_id">>, Ws},
                    {<<"items_status">>, <<"bogus">>}
                ])
            ),
            ?assertEqual(422, maps:get(status, Resp)),
            ?assertEqual(<<"invalid_items_status">>, ?S:msg(Resp))
        end)
    after
        scrub(Scope)
    end.

%% ===================================================================
%% 重试合同（EB-08-A04 的 HTTP 合同化）：对 failed case 重新 POST execute
%% ——只重试非 success 项、幂等键逐字不变、每次 execute 恰好一条审计。
%% ===================================================================

t_retry_semantics_via_http_contract() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        Owner = maps:get(owner_user_id, Scope),
        Actor = maps:get(actor_user_id, Scope),
        Successor = maps:get(peer_user_id, Scope),
        Third = maps:get(owner_user_id, Scope),
        Identity = maps:get(sales_identity_id, Scope),
        ok = insert_member(Org, Successor, <<"member">>),
        {ok, Opened} = open_case(Scope, Actor, Successor),
        CaseId = maps:get(case_id, Opened),
        ok = rebind_directly(Scope, Identity, Actor, Third),
        %% ① 第一次 execute（HTTP）：该项失败，case 落 failed；拿到 failed 后的 version
        {200, FailedVersion} = ?S:with_listener(
            tenant, offboarding_execute, session_real(Owner), fun(Port) ->
                First = ?S:request(
                    Port,
                    <<"POST">>,
                    qs(path(tenant, offboarding_execute, Org, CaseId), [
                        {<<"workspace_id">>, Ws}
                    ]),
                    #{expected_version => maps:get(version, Opened)}
                ),
                ?assertEqual(200, maps:get(status, First)),
                Payload = ?S:payload(First),
                ?assertEqual(<<"failed">>, maps:get(<<"status">>, Payload)),
                {200, maps:get(<<"version">>, Payload)}
            end
        ),
        %% ② 读失败项：幂等键先取快照
        KeyBefore = ?S:with_listener(tenant, offboarding_detail, session_real(Owner), fun(Port) ->
            Detail = ?S:request(
                Port,
                <<"GET">>,
                qs(path(tenant, offboarding_detail, Org, CaseId), [
                    {<<"workspace_id">>, Ws},
                    {<<"items_status">>, <<"failed">>}
                ])
            ),
            ?assertEqual(200, maps:get(status, Detail)),
            [FailedItem] = maps:get(<<"items">>, ?S:payload(Detail)),
            maps:get(<<"idempotency_key">>, FailedItem)
        end),
        %% ③ 排除障碍后重试：先结束第三人的经办，再 POST execute（同一幂等键路径）
        ok = end_active_assignment(Scope, Identity),
        ?S:with_listener(tenant, offboarding_execute, session_real(Owner), fun(Port) ->
            Retry = ?S:request(
                Port,
                <<"POST">>,
                qs(path(tenant, offboarding_execute, Org, CaseId), [{<<"workspace_id">>, Ws}]),
                #{expected_version => FailedVersion}
            ),
            ?assertEqual(200, maps:get(status, Retry)),
            Retried = ?S:payload(Retry),
            ?assertEqual(<<"transferring">>, maps:get(<<"status">>, Retried)),
            ?assertEqual(1, maps:get(<<"item_success">>, Retried)),
            ?assertEqual(0, maps:get(<<"item_failed">>, Retried))
        end),
        %% ④ 合同断言：幂等键逐字不变（重试不产生第二条事实）；两次 execute 各审计一次
        ?S:with_listener(tenant, offboarding_detail, session_real(Owner), fun(Port) ->
            Detail2 = ?S:request(
                Port,
                <<"GET">>,
                qs(path(tenant, offboarding_detail, Org, CaseId), [{<<"workspace_id">>, Ws}])
            ),
            ?assertEqual(200, maps:get(status, Detail2)),
            [DoneItem] = maps:get(<<"items">>, ?S:payload(Detail2)),
            ?assertEqual(KeyBefore, maps:get(<<"idempotency_key">>, DoneItem)),
            ?assertEqual(<<"success">>, maps:get(<<"status">>, DoneItem)),
            ?assertEqual(2, audit_count(Org, <<"offboarding.execute">>))
        end)
    after
        scrub(Scope)
    end.

%% ===================================================================
%% 平台面（platform_admin 凭据；Org/Workspace 显式强制）
%% ===================================================================

p_401_without_admin_session_real() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        ?S:with_listener(platform, p_offboarding_list, #{adm_user_id => 0}, fun(Port) ->
            Resp = ?S:request(
                Port,
                <<"GET">>,
                qs(path(platform, p_offboarding_list, Org), [{<<"workspace_id">>, Ws}])
            ),
            ?assertEqual(401, maps:get(status, Resp)),
            ?assertEqual(<<"credential_missing">>, ?S:msg(Resp))
        end)
    after
        ok = eb09_platform_facts_probe:clear(),
        scrub(Scope)
    end.

p_403_without_platform_permission_real() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        %% 真装配（eb_platform_auth_facts → Admin ACL）：合成 adm 在 DB 里无角色 ⇒ 403
        ?S:with_listener(platform, p_offboarding_list, session_admin(real), fun(Port) ->
            Resp = ?S:request(
                Port,
                <<"GET">>,
                qs(path(platform, p_offboarding_list, Org), [{<<"workspace_id">>, Ws}])
            ),
            ?assertEqual(403, maps:get(status, Resp)),
            ?assertNotEqual(nomatch, binary:match(?S:msg(Resp), <<"permission_missing">>))
        end)
    after
        ok = eb09_platform_facts_probe:clear(),
        scrub(Scope)
    end.

p_read_designated_org_with_isolation_probe() ->
    Scope = ?FIX:new_scope(),
    ok = eb09_platform_facts_probe:grant([<<"enterprise_business:read">>]),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        OtherOrg = maps:get(other_org_id, Scope),
        OtherWs = maps:get(other_workspace_id, Scope),
        Actor = maps:get(actor_user_id, Scope),
        Successor = maps:get(peer_user_id, Scope),
        ok = insert_member(Org, Successor, <<"member">>),
        {ok, Opened} = open_case(Scope, Actor, Successor),
        CaseId = maps:get(case_id, Opened),
        ?S:with_listener(platform, p_offboarding_list, session_admin(probe), fun(Port) ->
            %% 读显式指定的 Org：200 + 与租户面同口径投影
            Ok = ?S:request(
                Port,
                <<"GET">>,
                qs(path(platform, p_offboarding_list, Org), [{<<"workspace_id">>, Ws}])
            ),
            ?assertEqual(200, maps:get(status, Ok)),
            Rows = ?S:payload(Ok),
            ?assertEqual(1, length(Rows)),
            ?assertEqual(lists:sort(?LIST_KEYS), lists:sort(maps:keys(hd(Rows)))),
            %% 隔离：换 org_id 只得到空页，绝不返回 A 的数据
            CrossList = ?S:request(
                Port,
                <<"GET">>,
                qs(path(platform, p_offboarding_list, OtherOrg), [
                    {<<"workspace_id">>, OtherWs}
                ])
            ),
            ?assertEqual(200, maps:get(status, CrossList)),
            ?assertEqual([], ?S:payload(CrossList))
        end),
        ?S:with_listener(platform, p_offboarding_detail, session_admin(probe), fun(Port) ->
            Detail = ?S:request(
                Port,
                <<"GET">>,
                qs(path(platform, p_offboarding_detail, Org, CaseId), [
                    {<<"workspace_id">>, Ws}
                ])
            ),
            ?assertEqual(200, maps:get(status, Detail)),
            ?assertEqual(lists:sort(?CASE_KEYS), lists:sort(maps:keys(?S:payload(Detail)))),
            CrossDetail = ?S:request(
                Port,
                <<"GET">>,
                qs(path(platform, p_offboarding_detail, OtherOrg, CaseId), [
                    {<<"workspace_id">>, OtherWs}
                ])
            ),
            ?assertEqual(404, maps:get(status, CrossDetail)),
            ?assertEqual(
                nomatch, binary:match(?S:raw(CrossDetail), integer_to_binary(CaseId))
            )
        end)
    after
        ok = eb09_platform_facts_probe:clear(),
        scrub(Scope)
    end.

p_workspace_id_mandatory_probe() ->
    Scope = ?FIX:new_scope(),
    ok = eb09_platform_facts_probe:grant([<<"enterprise_business:read">>]),
    try
        Org = maps:get(org_id, Scope),
        ?S:with_listener(platform, p_offboarding_list, session_admin(probe), fun(Port) ->
            Resp = ?S:request(Port, <<"GET">>, path(platform, p_offboarding_list, Org)),
            ?assertEqual(422, maps:get(status, Resp)),
            ?assertEqual(<<"missing_workspace_id">>, ?S:msg(Resp))
        end)
    after
        ok = eb09_platform_facts_probe:clear(),
        scrub(Scope)
    end.

%% ===================================================================
%% 内部辅助（无业务逻辑：只有造数 / 会话 / 路径 / 清场）
%% ===================================================================

session_real(Uid) ->
    #{current_uid => Uid, auth_facts => eb_pg_auth_facts}.

session_admin(real) ->
    #{adm_user_id => ?FIX:id(), auth_facts => eb_platform_auth_facts};
session_admin(probe) ->
    #{adm_user_id => 1, auth_facts => eb09_platform_facts_probe}.

path(Surface, Action, Org) ->
    ?S:path(Surface, Action, #{org_id => Org}).

path(Surface, Action, Org, CaseId) ->
    ?S:path(Surface, Action, #{org_id => Org, id => CaseId}).

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

leaky_keys(Keys) ->
    Forbidden = [<<"cipher">>, <<"key_version">>, <<"plaintext">>],
    [
        K
     || K <- Keys,
        lists:any(
            fun(F) -> nomatch =/= binary:match(K, F) end,
            Forbidden
        )
    ].

open_case(Scope, Leaver, Successor) ->
    {Org, Ws} = tenant(Scope),
    ?APP:open_offboarding(Org, #{
        workspace_id => Ws,
        leaver_user_id => Leaver,
        successor_user_id => Successor,
        reason => <<"eb-off-reading-synthetic">>,
        actor_user_id => maps:get(owner_user_id, Scope)
    }).

tenant(Scope) ->
    {maps:get(org_id, Scope), maps:get(workspace_id, Scope)}.

%% 追加一个 active 成员作为新的 leaver（随机 TSID，绝不复用真实账号）。
fresh_leaver(Scope) ->
    Org = maps:get(org_id, Scope),
    Uid = ?FIX:id(),
    ?FIX:exec(
        <<"INSERT INTO \"user\"(id,password,account,reg_ip,reg_cosv) VALUES ($1,'x',$2,'127.0.0.1','x')">>,
        [Uid, account(Uid)]
    ),
    ok = insert_member(Org, Uid, <<"member">>),
    Uid.

account(Uid) ->
    <<"eb-off-reading-", (integer_to_binary(Uid))/binary>>.

insert_member(Org, Uid, Role) ->
    ?FIX:exec(
        <<
            "INSERT INTO organization_member(organization_id,user_id,role,status)"
            " VALUES ($1,$2,$3,'active')"
            " ON CONFLICT (organization_id,user_id) DO UPDATE SET role=EXCLUDED.role,"
            " status='active'"
        >>,
        [Org, Uid, Role]
    ).

%% 直接经 store 把某 identity 的 active 经办交给 To（合成「脏前置」：交接对象不是
%% leaver 也不是 successor ⇒ execute 判该项失败，而不是静默改交给别人）。
rebind_directly(Scope, Identity, From, To) ->
    {Org, Ws} = tenant(Scope),
    ok = end_active_assignment(Scope, Identity),
    {ok, _} = eb_pg_store:insert_assignment(Org, Ws, #{
        id => ?FIX:id(),
        business_identity_id => Identity,
        function_key => <<"sales">>,
        user_id => To,
        assigned_by => From
    }),
    ok.

end_active_assignment(Scope, Identity) ->
    {Org, Ws} = tenant(Scope),
    eb_pg_store:advance_assignment(Org, Ws, Identity, active, ended).

audit_count(Org, Action) ->
    ?FIX:scalar(
        <<
            "SELECT count(*) FROM enterprise_audit_event"
            " WHERE organization_id=$1 AND action=$2"
        >>,
        [Org, Action],
        -1
    ).

%% 清场：先删本卡合成行（item 先于 case），再交夹具，最后删本套件补建的成员行。
scrub(Scope) ->
    Org = maps:get(org_id, Scope, undefined),
    case is_integer(Org) of
        true ->
            _ = ?FIX:exec(<<"DELETE FROM enterprise_offboarding_item WHERE organization_id=$1">>, [
                Org
            ]),
            _ = ?FIX:exec(<<"DELETE FROM enterprise_offboarding_case WHERE organization_id=$1">>, [
                Org
            ]);
        false ->
            ok
    end,
    _ = ?FIX:cleanup(Scope),
    case is_integer(Org) of
        true ->
            _ = ?FIX:exec(<<"DELETE FROM organization_member WHERE organization_id=$1">>, [Org]);
        false ->
            ok
    end,
    ok.
