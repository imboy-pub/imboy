%%% @doc EB-09 企业**租户面** handler 套件（真 HTTP + 真 facade + 真 PG）。
%%%
%%% 依据：EB-09-A01/A02/A03/A04/A06。三个套件的分工：
%%%   * 本套件 = 租户面的**真请求**（真 cowboy 路由 + 真 route metadata + 真
%%%     `eb_pg_auth_facts` 逐请求读库 + 真 facade + 真 scratch PG）；
%%%   * `eb_platform_handler_tests` = 平台面同构；
%%%   * `eb_route_contract_tests` = 契约/形状/静态红线。
%%%
%%% 隔离：合成租户由 `eb_pg_test_fixture` 随机 TSID 生成（不 TRUNCATE、不碰
%%% 共享 `imboy_v1`、不删别人的行）。
%%%
%%% **两个装配口径**（每个用例显式标注，不混）：
%%%   * `real`：`auth_facts => eb_pg_auth_facts`（生产装配）；
%%%   * `probe`：`auth_facts => eb09_facts_probe`（在**真事实**上补足本动作声明的
%%%     权限——因为生产装配的静态权限映射只产出 4 个值，写动作恒 403，见 findings
%%%     EB-09-F1）。用 probe 的用例都同时保留了 `real` 口径下的 403 观察。
-module(eb_tenant_handler_tests).

-include_lib("eunit/include/eunit.hrl").

-define(S, eb_handler_test_support).
-define(FIX, eb_pg_test_fixture).
-define(TIMEOUT_S, 60).

%% ===================================================================
%% setup：需要真库（scratch PG）
%% ===================================================================

tenant_suite_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun cases/1}.

setup() ->
    case eunit_runner:eunit_setup_with_db() of
        {ok, Conn} ->
            case ?FIX:ensure_purge_role() of
                ok -> {ok, Conn};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

cleanup({ok, Conn}) ->
    eunit_runner:eunit_cleanup_db(Conn);
cleanup(Other) ->
    Other.

cases({ok, _Conn}) ->
    [
        {timeout, ?TIMEOUT_S, fun a02_identity_list_returns_tsid_strings_real/0},
        {timeout, ?TIMEOUT_S, fun c5_identity_list_pagination_real/0},
        {timeout, ?TIMEOUT_S, fun a03_401_without_credential_real/0},
        {timeout, ?TIMEOUT_S, fun a03_403_cross_org_real/0},
        {timeout, ?TIMEOUT_S, fun a03_403_suspended_member_real/0},
        {timeout, ?TIMEOUT_S, fun a03_403_function_mismatch_real/0},
        {timeout, ?TIMEOUT_S, fun a03_note_write_authorization_unblocked_real/0},
        {timeout, ?TIMEOUT_S, fun a03_404_unknown_contact_real/0},
        {timeout, ?TIMEOUT_S, fun a03_409_duplicate_occupation_real/0},
        {timeout, ?TIMEOUT_S, fun fnd1_governance_identity_routes_real/0},
        {timeout, ?TIMEOUT_S, fun a03_422_missing_param_real/0},
        {timeout, ?TIMEOUT_S, fun a03_400_malformed_json_real/0},
        {timeout, ?TIMEOUT_S, fun a03_400_client_supplied_tenant_key_real/0},
        {timeout, ?TIMEOUT_S, fun a03_405_method_not_allowed_real/0},
        {timeout, ?TIMEOUT_S, fun a04_workspace_id_is_mandatory_real/0},
        {timeout, ?TIMEOUT_S, fun a06_ack_delivery_only_idempotent_probe/0},
        %% EB-07 播种后新增：A05 的 asset content **端到端**闭环（真 HTTP → 真 facade
        %% → 真 eb_asset_app → 本地替身对象存储 → 流式字节）
        {timeout, ?TIMEOUT_S, fun a05_asset_content_end_to_end_through_http/0},
        {timeout, ?TIMEOUT_S, fun a05_asset_content_negative_cases_through_http/0},
        {timeout, ?TIMEOUT_S, fun a05_asset_download_writes_no_personal_attachment/0},
        %% F6：服务端经 `imboy.eb_enterprise_keyring` 装配主密钥（两面）。
        {timeout, ?TIMEOUT_S, fun a05_presign_resolves_server_side_keyring/0},
        {timeout, ?TIMEOUT_S, fun a05_presign_fails_closed_without_server_key/0}
    ];
cases(Other) ->
    erlang:error({eb09_tenant_suite_db_unavailable, Other}).

%% ===================================================================
%% A02：TSID 全以 JSON string 传输（真链路：facade → store → JSON）
%% ===================================================================

a02_identity_list_returns_tsid_strings_real() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        Actor = maps:get(actor_user_id, Scope),
        %% 业务身份列表要求组织治理权（org.manage）：让持有 sales 经办关系的
        %% 成员升 admin（owner/admin 并列持治理权，§五第 1 条；org15 起每组织
        %% 唯一 active owner，fixture 的 owner 行不转移 projection 不能降）。
        ok = sql(
            <<"UPDATE organization_member SET role='admin' WHERE organization_id=$1 AND user_id=$2">>,
            [Org, Actor]
        ),
        ?S:with_listener(tenant, business_identities, session(real, Actor), fun(Port) ->
            Resp = ?S:request(
                Port,
                <<"GET">>,
                qs(?S:path(tenant, business_identities, #{org_id => Org}), [
                    {<<"workspace_id">>, Ws}
                ])
            ),
            ?assertEqual(200, maps:get(status, Resp)),
            ?assertEqual(0, ?S:code(Resp)),
            %% C5 页形状：payload = {business_identities, next_after_id}
            Payload = ?S:payload(Resp),
            ?assert(is_map(Payload)),
            Rows = maps:get(<<"business_identities">>, Payload),
            ?assert(is_list(Rows)),
            ?assert(length(Rows) >= 1),

            %% 每个 TSID 字段都是 **string**（不是 number）。解码后的 JSON 对象键是
            %% binary（jsx），故这里按 binary 键取。
            ?assertEqual(
                [],
                [
                    {K, maps:get(K, Row)}
                 || Row <- Rows,
                    K <- [<<"id">>, <<"organization_id">>, <<"workspace_id">>],
                    maps:is_key(K, Row),
                    not is_binary(maps:get(K, Row))
                ]
            ),
            %% 至少 id 一个字段真的在每一行上存在（避免上面的空集恒真）
            ?assertEqual(
                [],
                [Row || Row <- Rows, not maps:is_key(<<"id">>, Row)]
            ),
            ?assertEqual(
                [],
                [Row || Row <- Rows, not is_binary(maps:get(<<"id">>, Row, undefined))]
            ),
            %% C5：active_assignment 投影（对象或 null），TSID 键同样是 string
            ?assertEqual(
                [],
                [
                    Row
                 || Row <- Rows,
                    begin
                        AA = maps:get(<<"active_assignment">>, Row, missing),
                        AA =/= null andalso
                            not (is_map(AA) andalso is_binary(maps:get(<<"assignment_id">>, AA)) andalso
                                is_binary(maps:get(<<"user_id">>, AA)) andalso
                                maps:get(<<"status">>, AA) =:= <<"active">>)
                    end
                ]
            ),
            %% C5：next_after_id 为 null 或十进制 string
            Next = maps:get(<<"next_after_id">>, Payload, missing),
            ?assert(Next =:= null orelse is_binary(Next)),
            %% 原始 JSON 文本层面也不得出现数字形态的 TSID
            ?assertEqual(
                nomatch,
                re:run(?S:raw(Resp), <<"\"(id|organization_id|next_after_id)\":[0-9]">>, [
                    {capture, none}
                ])
            ),
            %% 非真空：同样的正则对「未编码」的载荷必须命中
            RawNumbered = jsx:encode(#{<<"id">> => 123, <<"organization_id">> => 456}),
            ?assertEqual(
                match,
                re:run(RawNumbered, <<"\"(id|organization_id)\":[0-9]">>, [
                    {capture, none}
                ])
            )
        end)
    after
        ?FIX:cleanup(Scope)
    end.

%% C5：分页参数门与投影形状（真 HTTP）。
%%   * limit 越界（0 / 201）⇒ 422 invalid_limit（显式登记，无兜底）；
%%   * 满页响应携带 next_after_id（string），用它翻页取到余下行。
c5_identity_list_pagination_real() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        Actor = maps:get(actor_user_id, Scope),
        ok = sql(
            <<"UPDATE organization_member SET role='admin' WHERE organization_id=$1 AND user_id=$2">>,
            [Org, Actor]
        ),
        ?S:with_listener(tenant, business_identities, session(real, Actor), fun(Port) ->
            Path = qs(?S:path(tenant, business_identities, #{org_id => Org}), [
                {<<"workspace_id">>, Ws}
            ]),
            %% limit 越界 ⇒ 422（不静默钳制、不 500）
            lists:foreach(
                fun(BadLimit) ->
                    Resp = ?S:request(
                        Port,
                        <<"GET">>,
                        qs(?S:path(tenant, business_identities, #{org_id => Org}), [
                            {<<"workspace_id">>, Ws}, {<<"limit">>, BadLimit}
                        ])
                    ),
                    ?assertEqual(422, maps:get(status, Resp)),
                    ?assertEqual(<<"invalid_limit">>, ?S:msg(Resp))
                end,
                [<<"0">>, <<"201">>]
            ),
            %% 全量（fixture 2 行 < 默认 50）⇒ 尾页 next_after_id = null
            All = ?S:request(Port, <<"GET">>, Path),
            ?assertEqual(200, maps:get(status, All)),
            AllRows = maps:get(<<"business_identities">>, ?S:payload(All)),
            ?assertEqual(null, maps:get(<<"next_after_id">>, ?S:payload(All))),
            ?assertEqual(2, length(AllRows)),
            AllIds = [maps:get(<<"id">>, R) || R <- AllRows],
            %% limit=1 翻页：页1 满页 ⇒ next_after_id 为 string 游标
            Page1 = ?S:request(
                Port,
                <<"GET">>,
                qs(?S:path(tenant, business_identities, #{org_id => Org}), [
                    {<<"workspace_id">>, Ws}, {<<"limit">>, <<"1">>}
                ])
            ),
            ?assertEqual(200, maps:get(status, Page1)),
            Payload1 = ?S:payload(Page1),
            Cursor = maps:get(<<"next_after_id">>, Payload1),
            ?assert(is_binary(Cursor)),
            [First | _] = AllIds,
            ?assertEqual(First, Cursor),
            %% 游标续读取到余下行（倒序键集）
            Page2 = ?S:request(
                Port,
                <<"GET">>,
                qs(?S:path(tenant, business_identities, #{org_id => Org}), [
                    {<<"workspace_id">>, Ws},
                    {<<"limit">>, <<"1">>},
                    {<<"after_id">>, Cursor}
                ])
            ),
            ?assertEqual(200, maps:get(status, Page2)),
            Page2Rows = maps:get(<<"business_identities">>, ?S:payload(Page2)),
            ?assertEqual([lists:last(AllIds)], [maps:get(<<"id">>, R) || R <- Page2Rows])
        end)
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% A03：401 / 403 / 404 / 409 / 422 / 400 / 405 的真负例
%% ===================================================================

a03_401_without_credential_real() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        %% current_uid = 0（未登录）：certificate 缺失 ⇒ 401，且**不触库**
        ?S:with_listener(tenant, contacts, session(real, 0), fun(Port) ->
            Resp = ?S:request(
                Port,
                <<"GET">>,
                qs(?S:path(tenant, contacts, #{org_id => Org}), [{<<"workspace_id">>, Ws}])
            ),
            ?assertEqual(401, maps:get(status, Resp)),
            ?assertEqual(<<"credential_missing">>, ?S:msg(Resp))
        end)
    after
        ?FIX:cleanup(Scope)
    end.

a03_403_cross_org_real() ->
    Scope = ?FIX:new_scope(),
    try
        OtherOrg = maps:get(other_org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        Actor = maps:get(actor_user_id, Scope),
        ?S:with_listener(tenant, contacts, session(real, Actor), fun(Port) ->
            Resp = ?S:request(
                Port,
                <<"GET">>,
                qs(?S:path(tenant, contacts, #{org_id => OtherOrg}), [{<<"workspace_id">>, Ws}])
            ),
            %% 事实源在本 Org 查不到该成员 ⇒ fail-closed 403（**不是** 500，
            %% 也**不是**「无租户条件」的空放行）
            ?assertEqual(403, maps:get(status, Resp)),
            ?assertEqual(<<"no_member">>, ?S:msg(Resp))
        end)
    after
        ?FIX:cleanup(Scope)
    end.

a03_403_suspended_member_real() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        Actor = maps:get(actor_user_id, Scope),
        ok = sql(
            <<
                "UPDATE organization_member SET status='suspended'"
                " WHERE organization_id=$1 AND user_id=$2"
            >>,
            [Org, Actor]
        ),
        ?S:with_listener(tenant, contacts, session(real, Actor), fun(Port) ->
            Resp = ?S:request(
                Port,
                <<"GET">>,
                qs(?S:path(tenant, contacts, #{org_id => Org}), [{<<"workspace_id">>, Ws}])
            ),
            ?assertEqual(403, maps:get(status, Resp)),
            ?assertNotEqual(nomatch, binary:match(?S:msg(Resp), <<"member_not_active">>))
        end)
    after
        ?FIX:cleanup(Scope)
    end.

a03_403_function_mismatch_real() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        Peer = maps:get(peer_user_id, Scope),
        Service = maps:get(service_identity_id, Scope),
        Owner = maps:get(owner_user_id, Scope),
        %% Peer 只有 customer_service 经办关系：访问 sales 动作必须 403
        ok = sql(
            <<
                "INSERT INTO organization_member(organization_id,user_id,role,status)"
                " VALUES ($1,$2,'member','active')"
            >>,
            [Org, Peer]
        ),
        ok = sql(
            <<
                "INSERT INTO organization_business_identity_assignment"
                " (id,organization_id,business_identity_id,function_key,user_id,status,"
                "  assigned_by,version) VALUES ($1,$2,$3,'customer_service',$4,'active',$5,1)"
            >>,
            [?FIX:id(), Org, Service, Peer, Owner]
        ),
        ?S:with_listener(tenant, contacts, session(real, Peer), fun(Port) ->
            Resp = ?S:request(
                Port,
                <<"GET">>,
                qs(?S:path(tenant, contacts, #{org_id => Org}), [{<<"workspace_id">>, Ws}])
            ),
            ?assertEqual(403, maps:get(status, Resp)),
            ?assertNotEqual(nomatch, binary:match(?S:msg(Resp), <<"function_mismatch">>))
        end)
    after
        ?FIX:cleanup(Scope)
    end.

%% F1（RULING-2026-09-15 §五）修复后的**翻转断言**：EB-09-F1 曾观察到生产装配下
%% note.write 不可满足（permission_missing，ACTION 声明与装配事实权限集不一致）。
%% 事实层权限投影（sales assignment → 8 项业务能力含 note.write）修复后，
%% 有 active sales assignment 的 actor **通过授权** —— 响应不再是 403，错误里
%% 不再出现 permission_missing；其后的非 2xx 属参数/业务校验层（F6 面），
%% note 表行数与响应类别一一对应（成功=1 行 / 校验失败=0 行）。
a03_note_write_authorization_unblocked_real() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        Actor = maps:get(actor_user_id, Scope),
        Contact = maps:get(contact_id, Scope),
        ?S:with_listener(tenant, append_note, session(real, Actor), fun(Port) ->
            Resp = ?S:request(
                Port,
                <<"POST">>,
                qs(?S:path(tenant, append_note, #{org_id => Org, id => Contact}), [
                    {<<"workspace_id">>, Ws}
                ]),
                #{body_plaintext => <<"eb09-note">>}
            ),
            ?assertNotEqual(403, maps:get(status, Resp)),
            ?assertEqual(nomatch, binary:match(?S:msg(Resp), <<"permission_missing">>)),
            %% 行数与响应类别对应：授权已不拦截，写入与否由参数/业务校验决定
            ExpectedRows =
                case maps:get(status, Resp) of
                    S when S >= 200, S < 300 -> 1;
                    _ -> 0
                end,
            ?assertEqual(
                ExpectedRows,
                ?S:scalar(
                    -1,
                    <<"SELECT count(*) FROM enterprise_note WHERE organization_id=$1">>,
                    [Org]
                )
            )
        end)
    after
        ?FIX:cleanup(Scope)
    end.

a03_404_unknown_contact_real() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        Actor = maps:get(actor_user_id, Scope),
        Unknown = ?FIX:id(),
        ?S:with_listener(tenant, contact_detail, session(real, Actor), fun(Port) ->
            Resp = ?S:request(
                Port,
                <<"GET">>,
                qs(?S:path(tenant, contact_detail, #{org_id => Org, id => Unknown}), [
                    {<<"workspace_id">>, Ws}
                ])
            ),
            ?assertEqual(404, maps:get(status, Resp)),
            %% application 的冻结语义是 contact_not_found（**不**回显 id，也不区分
            %% 「不存在」与「别的租户」——枚举防护由 store 的同语句租户键承担）
            ?assertEqual(<<"contact_not_found">>, ?S:msg(Resp))
        end)
    after
        ?FIX:cleanup(Scope)
    end.

%% FND-1/FND-2/F2（RULING-2026-09-15 §五/§六）实测：身份创建/绑定是治理动作。
%%   * owner **无任何 assignment** 也能创建业务身份（空 Org 自举解锁）；
%%   * 持有 active sales assignment 的 member 被 403 拒（业务身份不自动获得治理权）；
%%   * 审计 actor_user_id 来自已验证 member facts（owner 的 user id），非请求正文。
fnd1_governance_identity_routes_real() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        Owner = maps:get(owner_user_id, Scope),
        Actor = maps:get(actor_user_id, Scope),
        ?S:with_listener(tenant, business_identities, session(real, Owner), fun(Port) ->
            Resp = ?S:request(
                Port,
                <<"POST">>,
                qs(?S:path(tenant, business_identities, #{org_id => Org}), [
                    {<<"workspace_id">>, Ws}
                ]),
                #{function_key => <<"sales">>, display_name => <<"fnd1-owner-boot">>}
            ),
            ?assertEqual(200, maps:get(status, Resp)),
            %% 审计：actor 是 owner 本人（来自 member facts），与请求正文无关
            ?assertEqual(
                Owner,
                ?S:scalar(
                    -1,
                    <<
                        "SELECT actor_user_id FROM enterprise_audit_event"
                        " WHERE organization_id=$1 AND action='business_identity.create'"
                        " ORDER BY id DESC LIMIT 1"
                    >>,
                    [Org]
                )
            )
        end),
        ?S:with_listener(tenant, business_identities, session(real, Actor), fun(Port) ->
            Resp2 = ?S:request(
                Port,
                <<"POST">>,
                qs(?S:path(tenant, business_identities, #{org_id => Org}), [
                    {<<"workspace_id">>, Ws}
                ]),
                #{function_key => <<"sales">>, display_name => <<"fnd1-member-denied">>}
            ),
            ?assertEqual(403, maps:get(status, Resp2)),
            ?assertNotEqual(nomatch, binary:match(?S:msg(Resp2), <<"governance_insufficient">>))
        end)
    after
        ?FIX:cleanup(Scope)
    end.

a03_409_duplicate_occupation_real() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        Actor = maps:get(actor_user_id, Scope),
        Sales = maps:get(sales_identity_id, Scope),
        %% 治理权（org.manage）owner/admin 并列：让 Actor 升 admin（org15 唯一
        %% active owner 约束下，fixture 的 owner 行不转移 projection 不能降）。
        ok = sql(
            <<"UPDATE organization_member SET role='admin' WHERE organization_id=$1 AND user_id=$2">>,
            [Org, Actor]
        ),
        Before = assignments(Org),
        ?S:with_listener(tenant, assign_identity, session(real, Actor), fun(Port) ->
            Resp = ?S:request(
                Port,
                <<"POST">>,
                qs(?S:path(tenant, assign_identity, #{org_id => Org, id => Sales}), [
                    {<<"workspace_id">>, Ws}
                ]),
                #{user_id => integer_to_binary(Actor)}
            ),
            %% Actor 已有同 (Org, user, sales) 的 active 经办 ⇒ 重复占用 = 409。
            %% 真实标签是 application 冻结的 `duplicate_active_user_function`
            %% （基数门在 identity 占用门之前先判）。
            ?assertEqual(409, maps:get(status, Resp)),
            ?assertEqual(<<"duplicate_active_user_function">>, ?S:msg(Resp)),
            ?assertEqual(Before, assignments(Org))
        end)
    after
        ?FIX:cleanup(Scope)
    end.

a03_422_missing_param_real() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        Actor = maps:get(actor_user_id, Scope),
        ?S:with_listener(tenant, contacts, session(real, Actor), fun(Port) ->
            %% POST /contacts 缺 subject（动作表 required）⇒ 422，零调用
            Resp = ?S:request(
                Port,
                <<"POST">>,
                qs(?S:path(tenant, contacts, #{org_id => Org}), [{<<"workspace_id">>, Ws}]),
                #{channel => <<"web">>}
            ),
            ?assertEqual(422, maps:get(status, Resp)),
            ?assertEqual(<<"missing_param.subject">>, ?S:msg(Resp))
        end)
    after
        ?FIX:cleanup(Scope)
    end.

a03_400_malformed_json_real() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        Actor = maps:get(actor_user_id, Scope),
        Contact = maps:get(contact_id, Scope),
        ?S:with_listener(tenant, contact_detail, session(real, Actor), fun(Port) ->
            Resp = ?S:request(
                Port,
                <<"PATCH">>,
                qs(?S:path(tenant, contact_detail, #{org_id => Org, id => Contact}), [
                    {<<"workspace_id">>, Ws}
                ]),
                <<"{not-json">>
            ),
            ?assertEqual(400, maps:get(status, Resp)),
            ?assertEqual(<<"malformed_json">>, ?S:msg(Resp))
        end)
    after
        ?FIX:cleanup(Scope)
    end.

a03_400_client_supplied_tenant_key_real() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        Actor = maps:get(actor_user_id, Scope),
        ?S:with_listener(tenant, contacts, session(real, Actor), fun(Port) ->
            Resp = ?S:request(
                Port,
                <<"POST">>,
                qs(?S:path(tenant, contacts, #{org_id => Org}), [{<<"workspace_id">>, Ws}]),
                #{
                    channel => <<"web">>,
                    subject => <<"eb09">>,
                    organization_id => integer_to_binary(?FIX:id())
                }
            ),
            ?assertEqual(400, maps:get(status, Resp)),
            ?assertEqual(<<"forbidden_client_key.organization_id">>, ?S:msg(Resp))
        end)
    after
        ?FIX:cleanup(Scope)
    end.

a03_405_method_not_allowed_real() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        Actor = maps:get(actor_user_id, Scope),
        ?S:with_listener(tenant, contacts, session(real, Actor), fun(Port) ->
            Resp = ?S:request(
                Port,
                <<"DELETE">>,
                qs(?S:path(tenant, contacts, #{org_id => Org}), [{<<"workspace_id">>, Ws}])
            ),
            ?assertEqual(405, maps:get(status, Resp)),
            ?assertEqual(<<"method_not_allowed">>, ?S:msg(Resp))
        end)
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% A04：租户面同样强制显式 Workspace（缺席即 422，绝不取默认）
%% ===================================================================

a04_workspace_id_is_mandatory_real() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Actor = maps:get(actor_user_id, Scope),
        ?S:with_listener(tenant, contacts, session(real, Actor), fun(Port) ->
            Resp = ?S:request(Port, <<"GET">>, ?S:path(tenant, contacts, #{org_id => Org})),
            ?assertEqual(422, maps:get(status, Resp)),
            ?assertEqual(<<"missing_workspace_id">>, ?S:msg(Resp))
        end)
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% A06：ACK = delivery-only；重复 ACK 幂等；canonical 逐字不变
%% ===================================================================

a06_ack_delivery_only_idempotent_probe() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        Actor = maps:get(actor_user_id, Scope),
        Contact = maps:get(contact_id, Scope),
        %% 生产装配下 message.write 不可满足（EB-09-F1）：本用例显式切到 probe 装配，
        %% 它只把**本动作声明的权限**补进真事实（见 eb09_facts_probe 的边界说明）。
        ok = eb09_facts_probe:grant([<<"message.write">>]),
        MsgId = insert_canonical_message(Scope),
        CanonicalBefore = canonical_row(MsgId),
        ?S:with_listener(tenant, ack_delivery, session(probe, Actor), fun(Port) ->
            Path = qs(
                ?S:path(tenant, ack_delivery, #{
                    org_id => Org, id => maps:get(conversation_id, Scope), message_id => MsgId
                }),
                [{<<"workspace_id">>, Ws}]
            ),
            Body = #{
                recipient_ref => <<"contact:", (integer_to_binary(Contact))/binary>>,
                %% 同一 device 的重复 ACK 才是同一投递事实（DB 唯一键含 device_id；
                %% device_id 为 NULL 时 Postgres 视每行为不同，见 EB-06 的
                %% a14_null_device_replay_observation）。
                device_id => <<"eb09-device-1">>
            },
            R1 = ?S:request(Port, <<"POST">>, ack_path(Path, MsgId), Body),
            ?assertEqual(200, maps:get(status, R1)),
            ?assertEqual(0, ?S:code(R1)),
            R2 = ?S:request(Port, <<"POST">>, ack_path(Path, MsgId), Body),
            ?assertEqual(200, maps:get(status, R2)),
            ?assertEqual(0, ?S:code(R2)),
            %% ① 幂等：delivery 行数不因第二次 ACK 增加
            ?assertEqual(1, deliveries(Org, Ws, MsgId)),
            %% ② canonical 逐字不变（行存在且关键字段一致）
            ?assertEqual(CanonicalBefore, canonical_row(MsgId)),
            ?assertEqual(1, canonical_count(Org, Ws, MsgId)),
            %% ③ 响应不得暗示归档/删除
            ?assertEqual(
                [],
                [
                    W
                 || W <- [<<"delete">>, <<"archive">>, <<"purge">>, <<"removed">>, <<"hidden">>],
                    binary:match(?S:raw(R1), W) =/= nomatch
                ]
            ),
            %% ④ 非真空：同一扫描对含 deleted 的响应必须命中
            ?assertNotEqual(nomatch, binary:match(<<"{\"deleted\":true}">>, <<"delete">>))
        end)
    after
        eb09_facts_probe:clear(),
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% A05：asset content 端到端（EB-07 播种后的完整链路）
%% ===================================================================

%% @doc A05 的**正向闭环**：客户端 PUT 直写私有桶（不经本 API）→ 经 HTTP 下载。
%%
%% 链路：真 cowboy 路由/route metadata → eb_tenant_handler → eb_auth_app（真事实
%% 逐请求读库）→ facade `content_stream/2` → **真** `eb_asset_app`（EB-07 播种）→
%% `eb_asset_store`（本地替身对象存储）→ 对象字节**流式**返回。
%%
%% 断言（红线，逐条）：① 返回的是**对象字节本身**（不是 URL/JSON 包装、不是重定向）；
%% ② 响应头只含白名单四项且 `x-asset-id`/`x-asset-sha256` 与真值一致；
%% ③ 响应（含头）在**值层**扫不到派生 object key / key 前缀 / `://` / bucket /
%% endpoint / presign / garage；④ 无 `location` 头（不签发 GET URL）。
a05_asset_content_end_to_end_through_http() ->
    Scope = eb_asset_it_lib:new_scope(),
    ok = eb09_facts_probe:grant([<<"asset.read">>]),
    try
        Actor = maps:get(actor_user_id, Scope),
        {Org, Ws} = eb_asset_it_lib:tenant(Scope),
        Payload = <<"EB09-A05-PAYLOAD-", (integer_to_binary(?FIX:id()))/binary>>,
        Hash = eb_asset_content:sha256_hex(Payload),
        AssetId = upload_canonical(Scope, Actor, Payload, Hash),
        %% 对象确实落在**私有桶**里（否则下面的「返回的是对象字节」可能只是空跑）
        ?assertEqual(true, eb_asset_it_lib:object_present(Org, Ws, AssetId)),
        ?assertEqual({ok, active}, wrap_status(eb_asset_it_lib:asset_status(Org, Ws, AssetId))),
        ?S:with_listener(tenant, asset_content, session(probe, Actor), fun(Port) ->
            Resp = ?S:request(
                Port,
                <<"GET">>,
                qs(?S:path(tenant, asset_content, #{org_id => Org, id => AssetId}), [
                    {<<"workspace_id">>, Ws}
                ])
            ),
            ?assertEqual(200, maps:get(status, Resp)),
            %% ① 流式返回的字节 == 上传的字节（逐字节相等，不是「包含」）
            ?assertEqual(Payload, maps:get(body, Resp)),
            %% ② 白名单头 + 真值
            Headers = maps:get(headers, Resp),
            ?assertEqual(<<"text/plain">>, maps:get(<<"content-type">>, Headers)),
            ?assertEqual(<<"private, no-store">>, maps:get(<<"cache-control">>, Headers)),
            ?assertEqual(integer_to_binary(AssetId), maps:get(<<"x-asset-id">>, Headers)),
            ?assertEqual(Hash, maps:get(<<"x-asset-sha256">>, Headers)),
            %% ④ 不是重定向、不签发 GET URL
            ?assertNot(maps:is_key(<<"location">>, Headers)),
            %% ③ 值层扫描：真派生 object key 与 key 前缀都不得出现在响应里
            Raw = ?S:raw(Resp),
            ?assertEqual(ok, eb_enterprise_http:storage_leak_scan(Raw)),
            ?assertEqual(
                nomatch,
                binary:match(Raw, eb_asset_it_lib:object_key(Org, Ws, AssetId))
            ),
            ?assertEqual(nomatch, binary:match(Raw, eb_asset_it_lib:key_prefix(Org, Ws))),
            %% 非真空：把同一个对象 key 拼进一段文本，同一判据必须命中
            ?assertNotEqual(
                nomatch,
                binary:match(
                    <<"probe ", (eb_asset_it_lib:object_key(Org, Ws, AssetId))/binary>>,
                    eb_asset_it_lib:object_key(Org, Ws, AssetId)
                )
            )
        end)
    after
        ok = eb09_facts_probe:clear(),
        ?FIX:cleanup(Scope)
    end.

%% @doc A05 的**真负例**（逐条真请求，且每条都断言响应体里没有对象字节）：
%%   * 跨 Org：租户归属换成另一个 Organization；
%%   * 跨 Workspace：同 Org 换 Workspace；
%%   * 猜测 asset id：同 Org 同 Workspace 的随机 TSID；
%%   * 非经办人：同 Org 的 active member 但经办身份不是会话当前经办；
%%   * suspend 后的旧 JWT：成员被置 suspended 后再下载。
a05_asset_content_negative_cases_through_http() ->
    Scope = eb_asset_it_lib:new_scope(),
    ok = eb09_facts_probe:grant([<<"asset.read">>]),
    try
        Actor = maps:get(actor_user_id, Scope),
        Peer = maps:get(peer_user_id, Scope),
        Service = maps:get(service_identity_id, Scope),
        {Org, Ws} = eb_asset_it_lib:tenant(Scope),
        OtherOrg = maps:get(other_org_id, Scope),
        OtherWs = maps:get(other_workspace_id, Scope),
        Payload = <<"EB09-A05-NEG-", (integer_to_binary(?FIX:id()))/binary>>,
        Hash = eb_asset_content:sha256_hex(Payload),
        AssetId = upload_canonical(Scope, Actor, Payload, Hash),
        %% 两个**不同**的普通成员，各自只持一条经办，才能分别打出两道不同的门：
        %%   * `Peer`（夹具 peer_user_id）只持 customer_service 经办
        %%     ⇒ 在**路由职能门**被拒（required_function = sales）→ function_mismatch
        %%   * `Other`（新合成 user）只持另一条 **sales** 身份（ExtraSales ≠ 会话当前经办）
        %%     ⇒ 通过路由职能门，但在 **asset ACL** 的「经办身份 == 会话当前经办」判据被拒
        %%     → {forbidden, not_assignee}
        %% 若只造一个成员并把两条经办都给他，他会通过职能门而落进 not_assignee——
        %% 那样 function_mismatch 这条门就**从未被真跑**（本卡首版即犯此错，已修正）。
        ExtraSales = ?FIX:id(),
        Other = ?FIX:id(),
        ok = ?FIX:exec(
            <<
                "INSERT INTO \"user\"(id,password,account,reg_ip,reg_cosv)"
                " VALUES ($1,'x',$2,'127.0.0.1','x')"
            >>,
            [Other, <<"eb09-other-", (integer_to_binary(Other))/binary>>]
        ),
        ok = ?FIX:exec(
            <<
                "INSERT INTO organization_business_identity"
                " (id,organization_id,function_key,display_name,status,version,created_by_user_id)"
                " VALUES ($1,$2,'sales',$3,'active',1,$4)"
            >>,
            [ExtraSales, Org, <<"eb09-extra-sales">>, Actor]
        ),
        ok = eb_asset_it_lib:add_member(Org, Peer),
        ok = eb_asset_it_lib:add_member(Org, Other),
        ok = eb_asset_it_lib:assignment_for(Org, Ws, Peer, Service, <<"customer_service">>),
        ok = eb_asset_it_lib:assignment_for(Org, Ws, Other, ExtraSales, <<"sales">>),
        %% 每个负例断言**精确**的状态码与稳定标签（不是「403 或 404」的宽松集合）
        Cases = [
            {cross_org_no_member, Peer, #{org_id => OtherOrg, id => AssetId}, Ws,
                {403, <<"no_member">>}},
            {cross_workspace, Actor, #{org_id => Org, id => AssetId}, OtherWs,
                {404, <<"not_found">>}},
            {guessed_asset_id, Actor, #{org_id => Org, id => ?FIX:id()}, Ws,
                {404, <<"not_found">>}},
            {function_mismatch, Peer, #{org_id => Org, id => AssetId}, Ws,
                {403, <<"function_mismatch">>}},
            {not_assignee_other_sales_identity, Other, #{org_id => Org, id => AssetId}, Ws,
                {403, <<"forbidden.not_assignee">>}}
        ],
        lists:foreach(
            fun({Name, User, Bindings, WorkspaceId, Expected}) ->
                assert_content_denied(Name, User, Bindings, WorkspaceId, Payload, Expected)
            end,
            Cases
        ),
        %% suspend 后的旧 JWT：同一 actor、同一请求，成员状态一改即失效（无缓存）
        ok = suspend(Org, Actor),
        assert_content_denied(
            suspended_actor,
            Actor,
            #{org_id => Org, id => AssetId},
            Ws,
            Payload,
            {403, <<"member_not_active.suspended">>}
        )
    after
        ok = eb09_facts_probe:clear(),
        ?FIX:cleanup(Scope)
    end.

assert_content_denied(Name, User, Bindings, WorkspaceId, Payload, {ExpStatus, ExpMsg}) ->
    ?S:with_listener(tenant, asset_content, session(probe, User), fun(Port) ->
        Resp = ?S:request(
            Port,
            <<"GET">>,
            qs(?S:path(tenant, asset_content, Bindings), [{<<"workspace_id">>, WorkspaceId}])
        ),
        ?assertEqual(
            {Name, ExpStatus},
            {Name, maps:get(status, Resp)},
            #{body => maps:get(body, Resp)}
        ),
        %% 稳定标签逐字相等（标签只含原子路径，不含取值）
        ?assertEqual({Name, ExpMsg}, {Name, ?S:msg(Resp)}),
        %% 拒绝时**不得**回漏对象字节，也不得回漏存储侧引用
        ?assertEqual(nomatch, binary:match(?S:raw(Resp), Payload)),
        ?assertEqual(ok, eb_enterprise_http:storage_leak_scan(?S:raw(Resp)))
    end).

%% @doc 下载是**只读代理**：不得登记为个人 private attachment（EB-07 硬约束 5 的
%% 接口侧复核；同一判据在 EB-07 的 IT 套件里对上传侧也成立）。
a05_asset_download_writes_no_personal_attachment() ->
    Scope = eb_asset_it_lib:new_scope(),
    ok = eb09_facts_probe:grant([<<"asset.read">>]),
    try
        Actor = maps:get(actor_user_id, Scope),
        {Org, Ws} = eb_asset_it_lib:tenant(Scope),
        Payload = <<"EB09-A05-RO-", (integer_to_binary(?FIX:id()))/binary>>,
        Hash = eb_asset_content:sha256_hex(Payload),
        AssetId = upload_canonical(Scope, Actor, Payload, Hash),
        Before = personal_rows(Scope),
        ?S:with_listener(tenant, asset_content, session(probe, Actor), fun(Port) ->
            lists:foreach(
                fun(_) ->
                    Resp = ?S:request(
                        Port,
                        <<"GET">>,
                        qs(?S:path(tenant, asset_content, #{org_id => Org, id => AssetId}), [
                            {<<"workspace_id">>, Ws}
                        ])
                    ),
                    ?assertEqual(200, maps:get(status, Resp)),
                    ?assertEqual(Payload, maps:get(body, Resp))
                end,
                [1, 2, 3]
            )
        end),
        %% 三次下载后个人 attachment 行数不变，且企业附件元数据仍 active
        ?assertEqual(Before, personal_rows(Scope)),
        ?assertEqual({ok, active}, wrap_status(eb_asset_it_lib:asset_status(Org, Ws, AssetId))),
        ?assertEqual(Hash, eb_asset_it_lib:asset_hash(Org, Ws, AssetId))
    after
        ok = eb09_facts_probe:clear(),
        ?FIX:cleanup(Scope)
    end.

%% @doc F6（RULING-2026-09-15 §七）presign 的**两面**合同。
%%
%% 生产装配：主密钥只来自服务端 application 配置 `imboy.eb_enterprise_keyring`
%% （`eb_env_keyring` 严格解码）；HTTP 面不接收、动作表也不声明任何密钥参数。
%%   * 正面：env 注入有效 keyring ⇒ presign **服务端解析成功**（200 + 不透明
%%     upload_ref；响应仍无 URL/endpoint/object key）；
%%   * 负面：env 无 keyring（部署缺陷/未配置）⇒ **500 missing_key** fail-closed，
%%     绝不降级为明文、绝不伪装成 4xx，零副作用。
%% 权限口径：两面都用测试装配补 `asset.write`（F1 的权限授予缺口），使请求能抵达
%% 用例层；否则会在授权门 403 而观测不到密钥装配语义。密钥面与权限无关。
a05_presign_resolves_server_side_keyring() ->
    Scope = eb_asset_it_lib:new_scope(),
    ok = eb09_facts_probe:grant([<<"asset.write">>]),
    try
        #{key := Key, key_version := V} = eb_asset_it_lib:key_ref(Scope),
        ok = application:set_env(imboy, eb_enterprise_keyring, #{
            active_version => V,
            keys => #{V => binary:encode_hex(Key, lowercase)}
        }),
        Actor = maps:get(actor_user_id, Scope),
        {Org, Ws} = eb_asset_it_lib:tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        Payload = <<"EB09-A05-PRESIGN-OK">>,
        ?S:with_listener(tenant, presign, session(probe, Actor), fun(Port) ->
            Resp = ?S:request(
                Port,
                <<"POST">>,
                qs(?S:path(tenant, presign, #{org_id => Org}), [{<<"workspace_id">>, Ws}]),
                #{
                    conversation_id => integer_to_binary(Conv),
                    mime => <<"text/plain">>,
                    size_bytes => byte_size(Payload),
                    object_hash => eb_asset_content:sha256_hex(Payload)
                }
            ),
            %% 服务端装配成功 ⇒ 200 + 不透明上传凭证
            ?assertEqual(200, maps:get(status, Resp)),
            ?assertEqual(0, ?S:code(Resp)),
            View = ?S:payload(Resp),
            ?assert(is_binary(maps:get(<<"upload_ref">>, View, undefined))),
            %% 响应面纪律不变：无 URL / endpoint / bucket / 签名串等真实存储能力
            %% （storage_leak_scan 全量 needle 会误撞 A05 契约声明文本
            %% `opaque_token_no_url_no_object_key`，故这里扫真实泄露标志）。
            Raw = ?S:raw(Resp),
            lists:foreach(
                fun(Needle) ->
                    ?assertEqual({Needle, nomatch}, {Needle, binary:match(Raw, Needle)})
                end,
                [<<"://">>, <<"X-Amz-">>, <<"endpoint">>, <<"bucket">>]
            )
        end)
    after
        %% env 是 VM 级：两面互不污染，也不外泄到其他用例/套件。
        _ = application:unset_env(imboy, eb_enterprise_keyring),
        ok = eb09_facts_probe:clear(),
        ?FIX:cleanup(Scope)
    end.

a05_presign_fails_closed_without_server_key() ->
    Scope = eb_asset_it_lib:new_scope(),
    ok = eb09_facts_probe:grant([<<"asset.write">>]),
    try
        %% 显式保证「无 keyring」前提（前序用例/套件的 env 已在 after 清理）。
        _ = application:unset_env(imboy, eb_enterprise_keyring),
        Actor = maps:get(actor_user_id, Scope),
        {Org, Ws} = eb_asset_it_lib:tenant(Scope),
        Conv = maps:get(conversation_id, Scope),
        Payload = <<"EB09-A05-PRESIGN">>,
        Before = assets(Org, Ws),
        ?S:with_listener(tenant, presign, session(probe, Actor), fun(Port) ->
            Resp = ?S:request(
                Port,
                <<"POST">>,
                qs(?S:path(tenant, presign, #{org_id => Org}), [{<<"workspace_id">>, Ws}]),
                #{
                    conversation_id => integer_to_binary(Conv),
                    mime => <<"text/plain">>,
                    size_bytes => byte_size(Payload),
                    object_hash => eb_asset_content:sha256_hex(Payload)
                }
            ),
            %% 服务端配置缺失 ⇒ 500（fail-closed），绝不 200
            ?assertEqual(500, maps:get(status, Resp)),
            ?assertEqual(<<"missing_key">>, ?S:msg(Resp)),
            %% 响应里不得出现任何存储侧能力，也不得凭空给出凭证
            ?assertEqual(ok, eb_enterprise_http:storage_leak_scan(?S:raw(Resp))),
            ?assertEqual(nomatch, binary:match(?S:raw(Resp), <<"upload_ref">>))
        end),
        %% 零副作用：不落 enterprise_asset 行
        ?assertEqual(Before, assets(Org, Ws))
    after
        ok = eb09_facts_probe:clear(),
        ?FIX:cleanup(Scope)
    end.

%% -------------------------------------------------------------------
%% A05 内部辅助
%% -------------------------------------------------------------------

%% 服务端准备：facade presign（真端口 + 测试主密钥）→ PUT 直写私有桶 →
%% confirm。返回 asset_id。这是**生产**的 presign/PUT/confirm 语义（PUT 不经本 API）。
upload_canonical(Scope, Actor, Payload, Hash) ->
    Conv = maps:get(conversation_id, Scope),
    {ok, Presign} = eb_asset_it_lib:facade_presign(Scope, Actor, #{
        conversation_id => Conv,
        mime => <<"text/plain">>,
        size_bytes => byte_size(Payload),
        object_hash => Hash
    }),
    AssetId = maps:get(asset_id, Presign),
    Ref = maps:get(upload_ref, Presign),
    {Org, Ws} = eb_asset_it_lib:tenant(Scope),
    {ok, _Put} = eb_asset_app:put_object(Org, #{
        workspace_id => Ws,
        actor_user_id => Actor,
        conversation_id => Conv,
        upload_ref => Ref,
        payload => Payload,
        key_ref => eb_asset_it_lib:key_ref(Scope)
    }),
    {ok, Confirmed} = eb_asset_it_lib:facade_confirm(Scope, Actor, Ref, Conv),
    ?assertEqual(active, maps:get(status, Confirmed, undefined)),
    AssetId.

suspend(Org, UserId) ->
    ok = ?FIX:exec(
        <<
            "UPDATE organization_member SET status='suspended'"
            " WHERE organization_id=$1 AND user_id=$2"
        >>,
        [Org, UserId]
    ).

personal_rows(Scope) ->
    Users = [
        maps:get(owner_user_id, Scope),
        maps:get(actor_user_id, Scope),
        maps:get(peer_user_id, Scope)
    ],
    ?S:scalar(-1, <<"SELECT count(*) FROM attachment WHERE creator_user_id = ANY($1)">>, [Users]).

assets(Org, Ws) ->
    ?S:scalar(
        -1,
        <<"SELECT count(*) FROM enterprise_asset WHERE organization_id=$1 AND workspace_id=$2">>,
        [Org, Ws]
    ).

wrap_status({error, not_found}) -> {error, not_found};
wrap_status(Other) -> {ok, Other}.

%% ===================================================================
%% 内部辅助
%% ===================================================================

session(real, Uid) ->
    #{current_uid => Uid, auth_facts => ?S:facts(real)};
session(probe, Uid) ->
    #{current_uid => Uid, auth_facts => ?S:facts({probe, []})}.

%% ACK 路径在动作表里是 `/conversations/:id/messages/:message_id/ack`；
%% `path/3` 已把两个参数替换好，这里只做「非空断言」式的自检。
ack_path(Path, MsgId) ->
    ?assertNotEqual(nomatch, binary:match(Path, integer_to_binary(MsgId))),
    Path.

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

sql(Sql, Params) ->
    ok = ?FIX:exec(Sql, Params).

assignments(Org) ->
    ?S:scalar(
        0,
        <<
            "SELECT count(*) FROM organization_business_identity_assignment"
            " WHERE organization_id=$1"
        >>,
        [Org]
    ).

insert_canonical_message(Scope) ->
    Org = maps:get(org_id, Scope),
    Ws = maps:get(workspace_id, Scope),
    Conv = maps:get(conversation_id, Scope),
    MsgId = ?FIX:id(),
    Aad = #{
        organization_id => Org,
        workspace_id => Ws,
        conversation_id => Conv,
        message_id => MsgId
    },
    {ok, Sealed} = eb_managed_crypto:seal(Aad, ?FIX:canary(), ?FIX:key_ref(1)),
    {ok, _Row} = eb_pg_store:append_message(Org, Ws, #{
        id => MsgId,
        conversation_id => Conv,
        client_msg_id => <<"eb09-ack-", (integer_to_binary(MsgId))/binary>>,
        sender_type => <<"contact">>,
        sender_contact_id => maps:get(contact_id, Scope),
        sender_business_identity_id => null,
        actor_user_id => null,
        body_cipher => maps:get(cipher, Sealed),
        key_version => maps:get(key_version, Sealed),
        aad_hash => maps:get(aad_hash, Sealed),
        content_hash => binary:encode_hex(crypto:hash(sha256, maps:get(cipher, Sealed))),
        policy_id => maps:get(policy_id, Scope),
        policy_version => 1,
        retention_days => 1095,
        retain_until => now_secs() + 86400
    }),
    MsgId.

now_secs() ->
    erlang:system_time(second).

canonical_row(MsgId) ->
    ?S:scalar(
        undefined,
        <<
            "SELECT coalesce(content_hash,'') || '|' || coalesce(aad_hash,'') || '|'"
            " || coalesce(body_cipher,'') || '|' || visibility || '|' || version::text"
            " FROM enterprise_message WHERE id=$1"
        >>,
        [MsgId]
    ).

canonical_count(Org, Ws, MsgId) ->
    ?S:scalar(
        0,
        <<
            "SELECT count(*) FROM enterprise_message"
            " WHERE organization_id=$1 AND workspace_id=$2 AND id=$3"
        >>,
        [Org, Ws, MsgId]
    ).

deliveries(Org, Ws, MsgId) ->
    ?S:scalar(
        0,
        <<
            "SELECT count(*) FROM enterprise_message_delivery"
            " WHERE organization_id=$1 AND workspace_id=$2 AND message_id=$3"
        >>,
        [Org, Ws, MsgId]
    ).
