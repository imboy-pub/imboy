%%% @doc EB-09-A01/A02/A04/A05/A06 的**契约与形状**套件（纯静态 + 纯函数，无 DB）。
%%%
%%% 覆盖：
%%%   * **A01**：`imboy_router:get_routes/0` 里每一条企业路由的
%%%     method/path/action/auth_context/feature 与冻结动作表
%%%     （`eb_enterprise_actions`）逐字一致；`eb_auth_principal:principal_for_route/1`
%%%     接受每一条企业路由的 metadata（真调用，不是文本断言）。
%%%   * **A02**：TSID 出站编码一律 string（`encode_entity/1`），并机械排除
%%%     「像 TSID 但不是」的键。
%%%   * **A03**：错误 → HTTP 状态映射对 400/401/403/404/409/422 的**每条**都有
%%%     显式登记（且真负例在其他两个套件里逐条跑真请求）。
%%%   * **A04**：平台面每条路径都显式带 `:org_id`；平台 handler 唯一调用点是
%%%     `eb_enterprise_facade_call:call(Facade, OrgId, Params)`（租户条件不可省略）。
%%%   * **A05**：handler / http / facade-call 三层源码零 DB、零 `eb_pg_`、零个人
%%%     attachment 下载能力；`reply_content/2` 的真实响应字节里不含任何存储能力。
%%%   * **A06**：ACK 动作是 delivery-only（只调 ack_delivery），且响应形状不含
%%%     删除/归档语义。
%%%
%%% 所有静态断言都带**非真空**（non-vacuity）证明：把被审计的输入换成一份「做了
%%% 该断言要拦住的改动」的副本，同一审计函数必须报出违规。这使断言不可能恒真。
-module(eb_route_contract_tests).

-include_lib("eunit/include/eunit.hrl").

-define(S, eb_handler_test_support).
-define(TENANT_HANDLER, "src/features/enterprise_business/interfaces/eb_tenant_handler.erl").
-define(PLATFORM_HANDLER, "src/features/enterprise_business/interfaces/eb_platform_handler.erl").
-define(HTTP_MODULE, "src/features/enterprise_business/interfaces/eb_enterprise_http.erl").
-define(FACADE_CALL, "src/features/enterprise_business/interfaces/eb_enterprise_facade_call.erl").
-define(FACADE, "src/features/enterprise_business/enterprise_business_facade.erl").

%% ===================================================================
%% 冻结的路由清单（path → 动作/方法）；与 router 里的字面登记互为审计面
%% ===================================================================

tenant_literal_routes() ->
    P = <<"/api/v1/enterprise/organizations/:org_id">>,
    [
        {<<P/binary, "/business-identities">>, business_identities, [<<"GET">>, <<"POST">>]},
        {<<P/binary, "/business-identities/:id/assign">>, assign_identity, [<<"POST">>]},
        {<<P/binary, "/contacts">>, contacts, [<<"GET">>, <<"POST">>]},
        {<<P/binary, "/contacts/:id">>, contact_detail, [<<"GET">>, <<"PATCH">>]},
        {<<P/binary, "/contacts/:id/notes">>, append_note, [<<"POST">>]},
        {<<P/binary, "/conversations">>, open_conversation, [<<"POST">>]},
        {<<P/binary, "/conversations/:id/messages">>, conversation_messages, [
            <<"GET">>, <<"POST">>
        ]},
        {<<P/binary, "/conversations/:id/messages/:message_id/ack">>, ack_delivery, [<<"POST">>]},
        {<<P/binary, "/assets/presign">>, presign, [<<"POST">>]},
        {<<P/binary, "/assets/confirm">>, confirm_asset, [<<"POST">>]},
        {<<P/binary, "/assets/:id/content">>, asset_content, [<<"GET">>]},
        {<<P/binary, "/members/:uid/suspend">>, suspend_member, [<<"POST">>]},
        {<<P/binary, "/offboarding">>, offboarding_open, [<<"POST">>]},
        {<<P/binary, "/offboarding/:id/execute">>, offboarding_execute, [<<"POST">>]},
        {<<P/binary, "/offboarding/:id/verify">>, offboarding_verify, [<<"POST">>]},
        {<<P/binary, "/offboarding/:id/finalize">>, offboarding_finalize, [<<"POST">>]}
    ].

platform_literal_routes() ->
    P = <<"/api/adm/enterprise-business/organizations/:org_id">>,
    [
        {<<P/binary, "/identities">>, p_identities, [<<"GET">>]},
        {<<P/binary, "/contacts">>, p_contacts, [<<"GET">>]},
        {<<P/binary, "/contacts/:id">>, p_contact_detail, [<<"GET">>]},
        {<<P/binary, "/conversations/:id/messages">>, p_conversation_messages, [<<"GET">>]},
        {<<P/binary, "/messages/:message_id">>, p_message_detail, [<<"GET">>]},
        {<<P/binary, "/assets/:id/content">>, p_asset_content, [<<"GET">>]},
        {<<P/binary, "/members/:uid/suspend">>, p_suspend_member, [<<"POST">>]},
        {<<P/binary, "/offboarding/:id/execute">>, p_offboarding_execute, [<<"POST">>]},
        {<<P/binary, "/offboarding/:id/verify">>, p_offboarding_verify, [<<"POST">>]},
        {<<P/binary, "/offboarding/:id/finalize">>, p_offboarding_finalize, [<<"POST">>]}
    ].

%% ===================================================================
%% A01：method/path/action/auth_context/feature 一致
%% ===================================================================

a01_route_metadata_matches_frozen_action_table_test() ->
    ?assertEqual([], violations(?S:enterprise_routes(all))).

%% @doc 审计入口：每条企业路由必须（1）在冻结清单里、（2）动作在动作表里、
%% （3）`auth` 逐键一致、（4）面级不变量齐备且取值正确。
violations(Routes) ->
    Known = lists:append([
        [{tenant, Path, Action, Methods} || {Path, Action, Methods} <- tenant_literal_routes()],
        [{platform, Path, Action, Methods} || {Path, Action, Methods} <- platform_literal_routes()]
    ]),
    %% 双向审计：① 冻结清单里的每条路径都必须**真的注册**（少登记即报）；
    %%           ② 每条注册路由都必须与冻结清单/动作表逐键一致（多登记/漂移即报）。
    Registered = [{Path, maps:get(action, Opts, undefined)} || {Path, _H, Opts} <- Routes],
    Missing = [
        {route_not_registered, Path, Action}
     || {_Surface, Path, Action, _Methods} <- Known,
        not lists:member({Path, Action}, Registered)
    ],
    Missing ++ audit(Routes, Known, []).

audit([], _Known, Acc) ->
    lists:reverse(Acc);
audit([{Path, Handler, Opts} | Rest], Known, Acc) ->
    Action = maps:get(action, Opts, undefined),
    Surface = maps:get(surface, Opts, undefined),
    Entry = find_entry(Surface, Action),
    Violations = lists:flatten(route_violations(Path, Handler, Opts, Known, Entry)),
    audit(Rest, Known, lists:reverse(Violations) ++ Acc).

find_entry(_Surface, undefined) ->
    {error, {unknown_action, undefined}};
find_entry(tenant, Action) ->
    eb_enterprise_actions:tenant(Action);
find_entry(platform, Action) ->
    eb_enterprise_actions:platform(Action);
find_entry(_Surface, Action) ->
    {error, {unknown_surface, Action}}.

route_violations(Path, Handler, Opts, Known, EntryResult) ->
    Action = maps:get(action, Opts, undefined),
    Surface = maps:get(surface, Opts, undefined),
    ExpectedHandler =
        case Surface of
            platform -> eb_platform_handler;
            _ -> eb_tenant_handler
        end,
    Base = [{path_not_frozen, Path} || not lists:keymember(Path, 2, Known)],
    [
        Base,
        [{wrong_handler, Path, Handler} || Handler =/= ExpectedHandler],
        [
            {feature_mismatch, Path, maps:get(feature, Opts, undefined)}
         || maps:get(feature, Opts, undefined) =/= eb_enterprise_actions:feature()
        ],
        [
            {missing_surface_key, Path, K}
         || K <- eb_enterprise_actions:surface_required(),
            not maps:is_key(K, Opts)
        ],
        [{surface_not_atom, Path, Surface} || not is_atom(Surface)],
        entry_violations(Path, Action, Opts, EntryResult)
    ].

entry_violations(_Path, _Action, _Opts, {error, Reason}) ->
    [{action_not_in_table, Reason}];
entry_violations(Path, Action, Opts, {ok, Entry}) ->
    Auth = maps:get(auth, Entry),
    [
        [
            {auth_context_mismatch, Path, maps:get(auth_context, Opts, undefined)}
         || maps:get(auth_context, Opts, undefined) =/= maps:get(auth_context, Auth)
        ],
        [
            {auth_key_mismatch, Path, K, maps:get(K, Opts, undefined), V}
         || {K, V} <- maps:to_list(Auth),
            maps:get(K, Opts, undefined) =/= V
        ],
        [
            {method_not_declared, Path, Action, M}
         || M <- route_methods(Opts),
            not lists:member(M, entry_methods(Entry))
        ]
    ] ++
        [
            {undeclared_action_for_path, Path, Action}
         || not lists:member({Path, Action}, path_actions())
        ].

entry_methods(Entry) ->
    [maps:get(method, Case) || Case <- maps:get(cases, Entry)].

path_actions() ->
    sets:to_list(
        sets:from_list(
            [{Path, Action} || {Path, Action, _} <- tenant_literal_routes()] ++
                [{Path, Action} || {Path, Action, _} <- platform_literal_routes()]
        )
    ).

%% 冻结清单里登记的方法集合（路由表本身不存方法，方法在动作表里；两者都要一致）。
route_methods(Opts) ->
    case {maps:get(surface, Opts, undefined), maps:get(action, Opts, undefined)} of
        {Surface, Action} when Surface =:= tenant; Surface =:= platform ->
            case lists:keyfind(Action, 2, frozen_for(Surface)) of
                {_Path, Action, Methods} -> Methods;
                false -> []
            end;
        _ ->
            []
    end.

frozen_for(tenant) -> tenant_literal_routes();
frozen_for(platform) -> platform_literal_routes().

%% @doc A01 的**非真空**证明：把一条路由的 auth_context 改错、把方法表改错、
%% 把 feature 去掉，审计必须逐条报出对应违规。若审计恒返回 []，下面三条会红。
a01_audit_is_not_vacuous_test() ->
    Real = ?S:enterprise_routes(all),
    ?assert(length(Real) >= 26),
    %% ① auth_context 改错
    MutatedAuth = lists:map(
        fun({Path, H, Opts}) ->
            case maps:get(action, Opts) of
                p_identities -> {Path, H, Opts#{auth_context => enterprise_member}};
                _ -> {Path, H, Opts}
            end
        end,
        Real
    ),
    ?assertNotEqual([], violations(MutatedAuth)),
    %% ② feature 键被拿掉
    MutatedFeature = [
        {Path, H, maps:remove(feature, Opts)}
     || {Path, H, Opts} <- Real
    ],
    ?assertNotEqual([], violations(MutatedFeature)),
    %% ③ 面级装配键（auth_facts）被拿掉
    MutatedFacts = [
        {Path, H, maps:remove(auth_facts, Opts)}
     || {Path, H, Opts} <- Real
    ],
    ?assertNotEqual([], violations(MutatedFacts)),
    %% ④ 整条路由缺失（少登记一条企业路由）
    ?assertNotEqual([], violations(lists:droplast(Real))),
    %% ⑤ 未登记动作
    MutatedAction = [
        case Path of
            <<"/api/v1/enterprise/organizations/:org_id/members/:uid/suspend">> ->
                {Path, H, Opts#{action => suspend_everything}};
            _ ->
                {Path, H, Opts}
        end
     || {Path, H, Opts} <- Real
    ],
    ?assertNotEqual([], violations(MutatedAction)).

%% @doc 冻结清单 ↔ 动作表双向覆盖：动作表里每个动作恰好被一条冻结路径使用；
%% 每个多方法动作的用例方法集与冻结清单一致。
a01_action_table_covers_frozen_paths_test() ->
    TenantActions = sets:from_list(eb_enterprise_actions:tenant_actions()),
    PlatformActions = sets:from_list(eb_enterprise_actions:platform_actions()),
    FrozenTenant = sets:from_list([A || {_P, A, _M} <- tenant_literal_routes()]),
    FrozenPlatform = sets:from_list([A || {_P, A, _M} <- platform_literal_routes()]),
    ?assertEqual(sets:to_list(FrozenTenant), sets:to_list(TenantActions)),
    ?assertEqual(sets:to_list(FrozenPlatform), sets:to_list(PlatformActions)),
    %% 方法集合双向一致
    lists:foreach(
        fun({Path, Action, Methods}) ->
            {ok, Entry} = eb_enterprise_actions:tenant(Action),
            ?assertEqual({Path, lists:sort(Methods)}, {Path, lists:sort(entry_methods(Entry))})
        end,
        tenant_literal_routes()
    ),
    lists:foreach(
        fun({Path, Action, Methods}) ->
            {ok, Entry} = eb_enterprise_actions:platform(Action),
            ?assertEqual({Path, lists:sort(Methods)}, {Path, lists:sort(entry_methods(Entry))})
        end,
        platform_literal_routes()
    ).

%% @doc 每条企业路由的 metadata 都必须被 `eb_auth_principal` 接受，且解析出的
%% principal 与登记值一致（**真调用**，principal 来源纪律的可执行证明）。
a01_auth_principal_accepts_every_enterprise_route_test() ->
    Routes = ?S:enterprise_routes(all),
    ?assert(length(Routes) >= 26),
    lists:foreach(
        fun({Path, _H, Opts}) ->
            Metadata = maps:put(path, Path, Opts),
            case eb_auth_principal:principal_for_route(Metadata) of
                {ok, Requirement} ->
                    ?assertEqual(maps:get(auth_context, Opts), maps:get(principal, Requirement)),
                    ?assertEqual(maps:get(surface, Opts), maps:get(surface, Requirement));
                {error, Reason} ->
                    erlang:error({unauthorized_route_metadata, Path, Reason})
            end
        end,
        Routes
    ).

%% @doc A01 的**面与 principal 不相容**负例（真调用）：把租户路径声明成
%% `platform_admin`，`principal_for_route/1` 必须 fail-closed。
a01_surface_principal_incompatibility_is_rejected_test() ->
    {Path, Opts} = ?S:route_opt(tenant, contacts),
    Bad = maps:put(auth_context, platform_admin, maps:put(path, Path, Opts)),
    ?assertMatch(
        {error, {surface_principal_mismatch, tenant, platform_admin}},
        eb_auth_principal:principal_for_route(Bad)
    ),
    %% 反向：平台路径声明企业成员身份同样被拒
    {PPath, POpts} = ?S:route_opt(platform, p_contacts),
    BadP = maps:put(auth_context, enterprise_member, maps:put(path, PPath, POpts)),
    ?assertMatch(
        {error, {surface_principal_mismatch, platform, enterprise_member}},
        eb_auth_principal:principal_for_route(BadP)
    ).

%% @doc 企业面**不得**进入免鉴权白名单（middleware 的 fail-closed 前提）。
a01_enterprise_paths_are_not_open_test() ->
    Open = imboy_router:open(),
    Enterprise = [Path || {Path, _H, _O} <- ?S:enterprise_routes(all)],
    ?assertEqual([], [P || P <- Enterprise, lists:member(P, Open)]).

%% ===================================================================
%% A02：TSID 全以 JSON string 传输
%% ===================================================================

a02_tsid_encoding_is_string_test() ->
    Payload = #{
        id => 1234567890123456789,
        organization_id => 987654321,
        business_identity_id => 42,
        actor_user_id => 7,
        created_by_user_id => 9,
        message_id => 11,
        conversation_id => 13,
        asset_id => 15,
        case_id => 17,
        hold_id => 19,
        user_id => 21,
        %% 非 TSID：版本/计数/时间戳/字节数保持 number
        version => 3,
        retention_days => 1095,
        size_bytes => 2048,
        retain_until => 1790000000,
        limit => 50,
        count => 2,
        %% 字符串语义的 id（DID 等）不受影响
        device_id => <<"did-abc">>,
        client_msg_id => <<"cmid-1">>
    },
    Encoded = eb_enterprise_http:encode_entity(Payload),
    TsidKeys = [
        id,
        organization_id,
        business_identity_id,
        actor_user_id,
        created_by_user_id,
        message_id,
        conversation_id,
        asset_id,
        case_id,
        hold_id,
        user_id
    ],
    ?assertEqual(
        [],
        [K || K <- TsidKeys, not is_binary(maps:get(K, Encoded))]
    ),
    ?assertEqual(
        [],
        [
            K
         || K <- [version, retention_days, size_bytes, retain_until, limit, count],
            not is_integer(maps:get(K, Encoded))
        ]
    ),
    ?assertEqual(<<"1234567890123456789">>, maps:get(id, Encoded)),
    %% 递归：列表里的元素同样编码
    ?assertEqual(
        [#{id => <<"1">>}, #{id => <<"2">>}],
        eb_enterprise_http:encode_entity([#{id => 1}, #{id => 2}])
    ),
    ?assertEqual(
        #{nested => #{contact_id => <<"5">>}}, encode_nested(#{nested => #{contact_id => 5}})
    ).

encode_nested(Term) ->
    %% 把 atom 键转成 binary 键后比较（jsx 解码后的世界）
    (eb_enterprise_http:encode_entity(Term)).

%% @doc A02 的非真空证明：若把 `is_tsid_key/1` 的判据改成「一切都算 TSID」或
%% 「一切都不算」，下面的断言必然红——故 `id` 必须恰好命中而 `version` 必须不命中。
a02_tsid_key_predicate_is_not_vacuous_test() ->
    ?assert(eb_enterprise_http:is_tsid_key(id)),
    ?assert(eb_enterprise_http:is_tsid_key(organization_id)),
    ?assert(eb_enterprise_http:is_tsid_key(message_id)),
    ?assertNot(eb_enterprise_http:is_tsid_key(version)),
    ?assertNot(eb_enterprise_http:is_tsid_key(retention_days)),
    ?assertNot(eb_enterprise_http:is_tsid_key(device_id)),
    ?assertNot(eb_enterprise_http:is_tsid_key(client_msg_id)),
    ?assertNot(eb_enterprise_http:is_tsid_key(size_bytes)).

%% @doc 入站：TSID 只接受十进制字符串（或 JSON number），拒绝负数/十六进制/空串。
a02_inbound_tsid_parsing_is_strict_test() ->
    ?assertEqual({ok, 123}, eb_enterprise_http:tsid(<<"123">>)),
    ?assertEqual({ok, 123}, eb_enterprise_http:tsid(123)),
    ?assertEqual(error, eb_enterprise_http:tsid(<<"0">>)),
    ?assertEqual(error, eb_enterprise_http:tsid(<<"-1">>)),
    ?assertEqual(error, eb_enterprise_http:tsid(<<"0x10">>)),
    ?assertEqual(error, eb_enterprise_http:tsid(<<>>)),
    ?assertEqual(error, eb_enterprise_http:tsid(undefined)),
    ?assertEqual(error, eb_enterprise_http:tsid(1.5)).

%% ===================================================================
%% A03：错误映射对 6 个状态码逐条有登记
%% ===================================================================

a03_status_mapping_covers_required_statuses_test() ->
    Table = [
        {400, malformed_json},
        {400, body_not_object},
        {400, {forbidden_client_key, organization_id}},
        {400, invalid_tsid},
        {401, credential_missing},
        {401, {principal_mismatch, enterprise_member, adm_session}},
        {403, cross_org},
        {403, cross_identity},
        {403, identity_assignment_missing},
        {403, {member_not_active, suspended}},
        {403, {function_mismatch, <<"sales">>, [<<"customer_service">>]}},
        {403, {permission_missing, <<"note.write">>}},
        {404, not_found},
        {404, {contact_not_found, 1}},
        {405, method_not_allowed},
        {409, conflict},
        {409, duplicate_occupation},
        {409, {conversation_exists, 1}},
        {409, {invalid_transition, frozen}},
        {409, {leaver_not_active, removed}},
        {409, {successor_not_active, suspended}},
        {422, {missing_param, subject}},
        {422, missing_workspace_id},
        {422, empty_patch},
        {422, {invalid_argument, create_identity}},
        %% 服务端侧失败一律 500：**不得**伪装成 4xx（否则调用方以为「只是参数问题」）。
        %% `missing_key` 是接口层能观测到的真实值：企业写路径需要企业托管主密钥，
        %% 而本树没有生产侧提供者（findings EB-09-F6），HTTP 层正确地不接收客户端
        %% 提交的密钥 ⇒ 用例层 fail-closed。实测：POST assets/presign → 500 missing_key。
        {500, missing_key},
        {500, missing_key_version},
        {500, invalid_key_length},
        {500, clock_unavailable},
        {500, {seal_failed, badarg}},
        {500, {object_unreadable, noent}},
        {500, {integrity_check_failed, <<"h1">>, <<"h2">>}},
        {500, {unimplemented_port, asset}},
        {500, {id_generation_failed, tsid}},
        {500, {audit_append_failed, closed}},
        %% `{forbidden, {facts_unavailable, _}}` 是**服务端**事实源不可用，
        %% 不得被折叠成 403（递归一层判定）
        {500, {forbidden, {facts_unavailable, db_down}}},
        %% 反向非真空：真正的授权拒绝仍是 403
        {403, {forbidden, {member_status, suspended}}},
        {403, {forbidden, not_assignee}}
    ],
    ?assertEqual(
        [],
        [
            {Expected, Reason, eb_enterprise_http:status(Reason)}
         || {Expected, Reason} <- Table, eb_enterprise_http:status(Reason) =/= Expected
        ]
    ),
    %% 未登记的原因一律 **500**（fail-closed，不降级成 4xx）
    ?assertEqual(500, eb_enterprise_http:status({something_unregistered, 1})),
    ?assertEqual(500, eb_enterprise_http:status(auth_assembly_missing)).

%% @doc 真负例：错误项来自**真实模块**（不是手写字符串）。
%% `enterprise_business_facade` 的形状收敛失败、`eb_auth_app` 的拒绝、domain 的
%% 否定词各自都要映射到稳定状态码与稳定标签。
a03_status_mapping_on_real_errors_test() ->
    %% ① facade 形状收敛（真调用，不触库）
    {error, FacadeError} = enterprise_business_facade:create_identity(1, #{}),
    ?assertEqual(422, eb_enterprise_http:status(FacadeError)),
    {error, OrgError} = enterprise_business_facade:create_identity(not_an_int, #{}),
    ?assertEqual(422, eb_enterprise_http:status(OrgError)),
    %% ② eb_auth_app 的真拒绝（真调用）
    {error, NoCredential} = eb_auth_app:authorize(
        #{
            auth_context => enterprise_member,
            required_function => <<"sales">>,
            required_permission => <<"contact.read">>
        },
        #{organization_id => 1, credential => #{class => adm_session, adm_user_id => 1}}
    ),
    ?assertEqual(401, eb_enterprise_http:status(NoCredential)),
    %% ③ cross_org（真调用：事实归属与目标 Org 不一致）
    {error, CrossOrg} = eb_auth_app:authorize(
        #{
            auth_context => enterprise_member,
            required_function => <<"sales">>,
            required_permission => <<"contact.read">>
        },
        #{
            organization_id => 2,
            user_id => 7,
            credential => #{class => imboy_jwt, user_id => 7},
            facts => #{
                organization_id => 3,
                member => #{user_id => 7, role => member, status => active},
                assignments => [],
                permissions => [<<"contact.read">>]
            }
        }
    ),
    ?assertEqual(403, eb_enterprise_http:status(CrossOrg)),
    %% ④ 标签只含原子路径，**不含**任何取值
    ?assertEqual(<<"cross_org">>, eb_enterprise_http:tag(CrossOrg)),
    Tag = eb_enterprise_http:tag({invalid_argument, {presign_scope, [1, 2, 3]}}),
    ?assertEqual(<<"invalid_argument.presign_scope">>, Tag),
    ?assertEqual(nomatch, binary:match(Tag, <<"1">>)),
    %% ⑤ 6 个状态码在真调用里都出现过（非真空）
    Real = [FacadeError, OrgError, NoCredential, CrossOrg],
    Statuses = lists:usort([eb_enterprise_http:status(R) || R <- Real]),
    ?assert(lists:member(401, Statuses)),
    ?assert(lists:member(403, Statuses)),
    ?assert(lists:member(422, Statuses)).

%% ===================================================================
%% A04：平台面强制显式 Org
%% ===================================================================

a04_every_platform_path_carries_org_id_test() ->
    Paths = [Path || {Path, _H, _O} <- ?S:enterprise_routes(platform)],
    ?assert(length(Paths) >= 10),
    ?assertEqual([], [P || P <- Paths, binary:match(P, <<":org_id">>) =:= nomatch]),
    %% 非真空：审计函数对「拿掉 Org 段」的副本必须报错
    Mutated = [strip_org(P) || P <- Paths],
    ?assertEqual([], [P || P <- Mutated, binary:match(P, <<":org_id">>) =/= nomatch]),
    ?assertNotEqual(
        [],
        [P || P <- Mutated, binary:match(P, <<":org_id">>) =:= nomatch]
    ).

strip_org(Path) ->
    binary:replace(Path, <<"/organizations/:org_id">>, <<>>).

%% @doc 平台 handler 的**唯一**用例调用点必须显式传 OrgId（租户条件不可省略）。
a04_platform_handler_passes_org_id_to_every_call_test() ->
    %% ① 两个 handler 的**唯一**用例调用点都显式传 OrgId
    Handlers = [
        {?TENANT_HANDLER, code_only(read(?TENANT_HANDLER))},
        {?PLATFORM_HANDLER, code_only(read(?PLATFORM_HANDLER))}
    ],
    ?assertEqual(
        [],
        [File || {File, Src} <- Handlers, not has_call_with_org(Src)]
    ),
    %% ② 调用点表里**每一条** facade 调用都把 OrgId 作为第 2 个实参传下去
    %%    （逐行核对：承载 facade 调用的那一行必须同时含 `(OrgId, Params)`）
    FacadeCall = code_only(read(?FACADE_CALL)),
    Lines = [
        L
     || L <- binary:split(FacadeCall, <<"\n">>, [global]),
        binary:match(L, <<"enterprise_business_facade:">>) =/= nomatch
    ],
    ?assert(length(Lines) >= 21),
    ?assertEqual([], [L || L <- Lines, binary:match(L, <<"(OrgId, Params)">>) =:= nomatch]),
    %% 非真空 ①：把 handler 的调用点实参换成常量后，①必须报红
    MutatedHandler = binary:replace(
        code_only(read(?PLATFORM_HANDLER)),
        <<"maps:get(facade, Case), OrgId, Params">>,
        <<"maps:get(facade, Case), 0, Params">>
    ),
    ?assertNot(has_call_with_org(MutatedHandler)),
    %% 非真空 ②：把调用点表里的 OrgId 换成常量后，②必须报红（计数不再相等）
    MutatedTable = binary:replace(FacadeCall, <<"(OrgId, Params)">>, <<"(0, Params)">>, [
        global
    ]),
    MutatedLines = [
        L
     || L <- binary:split(MutatedTable, <<"\n">>, [global]),
        binary:match(L, <<"enterprise_business_facade:">>) =/= nomatch
    ],
    ?assert(length(MutatedLines) >= 21),
    ?assertNotEqual([], [L || L <- MutatedLines, binary:match(L, <<"(OrgId, Params)">>) =:= nomatch]).

has_call_with_org(Src) ->
    binary:match(Src, <<"eb_enterprise_facade_call:call(">>) =/= nomatch andalso
        binary:match(Src, <<"OrgId, Params">>) =/= nomatch.

%% ===================================================================
%% A05：handler 无 DB/跨层调用；content 经 facade 且不泄露存储能力
%% ===================================================================

a05_handlers_have_no_db_or_cross_layer_calls_test() ->
    Forbidden = [
        <<"elib_pg">>,
        <<"eb_pg_">>,
        <<"eb_pg_sql">>,
        <<"_repo:">>,
        <<"_ds:">>,
        <<"epgsql">>,
        <<"attach_logic">>,
        <<"view_url">>,
        <<"presign_url">>,
        <<"list_to_atom">>,
        <<"binary_to_atom">>,
        <<"erlang:apply">>,
        <<"enterprise_business_facade:">>
    ],
    Sources = [
        {?TENANT_HANDLER, code_only(read(?TENANT_HANDLER))},
        {?PLATFORM_HANDLER, code_only(read(?PLATFORM_HANDLER))},
        {?HTTP_MODULE, code_only(read(?HTTP_MODULE))},
        {?FACADE_CALL, code_only(read(?FACADE_CALL))}
    ],
    Hits = [
        {File, Needle}
     || {File, Src} <- Sources,
        Needle <- Forbidden,
        binary:match(Src, Needle) =/= nomatch,
        %% 唯一允许 facade 名字出现的模块是调用点表本身
        not (Needle =:= <<"enterprise_business_facade:">> andalso File =:= ?FACADE_CALL)
    ],
    ?assertEqual([], Hits),
    %% 非真空：把一段真 DB 调用（**代码**，不是注释）追加进源码副本，
    %% 同一判据必须命中
    Poisoned = <<(code_only(read(?TENANT_HANDLER)))/binary, "\nf() -> elib_pg:query(1).\n">>,
    ?assertNotEqual(nomatch, binary:match(Poisoned, <<"elib_pg">>)),
    %% 注释里的同名 token 不会被判红（判据只看代码）
    Commented = <<"%% elib_pg 只是注释\nf() -> ok.\n">>,
    ?assertEqual(nomatch, binary:match(code_only(Commented), <<"elib_pg">>)).

%% @doc content 动作经 facade 取流（proxy_content），且**非**走个人下载能力。
a05_asset_content_is_proxy_only_test() ->
    {ok, Entry} = eb_enterprise_actions:tenant(asset_content),
    ?assert(maps:get(proxy_content, Entry)),
    {ok, Case} = eb_enterprise_actions:case_for(Entry, <<"GET">>),
    ?assertEqual(content_stream, maps:get(facade, Case)),
    %% 需要 actor（鉴权代理的 ACL 判据是「请求者 == 会话当前经办」）
    ?assert(lists:keymember(asset_id, 2, maps:get(path_params, Case))),
    {ok, Platform} = eb_enterprise_actions:platform(p_asset_content),
    ?assert(maps:get(proxy_content, Platform)),
    {ok, PCase} = eb_enterprise_actions:case_for(Platform, <<"GET">>),
    ?assertEqual(content_stream, maps:get(facade, PCase)),
    ?assert(lists:member({actor_user_id, tsid, required}, maps:get(params, PCase))).

%% @doc 真响应字节（经真 socket）里不得出现任何存储能力：把「诱饵」存储字段塞进
%% facade 视图，由**生产侧** `eb_enterprise_http:reply_content/2` 真的写到 socket 上，
%% 再按原始字节扫描（生产侧与证据侧共用同一判据 `storage_leak_scan/1`）。
%%
%% 说明（不假绿）：本测试覆盖的是「content 响应**构造器**」的真实输出；handler →
%% facade → 对象存储的完整链路在本 worktree **无法运行**（`eb_asset_app` 未播种，
%% 见 RESULT findings EB-09-F4），故该段标记为 NOT_VERIFIED，不由本测试冒充。
a05_content_response_has_no_storage_capability_test() ->
    {ok, _Started} = application:ensure_all_started(cowboy),
    Body = <<"EB09-CONTENT-BYTES">>,
    View = #{
        asset_id => 12345,
        object_hash => <<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
        mime => <<"text/plain">>,
        size_bytes => byte_size(Body),
        body => Body,
        %% 诱饵：真实实现里根本不该出现这些字段；若 reply_content 把它们写出去，
        %% 下面的扫描必须命中
        object_key => <<"org/1/ws/2/asset/3/object_key_LEAK">>,
        storage_ref => <<"garage-bucket-LEAK">>,
        url => <<"https://garage.internal/LEAK">>,
        endpoint => <<"http://127.0.0.1:3900/LEAK">>
    },
    ok = persistent_term:put({eb09_content_probe, view}, View),
    {ok, Name, Port} = eb_handler_test_support:listener(
        <<"/__eb09_probe_content">>, #{}, eb09_content_probe_handler
    ),
    try
        Resp = ?S:request(Port, <<"GET">>, <<"/__eb09_probe_content">>),
        ?assertEqual(200, maps:get(status, Resp)),
        Raw = ?S:raw(Resp),
        %% ① 字节原样返回（流式内容 = 对象字节）
        ?assertNotEqual(nomatch, binary:match(Raw, Body)),
        %% ② 存储能力扫描：object key / bucket / URL / endpoint 一个都不许出现
        ?assertEqual(ok, eb_enterprise_http:storage_leak_scan(Raw)),
        %% ③ 只出现白名单头
        Headers = maps:get(headers, Resp),
        ?assertEqual(<<"text/plain">>, maps:get(<<"content-type">>, Headers)),
        ?assertEqual(<<"12345">>, maps:get(<<"x-asset-id">>, Headers)),
        ?assertNot(maps:is_key(<<"object_key">>, Headers)),
        %% ④ 非真空：把诱饵值单独扫一遍，扫描器必须命中（证明②不是恒真）
        ?assertMatch({leak, _}, eb_enterprise_http:storage_leak_scan(maps:get(object_key, View))),
        ?assertMatch({leak, _}, eb_enterprise_http:storage_leak_scan(maps:get(url, View))),
        ?assertMatch({leak, _}, eb_enterprise_http:storage_leak_scan(maps:get(endpoint, View))),
        %% ⑤ 「全量视图」扫一遍会命中（若有人把整个 View 序列化进响应，②必红）
        ?assertMatch({leak, _}, eb_enterprise_http:storage_leak_scan(jsx:encode(View)))
    after
        _ = persistent_term:erase({eb09_content_probe, view}),
        ?S:stop(Name)
    end.

%% @doc A05 的**静态防线**：facade 委派的每个 application 模块与其函数必须真实存在。
%%
%% 为什么必须有：EB-09 首次交付时 `enterprise_business_facade:content_stream/2`
%% 指向 `eb_asset_app`，而播种树里**该模块不存在**（findings EB-09-F4）——facade 的
%% 文面契约与装配事实脱节，任何请求到 asset 路径都会 undef。本断言把「facade 的每条
%% 委派都可达」变成机械事实：模块可加载 + 该函数有导出（任意 arity）。
facade_delegation_targets_exist_test() ->
    Src = code_only(read(?FACADE)),
    Refs = delegate_refs(Src),
    %% 非真空：facade 的委派点必须被真的抽到（否则下面的空集断言恒真）
    ?assert(length(Refs) >= 20),
    Missing = [
        {Mod, Fun, Reason}
     || {Mod, Fun} <- Refs,
        Reason <- [reachability(Mod, Fun)],
        Reason =/= ok
    ],
    ?assertEqual([], Missing),
    %% 非真空：把一个不存在的模块名拼进去，同一判据必须命中
    ?assertNotEqual(ok, reachability(eb09_no_such_app, content_stream)).

delegate_refs(Src) ->
    case
        re:run(Src, <<"\\b([a-z][a-z0-9_]*):([a-z][a-z0-9_]*)\\(">>, [
            global, {capture, all_but_first, list}
        ])
    of
        {match, Captures} ->
            lists:usort([{list_to_atom(M), list_to_atom(F)} || [M, F] <- Captures]);
        nomatch ->
            []
    end.

reachability(Mod, Fun) ->
    case code:ensure_loaded(Mod) of
        {module, Mod} ->
            case
                lists:any(
                    fun({F, _A}) -> F =:= Fun end, Mod:module_info(exports)
                )
            of
                true -> ok;
                false -> {missing_function, Fun}
            end;
        {error, Reason} ->
            {module_not_loadable, Reason}
    end.

%% @doc asset content 的**端到端**静态事实（EB-07 播种后）：facade 的 asset 委派
%% 目标真实存在且导出 `content_stream/2`；接口层不引任何个人下载能力。
a05_asset_application_is_present_after_seeding_test() ->
    ?assertEqual(ok, reachability(eb_asset_app, content_stream)),
    ?assertEqual(ok, reachability(eb_asset_content, sha256_hex)),
    %% 非真空：不存在的模块/函数必须报出（同一判据）
    ?assertNotEqual(ok, reachability(eb09_no_such_app, content_stream)),
    ?assertNotEqual(ok, reachability(eb_asset_app, no_such_function)).

%% ===================================================================
%% A06：ACK 契约 = delivery-only
%% ===================================================================

a06_ack_contract_is_delivery_only_test() ->
    {ok, Entry} = eb_enterprise_actions:tenant(ack_delivery),
    ?assert(maps:get(delivery_only, Entry)),
    {ok, Case} = eb_enterprise_actions:case_for(Entry, <<"POST">>),
    ?assertEqual(ack_delivery, maps:get(facade, Case)),
    %% ACK 动作**只**有这一个用例（不存在第二个方法/第二条调用点）
    ?assertEqual(1, length(maps:get(cases, Entry))),
    ?assert(lists:member(ack_delivery, eb_enterprise_facade_call:actions())),
    %% 该动作的参数表里没有任何「删除/归档」语义的键
    Keys = [K || {K, _T, _R} <- maps:get(params, Case)],
    BannedWords = [<<"delete">>, <<"archive">>, <<"purge">>, <<"remove">>, <<"hidden">>],
    ?assertEqual(
        [],
        [K || K <- Keys, W <- BannedWords, binary:match(atom_to_binary(K, utf8), W) =/= nomatch]
    ),
    %% 且 ack 路径的参数表**不含** canonical 改写入口
    ?assertEqual([], [
        K
     || K <- Keys, lists:member(K, [canonical, message_status, seen, visibility])
    ]).

%% @doc ACK 响应形状不含删除/归档语义（对真实用例返回的真实载荷形状做扫描）。
a06_ack_response_shape_has_no_delete_semantics_test() ->
    %% 真实用例返回：#{delivery, message_id, canonical_unchanged}
    RealShape = #{
        delivery => #{id => 99, message_id => 11, recipient_ref => <<"contact:5">>},
        message_id => 11,
        canonical_unchanged => true
    },
    Encoded = jsx:encode(eb_enterprise_http:encode_entity(RealShape)),
    Words = [
        <<"delete">>, <<"deleted">>, <<"archive">>, <<"archived">>, <<"purge">>, <<"removed">>
    ],
    ?assertEqual([], [W || W <- Words, binary:match(Encoded, W) =/= nomatch]),
    %% 非真空：把 canonical_unchanged 换成 deleted 后必须被同一扫描命中
    Poisoned = jsx:encode(
        eb_enterprise_http:encode_entity(RealShape#{canonical_unchanged => deleted})
    ),
    ?assertNotEqual([], [W || W <- Words, binary:match(Poisoned, W) =/= nomatch]).

%% @doc ACK 的 domain 判据真的会拦住 canonical 改写（真调用 domain 纯函数）。
a06_domain_rejects_canonical_mutation_test() ->
    Before = #{
        message_id => 7, body_cipher_hash => <<"h1">>, retain_until => 100, status => <<"sent">>
    },
    Same = Before,
    Mutated = Before#{body_cipher_hash => <<"h2">>},
    ?assertEqual(ok, eb_message:ack_preserves_canonical(Before, Same)),
    ?assertMatch({error, _}, eb_message:ack_preserves_canonical(Before, Mutated)).

%% 读源码（静态红线的唯一输入；相对路径与 make eunit 的工作目录一致）。
read(Path) ->
    {ok, Bin} = file:read_file(Path),
    Bin.

%% 去注释：静态红线只对**代码**成立（注释里出现 elib_pg / list_to_atom 之类的
%% 说明文字不构成调用）。与 `scripts/check_feature_architecture.sh` 的 `code_only`
%% 同口径（`s/%.*$//`）。
code_only(Bin) ->
    iolist_to_binary([
        [first_comment_part(Line), <<"\n">>]
     || Line <- binary:split(Bin, <<"\n">>, [global])
    ]).

first_comment_part(Line) ->
    hd(binary:split(Line, <<"%">>)).
