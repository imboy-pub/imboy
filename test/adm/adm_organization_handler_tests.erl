-module(adm_organization_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

%%% Platform Admin Organization 治理 API（TASK_ID=ORG-ADM-ORG-API）
%%%
%%% 覆盖面（合同 20 条端点 = 18 条路由，invitations/departments 两条同路径
%%% 承载 GET 列表 + POST create 双语义）：
%%%   * route contract：18 条 /api/adm/organizations* 路由（20 端点）注册 +
%%%     handler/action 正确；固定路径先于通配；organizations 面不暴露 workspace
%%%     写路由（平台对 Workspace 只读关系事实）；方法分派 405；
%%%   * 旅程正例（marker 库真库，env 前缀 ADM_ORG_INTTEST）：查询+详情 /
%%%     archive→restore / owner transfer / member suspend→restore→remove /
%%%     invitation create→cancel / department create→rename→move→archive /
%%%     workspace 只读；
%%%   * 权限矩阵：read-only 角色（audit_admin 仅 organizations:read）对全部
%%%     mutation=403；read+write 角色读写皆可；仅有 write 无 read → 读 403；
%%%   * 负例：无会话 401（middleware）/ 400 坏参数 / 404 不存在与跨组织 /
%%%     409 stale version、archived 门禁、owner 保护 / 幂等 unchanged；
%%%   * 审计：mutation 后 admin_operation_logs 出现 organization_* 记录。
%%%
%%% 库供给契约（inttest_marker_db 配方）：一次性 marker 库全链迁移，
%%% 任一环境/迁移失败显式 error，无 skip 分支；erlfmt 口径与仓内一致。

-define(WRITE_UID, 9101).
-define(WO_READ_UID, 9102).
-define(RO_UID, 9103).

%% 合同许可的唯一 workspace 相关 organizations 路由（GET 只读关系）：
%% 除它之外 organizations 面不得出现任何 workspace 路径（写治理在 /api/adm/workspace/*）。
-define(READONLY_WORKSPACES_PATH, <<"/api/adm/organizations/:organization_id/workspaces">>).

%% ------------------------------------------------------------------
%% Mock 套装（照 adm_workspace_handler_tests 口径）
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
            (?WO_READ_UID, <<"id,role_id">>, _Key) ->
                #{<<"id">> => ?WO_READ_UID, <<"role_id">> => [7]};
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
            (7) ->
                {<<"ops_no_read">>, [<<"organizations:write">>], []};
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

%% ------------------------------------------------------------------
%% 调用 helper
%% ------------------------------------------------------------------

%% @doc 以指定管理员身份调用 handler；RespReq 捕获 elib_response 假体写入的
%% response_status / payload / error_msg。
call(AdmUid, Action, Method, Bindings, Body) ->
    Req0 = #{method => Method, bindings => Bindings, body => Body},
    {ok, RespReq, _} =
        adm_organization_handler:init(Req0, #{action => Action, adm_user_id => AdmUid}),
    RespReq.

status_of(RespReq) -> maps:get(response_status, RespReq, undefined).

payload_of(RespReq) -> maps:get(payload, RespReq, undefined).

bindings(OrgId) -> #{organization_id => integer_to_binary(OrgId)}.

org_binding(OrgId, Key, Value) ->
    #{
        organization_id => integer_to_binary(OrgId),
        Key => integer_to_binary(Value)
    }.

%% ===================================================================
%% 组 A：路由契约（不触库）
%% ===================================================================

adm_org_routes_registered_test_() ->
    {"adm organizations 面 18 条路由注册且 handler/action 正确",
        ?_assertEqual(
            [], lists:filter(fun(Route) -> not route_registered(Route) end, expected_routes())
        )}.

adm_org_fixed_paths_before_wildcard_test_() ->
    {"固定路径 /api/adm/organizations 先于 :organization_id 通配（cowboy 顺序匹配）",
        ?_assert(
            index_of(<<"/api/adm/organizations">>, all_route_paths()) <
                index_of(<<"/api/adm/organizations/:organization_id">>, all_route_paths())
        )}.

adm_org_no_workspace_write_route_test_() ->
    {
        "organizations 面不暴露 workspace 写路由（workspace 只读关系事实；workspace\n"
        "      写治理仍在 /api/adm/workspace/*）",
        ?_assertEqual(
            [],
            [
                P
             || P <- all_route_paths(),
                binary:match(P, <<"/api/adm/organizations/">>) =/= nomatch,
                binary:match(P, <<"workspace">>) =/= nomatch,
                P =/= ?READONLY_WORKSPACES_PATH
            ]
        )
    }.

adm_org_handler_module_exists_test_() ->
    {"handler 与平台 logic 模块可加载",
        ?_assertMatch(
            [{module, adm_organization_handler}, {module, organization_admin_logic}],
            [
                code:ensure_loaded(adm_organization_handler),
                code:ensure_loaded(organization_admin_logic)
            ]
        )}.

adm_org_method_dispatch_test_() ->
    {"方法分派：读端点拒 POST=405；invitations/departments 同路径 GET/POST 双语义",
        {foreach, fun mocks_on/0, fun(_S) -> mocks_off() end, [
            fun(_S) ->
                {"list 拒 POST → 405", fun() ->
                    ?assertEqual(405, status_of(call(?WRITE_UID, list, <<"POST">>, #{}, <<>>)))
                end}
            end,
            fun(_S) ->
                {"show 拒 PUT → 405", fun() ->
                    ?assertEqual(
                        405, status_of(call(?WRITE_UID, show, <<"PUT">>, bindings(1), <<>>))
                    )
                end}
            end
        ]}}.

expected_routes() ->
    H = adm_organization_handler,
    A = fun(Action) -> #{action => Action} end,
    [
        {<<"/api/adm/organizations">>, H, A(list)},
        {<<"/api/adm/organizations/:organization_id">>, H, A(show)},
        {<<"/api/adm/organizations/:organization_id/members">>, H, A(members)},
        {<<"/api/adm/organizations/:organization_id/invitations">>, H, A(invitations)},
        {<<"/api/adm/organizations/:organization_id/departments">>, H, A(departments)},
        {<<"/api/adm/organizations/:organization_id/workspaces">>, H, A(workspaces)},
        {<<"/api/adm/organizations/:organization_id/archive">>, H, A(org_archive)},
        {<<"/api/adm/organizations/:organization_id/restore">>, H, A(org_restore)},
        {<<"/api/adm/organizations/:organization_id/owner-transfer">>, H, A(owner_transfer)},
        {<<"/api/adm/organizations/:organization_id/members/:user_id/suspend">>, H,
            A(member_suspend)},
        {<<"/api/adm/organizations/:organization_id/members/:user_id/restore">>, H,
            A(member_restore)},
        {<<"/api/adm/organizations/:organization_id/members/:user_id/remove">>, H,
            A(member_remove)},
        {<<"/api/adm/organizations/:organization_id/invitations/:invitation_id/cancel">>, H,
            A(invitation_cancel)},
        {<<"/api/adm/organizations/:organization_id/departments/:department_id/rename">>, H,
            A(department_rename)},
        {<<"/api/adm/organizations/:organization_id/departments/:department_id/move">>, H,
            A(department_move)},
        {<<"/api/adm/organizations/:organization_id/departments/:department_id/archive">>, H,
            A(department_archive)}
    ].

route_registered(Route) ->
    lists:member(Route, all_routes()).

all_routes() ->
    lists:flatmap(
        fun({_Host, Routes}) ->
            [{unicode:characters_to_binary(P), H, S} || {P, H, S} <- Routes]
        end,
        imboy_router:get_routes()
    ).

all_route_paths() ->
    [P || {P, _H, _S} <- all_routes()].

index_of(Path, Paths) ->
    index_of(Path, Paths, 0).

index_of(_Path, [], _I) -> not_found;
index_of(Path, [Path | _], I) -> I;
index_of(Path, [_ | Rest], I) -> index_of(Path, Rest, I + 1).

%% ===================================================================
%% 组 B：marker 库真库旅程（ADM_ORG_INTTEST）
%% ===================================================================

adm_org_journeys_test_() ->
    {timeout, 900,
        {setup, fun setup_db/0, fun close_db/1, fun(Db) ->
            Conn = maps:get(conn, Db),
            {foreach, fun mocks_on/0, fun(_S) -> mocks_off() end, [
                fun(_S) -> journeys_tests(Conn) end,
                fun(_S) -> permission_matrix_tests(Conn) end,
                fun(_S) -> negative_tests(Conn) end
            ]}
        end}}.

setup_db() ->
    %% 纯套件（不启动 imboy app）：审计 insert（adm_operation_log_ds 用
    %% elib_tsid:generate(admin_op_log)）与邀请 ID（elib_tsid:generate/0）
    %% 都需要 TSID 生成器就绪——镜像 workspace_archive_tests 的 init+register
    %% 幂等配方，缺了它审计行永远写不进（handler 的 audit catch 会静默吞掉）。
    _ = (catch elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3})),
    ok = elib_tsid:register(admin_op_log),
    State =
        inttest_marker_db:provision(#{
            env_prefix => <<"ADM_ORG_INTTEST">>,
            connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
        }),
    {ok, _} = application:ensure_all_started(pooler),
    #{server := Server, db := Db} = State,
    case pooler:new_pool(pgsql_pool_conf(Server, Db)) of
        {ok, _Pid} ->
            State;
        {error, {already_started, _}} ->
            %% 共享 VM 里 pgsql 池已指向他库：本套件必须单跑（loudly fail，
            %% 不静默复用他库连接——agent_preflight_facts_pg_tests 同款语义）
            erlang:error({adm_org_pg_pool_conflict, 'pgsql'})
    end.

%% epgsql connect 参数原样透传（host/user/password 可能是 list 或 binary，
%% 与 agent_preflight_facts_pg_tests:pool_conf 同形态；不做类型转换）
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
    try pooler:stop_pool(pgsql) of
        _ -> ok
    catch
        _:_ -> ok
    end,
    inttest_marker_db:release(State).

mocks_on() ->
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

%% ------------------------------------------------------------------
%% 旅程正例
%% ------------------------------------------------------------------

journeys_tests(Conn) ->
    [
        {"旅程1 查询+详情：分页列表命中 + 详情 owner/计数 + TSID string 下发", fun() ->
            journey_list_and_detail(Conn)
        end},
        {"旅程2 archive→restore：状态落库 + 幂等重放 changed=false", fun() ->
            journey_archive_restore(Conn)
        end},
        {"旅程3 owner transfer：换人 + previous_owner + 自转移400/非成员409", fun() ->
            journey_owner_transfer(Conn)
        end},
        {"旅程4 member suspend→restore→remove + owner 保护 409", fun() ->
            journey_member_lifecycle(Conn)
        end},
        {"旅程5 invitation create→cancel→再 cancel 幂等 + token 只出现一次 + 跨组织 404", fun() ->
            journey_invitation(Conn)
        end},
        {"旅程6 department create→rename(CAS 200/409)→move→archive", fun() ->
            journey_department(Conn)
        end},
        {"旅程7 workspace 只读关系事实：列表返回 org 下 workspace", fun() -> journey_workspace_readonly(Conn) end},
        {"旅程8 审计：admin_operation_logs 出现 organization_archive 记录", fun() ->
            journey_audit_row(Conn)
        end}
    ].

journey_list_and_detail(Conn) ->
    Scope = seed_org(Conn, <<"list">>),
    OrgId = maps:get(org_id, Scope),
    %% 列表（keyword 按名称命中）
    RespReq = call(?WRITE_UID, list, <<"GET">>, #{}, <<>>),
    ?assertEqual(200, status_of(RespReq)),
    Page = payload_of(RespReq),
    List = maps:get(list, Page),
    Hit = [Row || Row <- List, maps:get(<<"id">>, Row) =:= integer_to_binary(OrgId)],
    ?assert(length(Hit) >= 1),
    Row = hd(Hit),
    ?assert(is_binary(maps:get(<<"id">>, Row))),
    ?assert(is_binary(maps:get(<<"owner_id">>, Row))),
    %% 详情
    RespReq2 = call(?WRITE_UID, show, <<"GET">>, bindings(OrgId), <<>>),
    ?assertEqual(200, status_of(RespReq2)),
    Detail = payload_of(RespReq2),
    ?assertEqual(integer_to_binary(OrgId), maps:get(<<"id">>, Detail)),
    ?assert(is_integer(maps:get(<<"member_count">>, Detail))),
    ?assert(is_integer(maps:get(<<"workspace_count">>, Detail))).

journey_archive_restore(Conn) ->
    Scope = seed_org(Conn, <<"arch">>),
    OrgId = maps:get(org_id, Scope),
    %% archive
    RespReq = call(?WRITE_UID, org_archive, <<"POST">>, bindings(OrgId), <<>>),
    ?assertEqual(200, status_of(RespReq)),
    ?assertMatch(#{<<"status">> := <<"archived">>, <<"changed">> := true}, payload_of(RespReq)),
    ?assertMatch({ok, [#{<<"status">> := <<"archived">>}]}, q(Conn, org_status_sql(), [OrgId])),
    %% 幂等重放：状态未变 changed=false
    RespReq2 = call(?WRITE_UID, org_archive, <<"POST">>, bindings(OrgId), <<>>),
    ?assertMatch(#{<<"changed">> := false}, payload_of(RespReq2)),
    %% restore
    RespReq3 = call(?WRITE_UID, org_restore, <<"POST">>, bindings(OrgId), <<>>),
    ?assertEqual(200, status_of(RespReq3)),
    ?assertMatch(#{<<"status">> := <<"active">>, <<"changed">> := true}, payload_of(RespReq3)).

journey_owner_transfer(Conn) ->
    Scope = seed_org(Conn, <<"tr">>),
    OrgId = maps:get(org_id, Scope),
    Owner = maps:get(owner, Scope),
    Admin = maps:get(admin, Scope),
    %% 正例：transfer 给 admin
    RespReq =
        call(
            ?WRITE_UID,
            owner_transfer,
            <<"POST">>,
            bindings(OrgId),
            jsone:encode(#{<<"target_user_id">> => integer_to_binary(Admin)})
        ),
    ?assertEqual(200, status_of(RespReq)),
    Result = payload_of(RespReq),
    ?assertEqual(integer_to_binary(Admin), maps:get(<<"owner_id">>, Result)),
    ?assertEqual(integer_to_binary(Owner), maps:get(<<"previous_owner_id">>, Result)),
    ?assertMatch({ok, [#{<<"owner_id">> := Admin}]}, q(Conn, org_owner_sql(), [OrgId])),
    %% 负例：target = 当前 owner（自转移）→ 400
    RespReq2 =
        call(
            ?WRITE_UID,
            owner_transfer,
            <<"POST">>,
            bindings(OrgId),
            jsone:encode(#{<<"target_user_id">> => integer_to_binary(Admin)})
        ),
    ?assertEqual(400, status_of(RespReq2)),
    %% 负例：target 非成员 → 409
    Outsider = new_id(),
    RespReq3 =
        call(
            ?WRITE_UID,
            owner_transfer,
            <<"POST">>,
            bindings(OrgId),
            jsone:encode(#{<<"target_user_id">> => integer_to_binary(Outsider)})
        ),
    ?assertEqual(409, status_of(RespReq3)),
    %% 负例：400 坏参数（非正整数）
    RespReq4 =
        call(
            ?WRITE_UID,
            owner_transfer,
            <<"POST">>,
            bindings(OrgId),
            jsone:encode(#{<<"target_user_id">> => <<"abc">>})
        ),
    ?assertEqual(400, status_of(RespReq4)).

journey_member_lifecycle(Conn) ->
    Scope = seed_org(Conn, <<"mem">>),
    OrgId = maps:get(org_id, Scope),
    Owner = maps:get(owner, Scope),
    Member = maps:get(member, Scope),
    Bind = fun(Uid) -> org_binding(OrgId, user_id, Uid) end,
    %% suspend
    RespReq = call(?WRITE_UID, member_suspend, <<"POST">>, Bind(Member), <<>>),
    ?assertEqual(200, status_of(RespReq)),
    ?assertMatch(#{<<"status">> := <<"suspended">>}, payload_of(RespReq)),
    ?assertMatch(
        {ok, [#{<<"status">> := <<"suspended">>}]}, q(Conn, member_status_sql(), [OrgId, Member])
    ),
    %% restore
    RespReq2 = call(?WRITE_UID, member_restore, <<"POST">>, Bind(Member), <<>>),
    ?assertEqual(200, status_of(RespReq2)),
    ?assertMatch(#{<<"status">> := <<"active">>}, payload_of(RespReq2)),
    %% remove（active 直接移除）
    RespReq3 = call(?WRITE_UID, member_remove, <<"POST">>, Bind(Member), <<>>),
    ?assertEqual(200, status_of(RespReq3)),
    ?assertMatch(#{<<"status">> := <<"removed">>}, payload_of(RespReq3)),
    %% remove 再 remove → 409（removed 不可再迁移）
    RespReq4 = call(?WRITE_UID, member_remove, <<"POST">>, Bind(Member), <<>>),
    ?assertEqual(409, status_of(RespReq4)),
    %% owner 保护：suspend owner → 409
    RespReq5 = call(?WRITE_UID, member_suspend, <<"POST">>, Bind(Owner), <<>>),
    ?assertEqual(409, status_of(RespReq5)).

journey_invitation(Conn) ->
    Scope = seed_org(Conn, <<"inv">>),
    OrgId = maps:get(org_id, Scope),
    Target = new_id(),
    ok = exec(
        Conn,
        <<
            "INSERT INTO \"user\"(id,password,account,reg_ip,reg_cosv)"
            " VALUES ($1,'x',$2,'127.0.0.1','x')"
        >>,
        [Target, account(Target)]
    ),
    %% create：token 明文只在响应出现一次
    RespReq =
        call(
            ?WRITE_UID,
            invitations,
            <<"POST">>,
            bindings(OrgId),
            jsone:encode(#{<<"target_user_id">> => integer_to_binary(Target)})
        ),
    ?assertEqual(200, status_of(RespReq)),
    View = payload_of(RespReq),
    InvitationId = maps:get(<<"invitation_id">>, View),
    ?assert(is_binary(InvitationId)),
    Token = maps:get(<<"token">>, View),
    ?assert(is_binary(Token)),
    ?assert(byte_size(Token) > 0),
    ?assertEqual(undefined, maps:get(<<"token_digest">>, View, undefined)),
    ?assertEqual(<<"pending">>, maps:get(<<"status">>, View)),
    ?assertEqual(null, maps:get(<<"invited_by">>, View)),
    InvitationInt = binary_to_integer(InvitationId),
    %% GET 列表可见
    RespReqL = call(?WRITE_UID, invitations, <<"GET">>, bindings(OrgId), <<>>),
    ?assertEqual(200, status_of(RespReqL)),
    Rows = payload_of(RespReqL),
    ?assert(
        lists:any(
            fun(R) -> maps:get(<<"invitation_id">>, R) =:= InvitationId end,
            Rows
        )
    ),
    %% cancel → revoked
    RespReq2 =
        call(
            ?WRITE_UID,
            invitation_cancel,
            <<"POST">>,
            org_binding(OrgId, invitation_id, InvitationInt),
            <<>>
        ),
    ?assertEqual(200, status_of(RespReq2)),
    ?assertEqual(<<"revoked">>, maps:get(<<"status">>, payload_of(RespReq2))),
    %% 再 cancel → 幂等 already_terminal
    RespReq3 =
        call(
            ?WRITE_UID,
            invitation_cancel,
            <<"POST">>,
            org_binding(OrgId, invitation_id, InvitationInt),
            <<>>
        ),
    ?assertEqual(200, status_of(RespReq3)),
    ?assertEqual(true, maps:get(<<"already_terminal">>, payload_of(RespReq3))),
    %% 跨组织：OrgB 路径 cancel OrgA 的邀请 → 404
    OtherScope = seed_org(Conn, <<"invb">>),
    RespReq4 =
        call(
            ?WRITE_UID,
            invitation_cancel,
            <<"POST">>,
            org_binding(maps:get(org_id, OtherScope), invitation_id, InvitationInt),
            <<>>
        ),
    ?assertEqual(404, status_of(RespReq4)),
    ok.

journey_department(Conn) ->
    Scope = seed_org(Conn, <<"dep">>),
    OrgId = maps:get(org_id, Scope),
    %% create
    RespReq =
        call(
            ?WRITE_UID,
            departments,
            <<"POST">>,
            bindings(OrgId),
            jsone:encode(#{<<"name">> => <<"研发部"/utf8>>})
        ),
    ?assertEqual(200, status_of(RespReq)),
    Dept = payload_of(RespReq),
    DeptId = binary_to_integer(maps:get(<<"id">>, Dept)),
    ?assertEqual(1, maps:get(<<"version">>, Dept)),
    %% GET 列表
    RespReqL = call(?WRITE_UID, departments, <<"GET">>, bindings(OrgId), <<>>),
    ?assertEqual(200, status_of(RespReqL)),
    ?assert(
        lists:any(
            fun(R) -> maps:get(<<"id">>, R) =:= maps:get(<<"id">>, Dept) end,
            payload_of(RespReqL)
        )
    ),
    %% rename：expected_version 正确 → 200，version 递增
    RespReq2 =
        call(
            ?WRITE_UID,
            department_rename,
            <<"POST">>,
            org_binding(OrgId, department_id, DeptId),
            jsone:encode(#{<<"name">> => <<"研发部2"/utf8>>, <<"expected_version">> => 1})
        ),
    ?assertEqual(200, status_of(RespReq2)),
    ?assertEqual(2, maps:get(<<"version">>, payload_of(RespReq2))),
    %% rename：expected_version 过期 → 409
    RespReq3 =
        call(
            ?WRITE_UID,
            department_rename,
            <<"POST">>,
            org_binding(OrgId, department_id, DeptId),
            jsone:encode(#{<<"name">> => <<"研发部3"/utf8>>, <<"expected_version">> => 1})
        ),
    ?assertEqual(409, status_of(RespReq3)),
    %% 子部门 + move
    RespReq4 =
        call(
            ?WRITE_UID,
            departments,
            <<"POST">>,
            bindings(OrgId),
            jsone:encode(#{<<"name">> => <<"后端组"/utf8>>, <<"parent_id">> => DeptId})
        ),
    ?assertEqual(200, status_of(RespReq4)),
    ChildId = binary_to_integer(maps:get(<<"id">>, payload_of(RespReq4))),
    RespReq5 =
        call(
            ?WRITE_UID,
            department_move,
            <<"POST">>,
            org_binding(OrgId, department_id, ChildId),
            jsone:encode(#{<<"parent_id">> => null, <<"expected_version">> => 1})
        ),
    ?assertEqual(200, status_of(RespReq5)),
    ?assertEqual(null, maps:get(<<"parent_id">>, payload_of(RespReq5))),
    %% archive（幂等：二次 archive archive_idempotent）
    RespReq6 =
        call(
            ?WRITE_UID,
            department_archive,
            <<"POST">>,
            org_binding(OrgId, department_id, ChildId),
            <<>>
        ),
    ?assertEqual(200, status_of(RespReq6)),
    ?assertEqual(<<"archived">>, maps:get(<<"status">>, payload_of(RespReq6))),
    ok.

journey_workspace_readonly(Conn) ->
    Scope = seed_org(Conn, <<"ws">>),
    OrgId = maps:get(org_id, Scope),
    WsId = maps:get(ws_id, Scope),
    RespReq = call(?WRITE_UID, workspaces, <<"GET">>, bindings(OrgId), <<>>),
    ?assertEqual(200, status_of(RespReq)),
    Page = payload_of(RespReq),
    %% 信封键为 atom（handler normalize_page 原样透传 #{list/page/size/total/...}，
    %% 行内键才是 PG binary）——对齐旅程1 的 maps:get(list, Page) 过绿形态
    ?assertEqual(1, maps:get(total, Page)),
    [Row] = maps:get(list, Page),
    ?assertEqual(integer_to_binary(WsId), maps:get(<<"id">>, Row)),
    %% org 不存在 → 404
    RespReq2 = call(?WRITE_UID, workspaces, <<"GET">>, bindings(new_id()), <<>>),
    ?assertEqual(404, status_of(RespReq2)).

journey_audit_row(Conn) ->
    Scope = seed_org(Conn, <<"aud">>),
    OrgId = maps:get(org_id, Scope),
    _ = call(?WRITE_UID, org_archive, <<"POST">>, bindings(OrgId), <<>>),
    {ok, _, [{Count}]} =
        epgsql:equery(
            Conn,
            <<"SELECT count(*) FROM admin_operation_logs",
                " WHERE target_id = $1 AND action = 'organization_archive'"
                " AND adm_user_id = $2">>,
            [OrgId, ?WRITE_UID]
        ),
    ?assert(Count >= 1).

%% ------------------------------------------------------------------
%% 权限矩阵
%% ------------------------------------------------------------------

permission_matrix_tests(Conn) ->
    Scope = seed_org(Conn, <<"perm">>),
    OrgId = maps:get(org_id, Scope),
    Owner = maps:get(owner, Scope),
    Member = maps:get(member, Scope),
    DeptId = maps:get(dept_id, Scope),
    MutationCalls = [
        {"archive", fun() ->
            call(?RO_UID, org_archive, <<"POST">>, bindings(OrgId), <<>>)
        end},
        {"restore", fun() ->
            call(?RO_UID, org_restore, <<"POST">>, bindings(OrgId), <<>>)
        end},
        {"owner-transfer", fun() ->
            call(
                ?RO_UID,
                owner_transfer,
                <<"POST">>,
                bindings(OrgId),
                jsone:encode(#{<<"target_user_id">> => integer_to_binary(Member)})
            )
        end},
        {"member suspend", fun() ->
            call(
                ?RO_UID,
                member_suspend,
                <<"POST">>,
                org_binding(OrgId, user_id, Member),
                <<>>
            )
        end},
        {"member restore", fun() ->
            call(
                ?RO_UID,
                member_restore,
                <<"POST">>,
                org_binding(OrgId, user_id, Member),
                <<>>
            )
        end},
        {"member remove", fun() ->
            call(?RO_UID, member_remove, <<"POST">>, org_binding(OrgId, user_id, Member), <<>>)
        end},
        {"invitation create", fun() ->
            call(
                ?RO_UID,
                invitations,
                <<"POST">>,
                bindings(OrgId),
                jsone:encode(#{<<"target_user_id">> => integer_to_binary(Owner)})
            )
        end},
        {"invitation cancel", fun() ->
            call(
                ?RO_UID,
                invitation_cancel,
                <<"POST">>,
                org_binding(OrgId, invitation_id, 1),
                <<>>
            )
        end},
        {"department create", fun() ->
            call(
                ?RO_UID,
                departments,
                <<"POST">>,
                bindings(OrgId),
                jsone:encode(#{<<"name">> => <<"x">>})
            )
        end},
        {"department rename", fun() ->
            call(
                ?RO_UID,
                department_rename,
                <<"POST">>,
                org_binding(OrgId, department_id, DeptId),
                jsone:encode(#{<<"name">> => <<"y">>, <<"expected_version">> => 1})
            )
        end},
        {"department move", fun() ->
            call(
                ?RO_UID,
                department_move,
                <<"POST">>,
                org_binding(OrgId, department_id, DeptId),
                jsone:encode(#{<<"parent_id">> => null, <<"expected_version">> => 1})
            )
        end},
        {"department archive", fun() ->
            call(
                ?RO_UID,
                department_archive,
                <<"POST">>,
                org_binding(OrgId, department_id, DeptId),
                <<>>
            )
        end}
    ],
    [
        {Name ++ " → read-only 角色 403", fun() -> ?assertEqual(403, status_of(F())) end}
     || {Name, F} <- MutationCalls
    ] ++
        [
            {"read-only 角色读列表 200", fun() ->
                ?assertEqual(200, status_of(call(?RO_UID, list, <<"GET">>, #{}, <<>>)))
            end},
            {"仅有 write 无 read 角色 → 读 403", fun() ->
                ?assertEqual(403, status_of(call(?WO_READ_UID, list, <<"GET">>, #{}, <<>>)))
            end},
            {"无 adm_user_id → 403（fail-closed）", fun() ->
                Req0 = #{method => <<"GET">>, bindings => #{}},
                {ok, RespReq, _} = adm_organization_handler:init(Req0, #{action => list}),
                ?assertEqual(403, maps:get(response_status, RespReq))
            end},
            {"write 角色对读端点 200", fun() ->
                ?assertEqual(
                    200, status_of(call(?WRITE_UID, members, <<"GET">>, bindings(OrgId), <<>>))
                )
            end}
        ].

%% ------------------------------------------------------------------
%% 负例
%% ------------------------------------------------------------------

negative_tests(Conn) ->
    Scope = seed_org(Conn, <<"neg">>),
    OrgId = maps:get(org_id, Scope),
    MissingOrg = new_id(),
    [
        {"无会话 401（adm_auth_middleware 拒绝）", fun() -> negative_unauthorized() end},
        {"400 坏组织 ID（非整数路径参数）", fun() ->
            Req0 = #{
                method => <<"POST">>,
                bindings => #{organization_id => <<"abc">>},
                body => <<>>
            },
            {ok, RespReq, _} =
                adm_organization_handler:init(Req0, #{
                    action => org_archive, adm_user_id => ?WRITE_UID
                }),
            ?assertEqual(400, maps:get(response_status, RespReq))
        end},
        {"404 详情不存在", fun() ->
            ?assertEqual(
                404, status_of(call(?WRITE_UID, show, <<"GET">>, bindings(MissingOrg), <<>>))
            )
        end},
        {"404 归档不存在的 org", fun() ->
            ?assertEqual(
                404,
                status_of(call(?WRITE_UID, org_archive, <<"POST">>, bindings(MissingOrg), <<>>))
            )
        end},
        {"409 跨组织引用：member remove 引用其他 org 的成员路径（org 作用域内查无此行，与 app 层同口径 409）", fun() ->
            OtherScope = seed_org(Conn, <<"negb">>),
            AlienMember = maps:get(member, OtherScope),
            ?assertEqual(
                409,
                status_of(
                    call(
                        ?WRITE_UID,
                        member_remove,
                        <<"POST">>,
                        org_binding(OrgId, user_id, AlienMember),
                        <<>>
                    )
                )
            )
        end},
        {"409 archived 门禁：归档 org 的成员 suspend 被拒", fun() ->
            _ = call(?WRITE_UID, org_archive, <<"POST">>, bindings(OrgId), <<>>),
            ?assertEqual(
                409,
                status_of(
                    call(
                        ?WRITE_UID,
                        member_suspend,
                        <<"POST">>,
                        org_binding(OrgId, user_id, maps:get(member, Scope)),
                        <<>>
                    )
                )
            )
        end},
        {"409 archived 门禁：归档父部门下 create 部门被拒（TASK_ID=ORG-DIALYZE-FIX-R2）", fun() ->
            ArchScope = seed_org(Conn, <<"negarchp">>),
            ArchOrg = maps:get(org_id, ArchScope),
            ArchDept = maps:get(dept_id, ArchScope),
            RespArch =
                call(
                    ?WRITE_UID,
                    department_archive,
                    <<"POST">>,
                    org_binding(ArchOrg, department_id, ArchDept),
                    <<>>
                ),
            ?assertEqual(200, status_of(RespArch)),
            ?assertEqual(
                409,
                status_of(
                    call(
                        ?WRITE_UID,
                        departments,
                        <<"POST">>,
                        bindings(ArchOrg),
                        jsone:encode(#{
                            <<"name">> => <<"归档父下子部门"/utf8>>, <<"parent_id">> => ArchDept
                        })
                    )
                )
            )
        end},
        {"409 archived 门禁：归档部门 rename 被拒（TASK_ID=ORG-DIALYZE-FIX-R2）", fun() ->
            ArchScope2 = seed_org(Conn, <<"negarchr">>),
            ArchOrg2 = maps:get(org_id, ArchScope2),
            ArchDept2 = maps:get(dept_id, ArchScope2),
            RespArch2 =
                call(
                    ?WRITE_UID,
                    department_archive,
                    <<"POST">>,
                    org_binding(ArchOrg2, department_id, ArchDept2),
                    <<>>
                ),
            ?assertEqual(200, status_of(RespArch2)),
            ?assertEqual(
                409,
                status_of(
                    call(
                        ?WRITE_UID,
                        department_rename,
                        <<"POST">>,
                        org_binding(ArchOrg2, department_id, ArchDept2),
                        jsone:encode(#{<<"name">> => <<"归档后新名"/utf8>>, <<"expected_version">> => 1})
                    )
                )
            )
        end},
        {"400 坏 JSON body", fun() ->
            ?assertEqual(
                400,
                status_of(
                    call(?WRITE_UID, owner_transfer, <<"POST">>, bindings(OrgId), <<"not-json">>)
                )
            )
        end},
        {"400 缺 target_user_id", fun() ->
            ?assertEqual(
                400, status_of(call(?WRITE_UID, owner_transfer, <<"POST">>, bindings(OrgId), <<>>))
            )
        end}
    ].

negative_unauthorized() ->
    Req0 = #{method => <<"POST">>},
    {stop, RespReq} =
        adm_auth_middleware:condition(<<"POST">>, undefined, Req0, #{handler_opts => #{}}),
    ?assertEqual(401, maps:get(response_status, RespReq)).

%% ===================================================================
%% Seed / SQL helpers（合成租户夹具；随机 TSID 隔离，不 TRUNCATE 不删行）
%% ===================================================================

seed_org(Conn, Prefix) ->
    Owner = new_id(),
    Admin = new_id(),
    Member = new_id(),
    Org = new_id(),
    Ws = new_id(),
    Dept = new_id(),
    Name = <<"orgadm-", Prefix/binary, "-", (integer_to_binary(Org))/binary>>,
    Users = [Owner, Admin, Member],
    %% 3 个用户 × 2 参数（id/account）→ 占位序号 $1,$3,$5
    Ph = [
        ["($", integer_to_binary(I), ",'x',$", integer_to_binary(I + 1), ",'127.0.0.1','x')"]
     || {I, _} <- lists:zip(lists:seq(1, 6, 2), Users)
    ],
    Params = lists:flatten([[U, account(U)] || U <- Users]),
    ok = exec(
        Conn,
        iolist_to_binary([
            <<"INSERT INTO \"user\"(id,password,account,reg_ip,reg_cosv) VALUES ">>,
            lists:join(",", Ph)
        ]),
        Params
    ),
    ok = exec(
        Conn,
        <<"INSERT INTO organization(id,name,owner_id,status) VALUES ($1,$2,$3,'active')">>,
        [Org, Name, Owner]
    ),
    %% owner 成员行由 trg_organization_owner_member_sync 在 organization INSERT
    %% 时自动创建（ON CONFLICT upsert）——这里只补 admin/member 两行，
    %% 手动再插 owner 行会撞 pk_organization_member。
    ok = exec(
        Conn,
        <<
            "INSERT INTO organization_member(organization_id,user_id,role,status) VALUES"
            " ($1,$2,'admin','active'),($3,$4,'member','active')"
        >>,
        [Org, Admin, Org, Member]
    ),
    ok = exec(
        Conn,
        <<
            "INSERT INTO workspace(id,name,owner_id,organization_id,status) VALUES"
            " ($1,$2,$3,$4,'active')"
        >>,
        [Ws, <<"ws-", Prefix/binary>>, Owner, Org]
    ),
    ok = exec(
        Conn,
        <<
            "INSERT INTO organization_department(id,organization_id,parent_id,name,status,version)"
            " VALUES ($1,$2,NULL,$3,'active',1)"
        >>,
        [Dept, Org, <<"dept-", Prefix/binary>>]
    ),
    #{
        org_id => Org,
        owner => Owner,
        admin => Admin,
        member => Member,
        ws_id => Ws,
        dept_id => Dept
    }.

new_id() ->
    try elib_tsid:generate() of
        Id when is_integer(Id) -> Id
    catch
        _:_ ->
            1000000000000000 + erlang:unique_integer([positive, monotonic])
    end.

account(Uid) ->
    <<"orgadm_", (integer_to_binary(Uid))/binary>>.

exec(Conn, Sql, Params) ->
    case epgsql:equery(Conn, Sql, Params) of
        {ok, _} -> ok;
        Other -> erlang:error({seed_failed, Sql, Other})
    end.

q(Conn, Sql, Params) ->
    case epgsql:equery(Conn, Sql, Params) of
        {ok, Cols, Rows} when is_list(Rows) ->
            {ok, [row_to_map(Cols, Row) || Row <- Rows]};
        Other ->
            {error, Other}
    end.

row_to_map(Cols, Row) when is_list(Cols) ->
    Names = [element(2, C) || C <- Cols],
    maps:from_list(lists:zip(Names, tuple_to_list(Row))).

org_status_sql() ->
    <<"SELECT status FROM organization WHERE id = $1">>.

org_owner_sql() ->
    <<"SELECT owner_id FROM organization WHERE id = $1">>.

member_status_sql() ->
    <<"SELECT status FROM organization_member WHERE organization_id = $1 AND user_id = $2">>.
