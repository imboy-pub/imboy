-module(organization_lifecycle_tests).

%% ORG-02 Organization lifecycle（C16）archive/restore 测试。
%%
%% 覆盖：
%%   * owner archive → status=archived + 审计事件一次；
%%     重复 archive 幂等（稳定成功、不再产生审计事件）；
%%   * restore 幂等同理；admin 可 archive/restore；
%%   * member / 非成员 archive|restore 被 403 拒；未知 org 404；
%%   * archived org 拒新写：org update 409、owner transfer 409、
%%     成员新增（invite）409 —— 授权只读与 restore 放行；
%%   * handler 三 action（archive/restore/deletion_preflight）分派正确。
%%
%% 运行：make eunit-local t=organization_lifecycle_tests

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(ORG_ID, 101).
-define(UID, 201).
-define(LIFECYCLE_EVENTS, [organization_archived, organization_restored]).

%% ===================================================================
%% 真 PG：archive/restore 幂等 command + 审计 + archived 拒新写
%% ===================================================================

archive_restore_idempotent_command_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        Uid = new_uid(),
        AdminUid = new_uid(),
        MemberUid = new_uid(),
        OrgId = new_uid(),
        Self = self(),
        %% 拦截 elib_log 审计事件（passthrough 转发，不破坏其余日志）
        ok = meck:new(elib_log, [passthrough, no_link]),
        ok = meck:expect(elib_log, internal_log, fun(Level, Msg, M, L) ->
            report_lifecycle(Self, Msg),
            meck:passthrough([Level, Msg, M, L])
        end),
        try
            lists:foreach(fun create_user/1, [Uid, AdminUid, MemberUid]),
            create_org_with_owner(OrgId, Uid),
            {ok, _} = elib_pg:query(
                <<
                    "INSERT INTO public.organization_member"
                    " (organization_id, user_id, role, joined_at, status)"
                    " VALUES ($1, $2, 'admin', CURRENT_TIMESTAMP, 'active')"
                >>,
                [OrgId, AdminUid]
            ),
            {ok, _} = elib_pg:query(
                <<
                    "INSERT INTO public.organization_member"
                    " (organization_id, user_id, role, joined_at, status)"
                    " VALUES ($1, $2, 'member', CURRENT_TIMESTAMP, 'active')"
                >>,
                [OrgId, MemberUid]
            ),
            %% ① member archive 被 403 拒
            {error, {403, _}} = organization_lifecycle:archive(MemberUid, OrgId),
            %% ② owner archive → archived + 审计一次
            {ok, #{<<"status">> := <<"archived">>}} = organization_lifecycle:archive(Uid, OrgId),
            org_status_is(OrgId, <<"archived">>),
            AfterArchive = drain_lifecycle_events(),
            ?assertEqual(1, count_events(organization_archived, AfterArchive)),
            ?assertEqual(0, count_events(organization_restored, AfterArchive)),
            %% ③ 重复 archive 幂等：稳定成功、无第二次审计事件
            {ok, #{<<"status">> := <<"archived">>}} = organization_lifecycle:archive(Uid, OrgId),
            ?assertEqual([], drain_lifecycle_events()),
            %% ④ archived 拒新写：org update 409（既有入口门禁）
            {error, {409, _}} = organization_logic:update(
                Uid, OrgId, <<"new-name">>, undefined, undefined
            ),
            %% ⑤ archived 拒新写：owner transfer 409（ORG-01 既有入口门禁）
            {error, {409, _}} = organization_owner_transfer:transfer(Uid, OrgId, AdminUid),
            %% ⑥ archived 拒新写：成员新增 409（member_logic 既有入口门禁）
            {error, {409, _}} = organization_member_logic:invite(
                Uid, OrgId, MemberUid, <<"member">>
            ),
            %% ⑦ admin restore（archived 态唯一放行的写入口）→ active
            {ok, #{<<"status">> := <<"active">>}} = organization_lifecycle:restore(AdminUid, OrgId),
            org_status_is(OrgId, <<"active">>),
            %% ⑧ restore 幂等：已 active 再 restore 稳定成功
            {ok, #{<<"status">> := <<"active">>}} = organization_lifecycle:restore(Uid, OrgId),
            %% ⑨ 未知 org 404 / 非成员 403
            {error, {404, _}} = organization_lifecycle:archive(Uid, new_uid()),
            {error, {403, _}} = organization_lifecycle:restore(Uid + 1000000, OrgId),
            %% ⑩ 幂等重放不重复审计：此后仅有 restore 首次的一条事件
            AfterRestore = drain_lifecycle_events(),
            ?assertEqual(1, count_events(organization_restored, AfterRestore)),
            ?assertEqual(0, count_events(organization_archived, AfterRestore))
        after
            _ = (catch meck:unload(elib_log)),
            %% 先删 org 行（级联成员行；127 invariant 对「org 已消失」放行），
            %% 再清理用户行（owner 用户的删除被 organization.owner_id RESTRICT
            %% 阻止，必须发生在 org 行删除之后）
            _ = elib_pg:query(<<"DELETE FROM public.organization WHERE id = $1">>, [OrgId]),
            lists:foreach(fun cleanup_user/1, [Uid, AdminUid, MemberUid])
        end
    end).

%% ===================================================================
%% handler 三 action 分派（meck，无 DB）
%% ===================================================================

handler_actions_delegate_test_() ->
    ?WITH_MECKS(handler_mocks(), fun() ->
        put(calls, []),
        Archive = organization_handler:handle_action(archive, post_req, #{
            current_uid => ?UID
        }),
        ?assertEqual(200, maps:get(response_status, Archive)),
        Restore = organization_handler:handle_action(restore, post_req, #{
            current_uid => ?UID
        }),
        ?assertEqual(200, maps:get(response_status, Restore)),
        Preflight = organization_handler:handle_action(deletion_preflight, get_req, #{
            current_uid => ?UID
        }),
        ?assertEqual(200, maps:get(response_status, Preflight)),
        NotAllowed = organization_handler:handle_action(archive, get_req, #{current_uid => ?UID}),
        ?assertEqual(405, maps:get(response_status, NotAllowed)),
        ?assertEqual(
            [
                {archive, ?UID, ?ORG_ID},
                {restore, ?UID, ?ORG_ID},
                {deletion_preflight, ?UID}
            ],
            get(calls)
        )
    end).

handler_mocks() ->
    [
        {cowboy_req, [
            {'method', 1, fun
                (get_req) -> <<"GET">>;
                (_) -> <<"POST">>
            end},
            {'binding', 2, fun
                (organization_id, _) -> integer_to_binary(?ORG_ID);
                (_, _) -> undefined
            end},
            {'reply', 4, fun(405, Headers, _Body, _Req) ->
                #{response_status => 405, allow => maps:get(<<"allow">>, Headers)}
            end}
        ]},
        {elib_param, [{'post', 1, fun(_) -> #{} end}]},
        {elib_response, [
            {'success', 2, fun(_Req, _Payload) -> #{response_status => 200} end},
            {'error', 3, fun(_Req, Msg, Code) -> #{response_status => Code, msg => Msg} end}
        ]},
        {auth_ds, [{'current_uid', 1, fun(_State) -> ?UID end}]},
        {organization_logic, [
            {'archive', 2, fun(Uid, OrgId) ->
                log_call(archive, Uid, OrgId),
                {ok, #{}}
            end},
            {'restore', 2, fun(Uid, OrgId) ->
                log_call(restore, Uid, OrgId),
                {ok, #{}}
            end},
            {'deletion_preflight', 1, fun(Uid) ->
                log_call1(deletion_preflight, Uid),
                {ok, #{}}
            end}
        ]}
    ].

log_call(Tag, Uid, OrgId) ->
    put(calls, get(calls) ++ [{Tag, Uid, OrgId}]).

log_call1(Tag, Uid) ->
    put(calls, get(calls) ++ [{Tag, Uid}]).

%% ===================================================================
%% Internal
%% ===================================================================

report_lifecycle(Self, [Tag, _OrgId, _Uid, _Status] = Msg) ->
    case lists:member(Tag, ?LIFECYCLE_EVENTS) of
        true -> Self ! {lifecycle, Msg};
        false -> ok
    end;
report_lifecycle(_Self, _Other) ->
    ok.

drain_lifecycle_events() ->
    drain_lifecycle_events([]).

drain_lifecycle_events(Acc) ->
    receive
        {lifecycle, Msg} -> drain_lifecycle_events([Msg | Acc])
    after 200 ->
        lists:reverse(Acc)
    end.

count_events(Tag, Events) ->
    length([1 || [T | _] <- Events, T =:= Tag]).

org_status_is(OrgId, Status) ->
    {ok, [#{<<"status">> := Status}]} = elib_pg:query(
        <<"SELECT status FROM public.organization WHERE id = $1">>, [OrgId]
    ),
    ok.

new_uid() ->
    erlang:system_time(millisecond) * 1000 + rand:uniform(999).

create_user(Uid) ->
    {ok, _} = elib_pg:query(
        <<
            "INSERT INTO public.\"user\" (id, account, password, reg_ip, reg_cosv)"
            " VALUES ($1, $2, 'x', '127.0.0.1', '')"
        >>,
        [Uid, <<"u", (integer_to_binary(Uid))/binary>>]
    ),
    ok.

create_org_with_owner(OrgId, OwnerUid) ->
    {ok, _} = elib_pg:query(
        <<
            "INSERT INTO public.organization (id, name, owner_id)"
            " VALUES ($1, 'lifecycle-probe', $2)"
        >>,
        [OrgId, OwnerUid]
    ),
    ok.

cleanup_user(Uid) ->
    lists:foreach(fun(Sql) -> _ = elib_pg:query(Sql, [Uid]) end, [
        <<"DELETE FROM public.organization_member WHERE user_id = $1">>,
        <<"DELETE FROM public.\"user\" WHERE id = $1">>
    ]).
