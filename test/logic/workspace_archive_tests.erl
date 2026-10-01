-module(workspace_archive_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

%%% 双体验 v2.5.2 WP4/T7 — 工作区归档/恢复 + 写路径拒绝/读取保留 单元测试
%%% 覆盖：archive/restore（Owner only、审计列 archived_at/archived_by、
%%% 重复归档 409、恢复后放行）、归档下每类写路径拒绝（稳定错误码 980）、
%%% 派生已读跳过（archived 时 clear_unread 不调用但不 403）、
%%% personal 资源不受 guard 影响。

-define(WS_ID, 800001).
-define(OWNER, 900001).
-define(MEMBER2, 900002).
-define(GUEST, 900003).
-define(GID, 777001).
-define(CID, 666001).

tx_fun() ->
    fun(Fun) ->
        try
            Fun(fake_conn)
        catch
            throw:{abort_tx, Reason} -> {error, Reason}
        end
    end.

ws_row(Status) ->
    #{<<"id">> => ?WS_ID, <<"owner_id">> => ?OWNER, <<"status">> => Status}.

%% ⚠️ eunit 不解释 {Desc, fun} 返回的 {setup,...} spec（探针实证），
%% ?WITH_MECKS 包在 {Desc, fun} 体内 = 静默空转。此 helper 立即执行等价语义：
%% setup → 执行断言 → cleanup，使断言真实生效（simple fun 与 generator 同进程，
%% Self 哨兵可用，无需改进程字典）。
run_with_mocks(MockConfigs, TestFun) ->
    lists:foreach(
        fun({Module, Expectations}) ->
            case meck_helper:setup_mock(Module, Expectations) of
                {ok, _} -> ok;
                {error, Reason} -> erlang:error({mock_setup_failed, Module, Reason})
            end
        end,
        MockConfigs
    ),
    try
        TestFun()
    after
        lists:foreach(
            fun({Module, _}) -> meck_helper:cleanup_mock(Module) end,
            MockConfigs
        )
    end.

%%% ===================================================================
%%% archive / restore（Owner only + 审计列）
%%% ===================================================================

wait_archive_sql() ->
    receive
        {archive_sql, Sql, Params} ->
            %% 审计列：status/archived_at/archived_by 同写
            ?assertNotEqual(nomatch, binary:match(Sql, <<"archived_at = $1">>)),
            ?assertNotEqual(nomatch, binary:match(Sql, <<"archived_by = $2">>)),
            ?assertEqual([?OWNER, ?WS_ID], tl(Params));
        _Other ->
            wait_archive_sql()
    after 500 ->
        ?assert(false, "archive UPDATE not executed")
    end.

archive_test_() ->
    Self = self(),
    [
        {"owner archives with audit columns", fun() ->
            run_with_mocks(archive_mocks(<<"active">>, Self), fun() ->
                ?assertMatch(
                    {ok, #{workspace_id := ?WS_ID, status := <<"archived">>, archived_by := ?OWNER}},
                    workspace_logic:archive(?OWNER, ?WS_ID)
                ),
                %% 哨兵 receive：非目标消息（eunit 用例串行复用进程邮箱，
                %% 跨用例陈旧消息先于目标到达属常态）跳过继续等，不以通配
                %% 兜底直接判死——即跨候选稳定失败例6的根因，取证见
                %% stable-failures-root-cause-20260929.md。
                wait_archive_sql(),
                %% C05/ORG-05：归档同事务触发 Org 默认工作区交接钩子
                %% （此处 ws 行归属列为 null → 个人域 undefined，钩子仍被调用）
                receive
                    {default_ws_handover, OrgId0, ?WS_ID, _ReplacementWsId} ->
                        ?assertEqual(undefined, OrgId0)
                after 500 -> ?assert(false, "default workspace handover hook not called")
                end
            end)
        end},
        {"non owner cannot archive (403)", fun() ->
            run_with_mocks(archive_mocks(<<"active">>, Self), fun() ->
                ?assertMatch(
                    {error, {403, _}}, workspace_logic:archive(?MEMBER2, ?WS_ID)
                )
            end)
        end},
        {"double archive rejected 409", fun() ->
            run_with_mocks(archive_mocks(<<"archived">>, Self), fun() ->
                ?assertMatch(
                    {error, {409, _}}, workspace_logic:archive(?OWNER, ?WS_ID)
                )
            end)
        end},
        {"archiving last default workspace rejected 409 (G3 strong handover)", fun() ->
            %% 计划 §105：被归档者是 Org 默认且**未显式指定替代项** → 稳定 409
            %% （引导先设默认或一并传 replacement_workspace_id），不再自动改指。
            run_with_mocks(archive_mocks(<<"active">>, Self), fun() ->
                meck:expect(
                    organization_default_workspace_app,
                    replace_on_archive_tx,
                    4,
                    fun(_Conn, _OrgId, _WsId, _ReplacementWsId) ->
                        throw(
                            {abort_tx,
                                {default_workspace_handover_required, replacement_not_specified}}
                        )
                    end
                ),
                ?assertMatch(
                    {error, {409, _Msg}}, workspace_logic:archive(?OWNER, ?WS_ID)
                )
            end)
        end},
        {"restore clears audit columns", fun() ->
            run_with_mocks(archive_mocks(<<"archived">>, Self), fun() ->
                ?assertMatch(
                    {ok, #{workspace_id := ?WS_ID, status := <<"active">>}},
                    workspace_logic:restore(?OWNER, ?WS_ID)
                ),
                receive
                    {restore_sql, Sql, _Params} ->
                        ?assertNotEqual(nomatch, binary:match(Sql, <<"archived_at = NULL">>)),
                        ?assertNotEqual(nomatch, binary:match(Sql, <<"archived_by = NULL">>))
                after 500 -> ?assert(false, "restore UPDATE not executed")
                end
            end)
        end},
        {"restore non-archived rejected 409", fun() ->
            run_with_mocks(archive_mocks(<<"active">>, Self), fun() ->
                ?assertMatch(
                    {error, {409, _}}, workspace_logic:restore(?OWNER, ?WS_ID)
                )
            end)
        end},
        {"guard passes again after restore", fun() ->
            run_with_mocks(archive_mocks(<<"active">>, Self), fun() ->
                ?assertEqual(
                    ok, workspace_guard:ensure_writable({workspace, ?WS_ID})
                )
            end)
        end}
    ].

archive_mocks(CurrStatus, Self) ->
    [
        {workspace_ds, [
            %% ds 层现行是 1 元包装（直通 repo:find_by_id/2 → elib_pg:one），
            %% mock 打在 ds 1 元入口，elib_pg:one 的全列 SELECT 不会发生
            {'find_by_id', 1, fun
                (?WS_ID) -> ws_row(CurrStatus);
                (_) -> #{}
            end}
        ]},
        {workspace_member_repo, [
            {'find', 3, fun(?WS_ID, U, _) ->
                case U of
                    ?OWNER -> #{<<"role">> => <<"owner">>, <<"status">> => <<"active">>};
                    ?GUEST -> #{<<"role">> => <<"guest">>, <<"status">> => <<"active">>};
                    ?MEMBER2 -> #{<<"role">> => <<"member">>, <<"status">> => <<"active">>};
                    _ -> #{}
                end
            end}
        ]},
        {workspace_resolver, [
            {'resolve_workspace', 1, fun
                ({workspace, ?WS_ID}) -> {ok, ?WS_ID};
                ({group, ?GID}) -> {ok, ?WS_ID};
                ({channel, ?CID}) -> {ok, ?WS_ID};
                (_) -> personal
            end}
        ]},
        {elib_pg, [
            {'with_tx', 1, tx_fun()},
            {'one', 2, fun(<<"SELECT status FROM workspace", _/binary>>, _) ->
                {ok, #{<<"status">> => ws_status_of(CurrStatus)}}
            end},
            {'query', 3, fun
                (fake_conn, <<"SELECT status FROM workspace", _/binary>>, _) ->
                    {ok, [#{<<"status">> => ws_status_of(CurrStatus)}]};
                %% C05/ORG-05：归档交接需事务内读归属 Org（本 mock 返回 null=个人域）
                (fake_conn, <<"UPDATE workspace SET status = 'archived'", _/binary>> = Sql, Params) ->
                    case ws_status_of(CurrStatus) of
                        <<"active">> ->
                            Self ! {archive_sql, Sql, Params},
                            {ok, [#{<<"organization_id">> => null}]};
                        _ ->
                            {ok, []}
                    end
            end},
            {'execute', 3, fun(fake_conn, Sql, Params) ->
                case Sql of
                    <<"UPDATE workspace SET status = 'archived'", _/binary>> ->
                        case ws_status_of(CurrStatus) of
                            <<"active">> ->
                                Self ! {archive_sql, Sql, Params},
                                {ok, 1};
                            _ ->
                                {ok, 0}
                        end;
                    <<"UPDATE workspace SET status = 'active'", _/binary>> ->
                        case ws_status_of(CurrStatus) of
                            <<"archived">> ->
                                Self ! {restore_sql, Sql, Params},
                                {ok, 1};
                            _ ->
                                {ok, 0}
                        end;
                    _ ->
                        {ok, 1}
                end
            end}
        ]},
        {organization_default_workspace_app, [
            %% 归档同事务默认工作区交接钩子（ORG-05）；哨兵断言钩子已触发
            {'replace_on_archive_tx', 4, fun(_Conn, OrgId, WsId, ReplacementWsId) ->
                Self ! {default_ws_handover, OrgId, WsId, ReplacementWsId},
                ok
            end}
        ]}
    ].

ws_status_of(Status) ->
    Status.

%%% ===================================================================
%%% 归档下各写路径拒绝（稳定错误码 980）
%%% ===================================================================

archived_write_rejection_test_() ->
    ArchivedMocks =
        {workspace_resolver, [
            {'resolve_workspace', 1, fun
                ({workspace, ?WS_ID}) -> {ok, ?WS_ID};
                ({group, ?GID}) -> {ok, ?WS_ID};
                ({channel, ?CID}) -> {ok, ?WS_ID};
                ({group_notice, 123}) -> {ok, ?WS_ID};
                (_) -> personal
            end}
        ]},
    PgMock =
        {elib_pg, [
            {'with_tx', 1, tx_fun()},
            {'one', 2, fun(<<"SELECT status FROM workspace", _/binary>>, _) ->
                {ok, #{<<"status">> => <<"archived">>}}
            end},
            {'query', 3, fun(fake_conn, <<"SELECT status FROM workspace", _/binary>>, _) ->
                {ok, [#{<<"status">> => <<"archived">>}]}
            end},
            {'execute', 3, fun(_C, _S, _P) -> {ok, 1} end}
        ]},
    %% P0 收口后：写守卫在 group_notice_ds 写事务内（真 DS 跑通），logic 只做
    %% 读取（find_by_id）与权限检查，故仅 mock 读函数与权限函数。
    NoticeMocks =
        {group_notice_ds, [
            {'find_by_id', 1, fun(_) -> {ok, #{<<"group_id">> => ?GID}} end}
        ]},
    PermMocks =
        {group_member_ds, [
            {'get_member_info', 3, fun(_, ?OWNER, <<"role">>) -> {ok, #{<<"role">> => 4}} end},
            {'find_by_gid_and_uid', 3, fun(_, _, _) -> #{<<"id">> => 1} end}
        ]},
    [
        {"group message write rejected in tx (R3 #1)", fun() ->
            run_with_mocks([ArchivedMocks, PgMock], fun() ->
                ?assertMatch(
                    {error, {980, _}},
                    msg_c2g_repo:write_msg(
                        <<"2026-08-26T00:00:00Z">>,
                        <<"msg-1">>,
                        <<"{}">>,
                        ?OWNER,
                        [?OWNER],
                        ?GID,
                        <<"text">>,
                        null
                    )
                )
            end)
        end},
        {"group notice write rejected (R3 #17, P0 收口后同事务守卫)", fun() ->
            run_with_mocks([ArchivedMocks, PgMock, NoticeMocks, PermMocks], fun() ->
                ?assertEqual(
                    {error, ?ERR_WORKSPACE_ARCHIVED},
                    group_notice_logic:pin(?OWNER, 123)
                ),
                ?assertEqual(
                    {error, ?ERR_WORKSPACE_ARCHIVED},
                    group_notice_logic:unpin(?OWNER, 123)
                ),
                ?assertEqual(
                    {error, ?ERR_WORKSPACE_ARCHIVED},
                    group_notice_logic:delete(?OWNER, 123)
                ),
                ?assertEqual(
                    {error, ?ERR_WORKSPACE_ARCHIVED},
                    %% ds 层先校验必填字段（group_id/user_id）再进归档守卫，
                    %% 载荷须可过校验才能到达 980
                    group_notice_logic:insert(?OWNER, #{
                        group_id => ?GID,
                        user_id => ?OWNER
                    })
                ),
                ?assertEqual(
                    {error, ?ERR_WORKSPACE_ARCHIVED},
                    group_notice_logic:update(?OWNER, 123, #{title => <<"t">>})
                )
            end)
        end},
        {"group notice mark_read skips instead of 403 (R3 #18)", fun() ->
            run_with_mocks([ArchivedMocks, PgMock, NoticeMocks, PermMocks], fun() ->
                ?assertMatch(
                    {ok, #{<<"group_id">> := ?GID}},
                    group_notice_logic:mark_as_read(?OWNER, 123)
                )
            end)
        end},
        {"channel publish rejected in tx (R3 #6)", fun() ->
            Self = self(),
            UserMock =
                {user_repo, [
                    {'find_by_id', 2, fun(_, _) ->
                        #{<<"nickname">> => <<"a">>, <<"avatar">> => <<>>}
                    end}
                ]},
            RepoMock =
                {channel_message_repo, [
                    {'add', 2, fun(_Conn, _Data) ->
                        Self ! channel_add_forbidden,
                        {error, must_not_add}
                    end}
                ]},
            run_with_mocks([ArchivedMocks, PgMock, UserMock, RepoMock], fun() ->
                ?assertMatch(
                    {error, {980, _}},
                    channel_ds:publish_message(?CID, ?OWNER, <<"hi">>, <<"text">>, #{})
                ),
                receive
                    channel_add_forbidden -> ?assert(false, "must not add when archived")
                after 0 -> ok
                end
            end)
        end}
    ].

%%% ===================================================================
%%% personal 资源永不受 guard 影响（回归红线）
%%% ===================================================================

personal_not_affected_test_() ->
    PersonalResolver =
        {workspace_resolver, [
            {'resolve_workspace', 1, fun(_) -> personal end}
        ]},
    PgMock =
        {elib_pg, [
            {'with_tx', 1, tx_fun()},
            {'query', 3, fun(_C, _S, _P) -> {error, must_not_query_workspace} end},
            {'execute', 3, fun(_C, _S, _P) -> {ok, 1} end}
        ]},
    [
        {"personal group message write passes without workspace query", fun() ->
            run_with_mocks([PersonalResolver, PgMock], fun() ->
                %% 真跑 msg_c2g_repo 需 TSID 环境：先 init（幂等）再注册
                _ = elib_tsid:init(#{dc_id => 1, node_id => 1}),
                ok = elib_tsid:register(msg_c2g),
                ?assertEqual(
                    ok,
                    msg_c2g_repo:write_msg(
                        <<"2026-08-26T00:00:00Z">>,
                        <<"msg-2">>,
                        <<"{}">>,
                        ?OWNER,
                        [?OWNER],
                        ?GID,
                        <<"text">>,
                        null
                    )
                )
            end)
        end}
    ].
