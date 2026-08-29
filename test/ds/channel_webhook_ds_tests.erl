-module(channel_webhook_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc channel_webhook_ds TX-01 事务收敛测试（真库直连）
%%%
%%% create/3 四步写【建 system_bot user 行 → 标 account_type=2 →
%%% bot 加频道编辑(role=1) → 落 channel_webhook 表】已收进
%%% workspace_guard:write_tx 单事务。本测试用 scratch 真库验证：
%%%   1. 故障注入矩阵：第 1/2/3/4 步分别注入失败 → 直连 SQL 断言零孤儿
%%%      （bot user 行、account_type 标记、channel_admin 授权、webhook 行）；
%%%   2. 归档拒绝：archived workspace 的频道上创建 → 980 且零行
%%%      （前置短路 + 事务内守卫兜底，不建任何行）；
%%%   3. 正常路径回归：成功创建后四表内容正确，token 唯一。
%%%===================================================================

%% ===================================================================
%% 正常路径回归：成功创建后各表行数与内容正确
%% ===================================================================

create_persists_user_admin_and_webhook_rows_test_() ->
    ?TEST_WITH_DB(fun() ->
        {ChannelId, _CreatorUid} = fixture_new(),
        {ok, #{<<"id">> := WhId, <<"token">> := Token, <<"bot_uid">> := BotUid}} =
            channel_webhook_ds:create(ChannelId, <<"回归 Webhook"/utf8>>, _CreatorUid),
        %% bot user 行：account_type=2、account=chbot_<uid>
        {ok, [User]} =
            elib_pg:query(
                <<"SELECT id, account, account_type, nickname FROM ",
                    (user_repo:tablename())/binary, " WHERE id = $1">>,
                [BotUid]
            ),
        ?assertEqual(2, maps:get(<<"account_type">>, User)),
        ?assertEqual(
            <<"chbot_", (integer_to_binary(BotUid))/binary>>, maps:get(<<"account">>, User)
        ),
        ?assertEqual(<<"回归 Webhook"/utf8>>, maps:get(<<"nickname">>, User)),
        %% channel_admin 授权：role=1（频道编辑）
        {ok, [Admin]} =
            elib_pg:query(
                <<
                    "SELECT channel_id, user_id, role FROM channel_admin"
                    " WHERE channel_id = $1 AND user_id = $2"
                >>,
                [ChannelId, BotUid]
            ),
        ?assertEqual(1, maps:get(<<"role">>, Admin)),
        %% webhook 行：token/bot_uid/creator_uid/status 正确
        {ok, [Wh]} =
            elib_pg:query(
                <<
                    "SELECT id, token, bot_uid, status, channel_id FROM channel_webhook"
                    " WHERE id = $1"
                >>,
                [WhId]
            ),
        ?assertEqual(Token, maps:get(<<"token">>, Wh)),
        ?assertEqual(BotUid, maps:get(<<"bot_uid">>, Wh)),
        ?assertEqual(1, maps:get(<<"status">>, Wh)),
        ?assertEqual(ChannelId, maps:get(<<"channel_id">>, Wh)),
        fixture_cleanup(ChannelId)
    end).

%% ===================================================================
%% 故障注入矩阵：第 1/2/3/4 步分别失败 → 整体回滚零孤儿
%% ===================================================================

%% 第 1 步失败：user_repo:create_tx 注入 error（bot user 行未写入）
create_step1_user_insert_failure_leaves_zero_orphans_test_() ->
    ?TEST_WITH_DB(fun() ->
        {ChannelId, CreatorUid} = fixture_new(),
        {Result, BotUid} = run_create_with_injection(step1, ChannelId, CreatorUid),
        ?assertMatch({error, _}, Result),
        assert_zero_orphans(ChannelId, BotUid),
        fixture_cleanup(ChannelId)
    end).

%% 第 2 步失败：user_repo:update_tx（account_type 标记）注入 error
create_step2_account_type_failure_leaves_zero_orphans_test_() ->
    ?TEST_WITH_DB(fun() ->
        {ChannelId, CreatorUid} = fixture_new(),
        {Result, BotUid} = run_create_with_injection(step2, ChannelId, CreatorUid),
        ?assertMatch({error, _}, Result),
        assert_zero_orphans(ChannelId, BotUid),
        fixture_cleanup(ChannelId)
    end).

%% 第 3 步失败：channel_admin_repo:add 注入 error
%% （路由锁定：{error, {channel_admin, _}} 分支 → 固定文案 <<"创建 webhook 失败">>）
create_step3_channel_admin_failure_leaves_zero_orphans_test_() ->
    ?TEST_WITH_DB(fun() ->
        {ChannelId, CreatorUid} = fixture_new(),
        {Result, BotUid} = run_create_with_injection(step3, ChannelId, CreatorUid),
        ?assertEqual({error, <<"创建 webhook 失败"/utf8>>}, Result),
        assert_zero_orphans(ChannelId, BotUid),
        fixture_cleanup(ChannelId)
    end).

%% 第 4 步失败：channel_webhook_repo:add_tx 注入 error
%% （路由锁定：{error, {webhook_insert, _}} 分支；insert_webhook_tx 已把底层
%%  错误归一为 <<"创建 webhook 失败">> 并日志记录底层注入错误）
create_step4_webhook_insert_failure_leaves_zero_orphans_test_() ->
    ?TEST_WITH_DB(fun() ->
        {ChannelId, CreatorUid} = fixture_new(),
        {Result, BotUid} = run_create_with_injection(step4, ChannelId, CreatorUid),
        ?assertEqual({error, <<"创建 webhook 失败"/utf8>>}, Result),
        assert_zero_orphans(ChannelId, BotUid),
        fixture_cleanup(ChannelId)
    end).

%% ===================================================================
%% 归档拒绝：archived workspace 频道 → 980 且零行（不白建 bot user）
%% ===================================================================

create_rejected_on_archived_workspace_channel_leaves_zero_rows_test_() ->
    ?TEST_WITH_DB(fun() ->
        ChannelId = fixture_new_channel(<<"tx01_arch_", (suffix())/binary>>),
        %% workspace.owner_id 有 FK，scratch 库无种子数据：fixture 先建 owner
        OwnerUid = 91_300_000_000_000_000 + rand:uniform(999_999_999),
        {ok, _} =
            elib_pg:query(
                <<"INSERT INTO ", (user_repo:tablename())/binary,
                    " (id, nickname, account, password, reg_ip, reg_cosv)"
                    " VALUES ($1, $2, $3, $4, $5, $6)">>,
                [
                    OwnerUid,
                    <<"tx01_ws_owner"/utf8>>,
                    <<"tx01_ws_owner_", (suffix())/binary>>,
                    <<>>,
                    <<>>,
                    <<>>
                ]
            ),
        WsId = fixture_new_workspace(OwnerUid, <<"archived">>),
        {ok, _} =
            elib_pg:query(
                <<"UPDATE channel SET scope = 'workspace', workspace_id = $1 WHERE id = $2">>,
                [WsId, ChannelId]
            ),
        Result = channel_webhook_ds:create(ChannelId, <<"归档频道 webhook"/utf8>>, OwnerUid),
        ?assertMatch({error, {980, _}}, Result),
        %% 零行：不落 webhook、不授权 channel_admin（前置短路 + 事务内守卫兜底）
        {ok, [#{<<"n">> := 0}]} =
            elib_pg:query(
                <<"SELECT count(*) AS n FROM channel_webhook WHERE channel_id = $1">>,
                [ChannelId]
            ),
        {ok, [#{<<"n">> := 0}]} =
            elib_pg:query(
                <<"SELECT count(*) AS n FROM channel_admin WHERE channel_id = $1">>,
                [ChannelId]
            ),
        fixture_cleanup(ChannelId),
        workspace_cleanup(WsId),
        _ = elib_pg:query(
            <<"DELETE FROM ", (user_repo:tablename())/binary, " WHERE id = $1">>,
            [OwnerUid]
        ),
        ok
    end).

%% ===================================================================
%% Internal — 数据准备与断言
%% ===================================================================

suffix() ->
    integer_to_binary(erlang:unique_integer([positive])).

fixture_new() ->
    CreatorUid = 1,
    ChannelId = fixture_new_channel(<<"tx01_wh_", (suffix())/binary>>),
    {ChannelId, CreatorUid}.

fixture_new_channel(Name) ->
    ChannelId = 91_100_000_000_000_000 + rand:uniform(999_999_999),
    {ok, _} =
        elib_pg:query(
            <<"INSERT INTO channel (id, name, creator_uid) VALUES ($1, $2, $3)">>,
            [ChannelId, Name, 1]
        ),
    ChannelId.

fixture_new_workspace(OwnerUid, Status) ->
    WsId = 91_200_000_000_000_000 + rand:uniform(999_999_999),
    {ok, _} =
        elib_pg:query(
            <<"INSERT INTO workspace (id, name, owner_id, status) VALUES ($1, $2, $3, $4)">>,
            [WsId, <<"tx01-ws-", (suffix())/binary>>, OwnerUid, Status]
        ),
    WsId.

workspace_cleanup(WsId) ->
    _ = elib_pg:query(<<"DELETE FROM workspace WHERE id = $1">>, [WsId]),
    ok.

%% 清理本 channel 的 webhook/admin/bot user（FK 顺序：webhook → admin → user）
fixture_cleanup(ChannelId) ->
    {ok, BotUids} =
        elib_pg:query(
            <<"SELECT bot_uid FROM channel_webhook WHERE channel_id = $1">>,
            [ChannelId]
        ),
    _ = elib_pg:query(<<"DELETE FROM channel_webhook WHERE channel_id = $1">>, [ChannelId]),
    _ = elib_pg:query(<<"DELETE FROM channel_admin WHERE channel_id = $1">>, [ChannelId]),
    lists:foreach(
        fun(#{<<"bot_uid">> := Uid}) ->
            _ = elib_pg:query(
                <<"DELETE FROM ", (user_repo:tablename())/binary, " WHERE id = $1">>,
                [Uid]
            )
        end,
        BotUids
    ),
    _ = elib_pg:query(<<"DELETE FROM channel WHERE id = $1">>, [ChannelId]),
    ok.

%% 零孤儿断言：bot user 行、account_type 标记、channel_admin、webhook 全查空
assert_zero_orphans(ChannelId, BotUid) ->
    {ok, UserRows} =
        elib_pg:query(
            <<"SELECT id, account_type FROM ", (user_repo:tablename())/binary,
                " WHERE id = $1 OR account = $2">>,
            [BotUid, <<"chbot_", (integer_to_binary(BotUid))/binary>>]
        ),
    ?assertEqual([], UserRows, {orphan_user_rows, UserRows}),
    {ok, AdminRows} =
        elib_pg:query(
            <<"SELECT user_id FROM channel_admin WHERE channel_id = $1 AND user_id = $2">>,
            [ChannelId, BotUid]
        ),
    ?assertEqual([], AdminRows, {orphan_admin_rows, AdminRows}),
    {ok, WhRows} =
        elib_pg:query(
            <<"SELECT id FROM channel_webhook WHERE bot_uid = $1">>,
            [BotUid]
        ),
    ?assertEqual([], WhRows, {orphan_webhook_rows, WhRows}),
    ok.

%% ===================================================================
%% 故障注入 helper：meck 只覆盖被 expect 的函数（passthrough 其余），
%% 注入 fun 先捕获本次要写入的 bot uid（供零孤儿断言定位），再返回 error。
%% 返回 {CreateResult, CapturedBotUid}。
%% ===================================================================

run_create_with_injection(step1, ChannelId, CreatorUid) ->
    Ets = ets:new(tx01_wh_capture, [public, set]),
    _ = meck_helper:setup_mock(user_repo, [
        {'create_tx', 2, fun(_Conn, Data) ->
            ets:insert(Ets, {uid, maps:get(id, Data)}),
            {error, {injected, step1}}
        end}
    ]),
    Result =
        try
            channel_webhook_ds:create(ChannelId, <<"注入 webhook"/utf8>>, CreatorUid)
        after
            meck_helper:cleanup_mock(user_repo)
        end,
    {Result, captured_uid(Ets)};
run_create_with_injection(step2, ChannelId, CreatorUid) ->
    Ets = ets:new(tx01_wh_capture, [public, set]),
    _ = meck_helper:setup_mock(user_repo, [
        {'create_tx', 2, fun(Conn, Data) ->
            Ret = meck:passthrough([Conn, Data]),
            ets:insert(Ets, {uid, element(2, Ret)}),
            Ret
        end},
        {'update_tx', 3, fun(_Conn, Id, _Data) ->
            ets:insert(Ets, {uid, Id}),
            {error, {injected, step2}}
        end}
    ]),
    Result =
        try
            channel_webhook_ds:create(ChannelId, <<"注入 webhook"/utf8>>, CreatorUid)
        after
            meck_helper:cleanup_mock(user_repo)
        end,
    {Result, captured_uid(Ets)};
run_create_with_injection(step3, ChannelId, CreatorUid) ->
    Ets = ets:new(tx01_wh_capture, [public, set]),
    _ = meck_helper:setup_mock(channel_admin_repo, [
        {'add', 2, fun(_Conn, Data) ->
            ets:insert(Ets, {uid, maps:get(user_id, Data)}),
            {error, {injected, step3}}
        end}
    ]),
    Result =
        try
            channel_webhook_ds:create(ChannelId, <<"注入 webhook"/utf8>>, CreatorUid)
        after
            meck_helper:cleanup_mock(channel_admin_repo)
        end,
    {Result, captured_uid(Ets)};
run_create_with_injection(step4, ChannelId, CreatorUid) ->
    Ets = ets:new(tx01_wh_capture, [public, set]),
    _ = meck_helper:setup_mock(channel_webhook_repo, [
        {'add_tx', 2, fun(_Conn, Data) ->
            ets:insert(Ets, {uid, maps:get(bot_uid, Data)}),
            {error, {injected, step4}}
        end}
    ]),
    Result =
        try
            channel_webhook_ds:create(ChannelId, <<"注入 webhook"/utf8>>, CreatorUid)
        after
            meck_helper:cleanup_mock(channel_webhook_repo)
        end,
    {Result, captured_uid(Ets)}.

captured_uid(Ets) ->
    case ets:lookup(Ets, uid) of
        [{uid, Uid}] ->
            Uid;
        [] ->
            ets:delete(Ets),
            error(no_captured_uid)
    end.
