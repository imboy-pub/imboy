-module(bot_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc bot_ds TX-01 事务收敛测试（真库直连）
%%%
%%% create/1 三步写【建 user 行 → 标 account_type=3 → 绑 bot 元数据】已收进
%%% elib_pg:with_tx 单事务。本测试用 scratch 真库验证：
%%%   1. 故障注入矩阵：第 1/2/3 步分别注入失败（meck 对应 repo tx 函数返回
%%%      error）→ 直连 SQL 断言库中零孤儿（user 行、account_type 标记、bot 行）；
%%%   2. 幂等/并发：同 username 顺序重复与并发重复 → 恰一个实体；
%%%   3. 正常路径回归：成功创建后各表行数与内容正确。
%%%
%%% 注入方式：meck_helper:setup_mock 默认 [passthrough, ...]，仅覆盖被 expect
%%% 的函数，其余（含真库 SQL）走真实实现；注入 fun 先捕获本次要写入的 user id
%%% （供零孤儿断言精确定位该行），再返回 error。user/bot 行断言均直连 SQL。
%%%===================================================================

%% ===================================================================
%% 正常路径回归：成功创建后各表行数与内容正确
%% ===================================================================

create_persists_user_bot_and_account_type_test_() ->
    ?TEST_WITH_DB(fun() ->
        Username = unique_username(<<"ok">>),
        cleanup_username(Username),
        Data = #{
            name => <<"回归 Bot"/utf8>>,
            username => Username,
            owner_uid => 1,
            description => <<"TX-01 正常路径"/utf8>>,
            api_token => <<"tok_", Username/binary>>
        },
        {ok, #{<<"user_id">> := Uid}} = bot_ds:create(Data),
        %% user 行存在且 account_type=3、account=bot_<uid>
        {ok, [User]} =
            elib_pg:query(
                <<"SELECT id, account, account_type, nickname, status FROM ",
                    (user_repo:tablename())/binary, " WHERE id = $1">>,
                [Uid]
            ),
        ?assertEqual(Uid, maps:get(<<"id">>, User)),
        ?assertEqual(3, maps:get(<<"account_type">>, User)),
        ?assertEqual(<<"bot_", (integer_to_binary(Uid))/binary>>, maps:get(<<"account">>, User)),
        ?assertEqual(1, maps:get(<<"status">>, User)),
        %% bot 行存在且内容正确
        {ok, [Bot]} =
            elib_pg:query(
                <<"SELECT user_id, name, username, owner_uid, status, api_token FROM ",
                    (bot_repo:tablename())/binary, " WHERE user_id = $1">>,
                [Uid]
            ),
        ?assertEqual(<<"回归 Bot"/utf8>>, maps:get(<<"name">>, Bot)),
        ?assertEqual(Username, maps:get(<<"username">>, Bot)),
        ?assertEqual(1, maps:get(<<"owner_uid">>, Bot)),
        ?assertEqual(1, maps:get(<<"status">>, Bot)),
        ?assertEqual(<<"tok_", Username/binary>>, maps:get(<<"api_token">>, Bot)),
        cleanup_username(Username)
    end).

%% ===================================================================
%% 故障注入矩阵：第 1/2/3 步分别失败 → 整体回滚零孤儿
%% ===================================================================

%% 第 1 步失败：user_repo:create_tx 注入 error（user 行未写入）
create_step1_user_insert_failure_leaves_zero_orphans_test_() ->
    ?TEST_WITH_DB(fun() ->
        Username = unique_username(<<"s1">>),
        cleanup_username(Username),
        {Result, Uid} = run_create_with_injection(step1, Username),
        ?assertEqual({error, <<"创建 Bot 账号失败"/utf8>>}, Result),
        assert_zero_orphans(Uid, Username),
        cleanup_username(Username)
    end).

%% 第 2 步失败：user_repo:update_tx（account_type 标记）注入 error；
%% 第 1 步 insert 已在同事务内执行 → 必须随整体回滚，不留 user 行
create_step2_account_type_failure_leaves_zero_orphans_test_() ->
    ?TEST_WITH_DB(fun() ->
        Username = unique_username(<<"s2">>),
        cleanup_username(Username),
        {Result, Uid} = run_create_with_injection(step2, Username),
        ?assertEqual({error, <<"创建 Bot 账号失败"/utf8>>}, Result),
        assert_zero_orphans(Uid, Username),
        cleanup_username(Username)
    end).

%% 第 3 步失败：bot_repo:create_tx 注入 error → user 两写同事务回滚
create_step3_bot_bind_failure_leaves_zero_orphans_test_() ->
    ?TEST_WITH_DB(fun() ->
        Username = unique_username(<<"s3">>),
        cleanup_username(Username),
        {Result, Uid} = run_create_with_injection(step3, Username),
        ?assertEqual({error, <<"绑定 Bot 元数据失败"/utf8>>}, Result),
        assert_zero_orphans(Uid, Username),
        cleanup_username(Username)
    end).

%% 重试幂等：第 2 步失败整体回滚后，重跑（同参数）成功且无残留冲突
create_retry_after_step2_failure_succeeds_test_() ->
    ?TEST_WITH_DB(fun() ->
        Username = unique_username(<<"retry">>),
        cleanup_username(Username),
        {Result, Uid} = run_create_with_injection(step2, Username),
        ?assertEqual({error, <<"创建 Bot 账号失败"/utf8>>}, Result),
        assert_zero_orphans(Uid, Username),
        %% 重跑：无残留 id/account 冲突，成功
        {ok, #{<<"user_id">> := Uid2}} = bot_ds:create(bot_data(Username)),
        {ok, [#{<<"n">> := 1}]} =
            elib_pg:query(
                <<"SELECT count(*) AS n FROM ", (bot_repo:tablename())/binary,
                    " WHERE user_id = $1">>,
                [Uid2]
            ),
        cleanup_username(Username)
    end).

%% ===================================================================
%% 幂等/并发：同 username 重复请求只产生一个实体
%% ===================================================================

create_duplicate_username_sequential_keeps_single_entity_test_() ->
    ?TEST_WITH_DB(fun() ->
        Username = unique_username(<<"dup">>),
        cleanup_username(Username),
        {ok, #{<<"user_id">> := Uid1}} = bot_ds:create(bot_data(Username)),
        %% 重复请求：username 唯一约束在事务内拦截，整体回滚零孤儿
        ?assertEqual(
            {error, <<"Bot 调用名已被占用"/utf8>>},
            bot_ds:create(bot_data(Username))
        ),
        %% 恰一实体：bot 1 行；该 bot 的 user 1 行
        assert_single_entity(Username, Uid1),
        cleanup_username(Username)
    end).

%% 并发用例真库 + spawn 往返可能超过 eunit 默认 5s：timeout 须经
%% TEST_WITH_DB_TIMEOUT 放在 setup 体内（外包 {timeout, T, ?TEST_WITH_DB(...)}
%% 不生效——带 fixture 的 group 不下推 timeout，见 eunit_setup.hrl 注释）
create_duplicate_username_concurrent_exactly_one_entity_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        Username = unique_username(<<"conc">>),
        cleanup_username(Username),
        Parent = self(),
        Worker = fun() ->
            Parent ! {tx01_done, bot_ds:create(bot_data(Username))}
        end,
        spawn(Worker),
        spawn(Worker),
        Results = collect_results(2, []),
        OkCount = length([X || {ok, X} <- Results]),
        ?assertEqual(1, OkCount, {results, Results}),
        %% 恰一实体：bot 表恰 1 行，其 user 行恰 1 行且 account_type=3
        {ok, [#{<<"n">> := 1}]} =
            elib_pg:query(
                <<"SELECT count(*) AS n FROM ", (bot_repo:tablename())/binary,
                    " WHERE username = $1">>,
                [Username]
            ),
        {ok, [#{<<"uid">> := BotUid, <<"n">> := 1}]} =
            elib_pg:query(
                <<"SELECT b.user_id AS uid, count(*) AS n FROM ", (bot_repo:tablename())/binary,
                    " b JOIN ", (user_repo:tablename())/binary,
                    " u ON u.id = b.user_id WHERE b.username = $1 GROUP BY b.user_id">>,
                [Username]
            ),
        ?assertEqual(3, user_account_type(BotUid)),
        cleanup_username(Username)
    end).

%% ===================================================================
%% Internal — 数据准备与断言
%% ===================================================================

bot_data(Username) ->
    #{
        name => <<"TX-01 Bot"/utf8>>,
        username => Username,
        owner_uid => 1,
        %% bot.api_token 有 UNIQUE 约束且 bot_repo 默认空串 <<>>（非 NULL），
        %% 任一行 '' 残留都会让后续不传 api_token 的创建整体 23505——
        %% 测试逐用例给唯一 token（与生产 bot_logic:register 生成 token 对齐）
        api_token => <<"tok_", Username/binary>>
    }.

unique_username(Prefix) ->
    <<
        "tx01_",
        Prefix/binary,
        "_",
        (integer_to_binary(erlang:unique_integer([positive])))/binary
    >>.

%% 清理：先 bot 后 user（FK 顺序），幂等可重跑
cleanup_username(Username) ->
    {ok, Uids} =
        elib_pg:query(
            <<"SELECT user_id FROM ", (bot_repo:tablename())/binary, " WHERE username = $1">>,
            [Username]
        ),
    _ = elib_pg:query(
        <<"DELETE FROM ", (bot_repo:tablename())/binary, " WHERE username = $1">>,
        [Username]
    ),
    lists:foreach(
        fun(#{<<"user_id">> := Uid}) ->
            _ = elib_pg:query(
                <<"DELETE FROM ", (user_repo:tablename())/binary, " WHERE id = $1">>,
                [Uid]
            )
        end,
        Uids
    ),
    ok.

%% 零孤儿断言：user 行（含 account_type 标记）与 bot 行全查空（直连 SQL）
assert_zero_orphans(Uid, Username) ->
    {ok, UserRows} =
        elib_pg:query(
            <<"SELECT id, account_type FROM ", (user_repo:tablename())/binary,
                " WHERE id = $1 OR account = $2">>,
            [Uid, <<"bot_", (integer_to_binary(Uid))/binary>>]
        ),
    ?assertEqual([], UserRows, {orphan_user_rows, UserRows}),
    {ok, BotRows} =
        elib_pg:query(
            <<"SELECT user_id FROM ", (bot_repo:tablename())/binary,
                " WHERE user_id = $1 OR username = $2">>,
            [Uid, Username]
        ),
    ?assertEqual([], BotRows, {orphan_bot_rows, BotRows}),
    ok.

%% 恰一实体断言：bot 表恰 1 行（username），其 user 行恰 1 行且 account_type=3
assert_single_entity(Username, Uid) ->
    {ok, [#{<<"n">> := 1}]} =
        elib_pg:query(
            <<"SELECT count(*) AS n FROM ", (bot_repo:tablename())/binary, " WHERE username = $1">>,
            [Username]
        ),
    {ok, [#{<<"n">> := 1}]} =
        elib_pg:query(
            <<"SELECT count(*) AS n FROM ", (user_repo:tablename())/binary, " WHERE id = $1">>,
            [Uid]
        ),
    ?assertEqual(3, user_account_type(Uid)),
    ok.

user_account_type(Uid) ->
    {ok, [#{<<"account_type">> := T}]} =
        elib_pg:query(
            <<"SELECT account_type FROM ", (user_repo:tablename())/binary, " WHERE id = $1">>,
            [Uid]
        ),
    T.

%% 收齐 N 条结果立即返回；兜底 40s（外层 TEST_WITH_DB_TIMEOUT 60s 内）
collect_results(N, Acc) when N > 0 ->
    receive
        {tx01_done, Result} -> collect_results(N - 1, [Result | Acc])
    after 40000 ->
        lists:reverse([{tx01_timeout, N} | Acc])
    end;
collect_results(_, Acc) ->
    lists:reverse(Acc).

%% ===================================================================
%% 故障注入 helper：meck 只覆盖被 expect 的函数（passthrough 其余），
%% 注入 fun 先捕获本次要写入的 user id（供零孤儿断言定位），再返回 error。
%% 返回 {CreateResult, CapturedUid}。
%% ===================================================================

run_create_with_injection(step1, Username) ->
    Ets = ets:new(tx01_capture, [public, set]),
    _ = meck_helper:setup_mock(user_repo, [
        {'create_tx', 2, fun(_Conn, Data) ->
            ets:insert(Ets, {uid, maps:get(id, Data)}),
            {error, {injected, step1}}
        end}
    ]),
    Result =
        try
            bot_ds:create(bot_data(Username))
        after
            meck_helper:cleanup_mock(user_repo)
        end,
    Uid = captured_uid(Ets),
    {Result, Uid};
run_create_with_injection(step2, Username) ->
    Ets = ets:new(tx01_capture, [public, set]),
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
            bot_ds:create(bot_data(Username))
        after
            meck_helper:cleanup_mock(user_repo)
        end,
    Uid = captured_uid(Ets),
    {Result, Uid};
run_create_with_injection(step3, Username) ->
    Ets = ets:new(tx01_capture, [public, set]),
    _ = meck_helper:setup_mock(bot_repo, [
        {'create_tx', 2, fun(_Conn, Data) ->
            ets:insert(Ets, {uid, maps:get(user_id, Data)}),
            {error, {injected, step3}}
        end}
    ]),
    Result =
        try
            bot_ds:create(bot_data(Username))
        after
            meck_helper:cleanup_mock(bot_repo)
        end,
    Uid = captured_uid(Ets),
    {Result, Uid}.

captured_uid(Ets) ->
    case ets:lookup(Ets, uid) of
        [{uid, Uid}] ->
            Uid;
        [] ->
            ets:delete(Ets),
            error(no_captured_uid)
    end.
