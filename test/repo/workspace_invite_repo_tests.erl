-module(workspace_invite_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% 工作区团队码 T2.2 — workspace_invite_repo 单元测试（meck DB 层，无数据库依赖）
%%% 覆盖：团队码字符集/长度性质、rand 边界、add_tx 参数化+23505 归一、
%%% find_active_by_code_tx 三态、revoke_tx / revoke_active_by_ws_tx 条件更新。
%%%
%%% ⚠️ ?WITH_MECKS 的 TestFun 必须直接写断言：返回 {Desc, fun} 列表会被
%%% 宏内 ?_test 包装吞掉——整个 test_ 判单个 ok、内层用例静默空转
%%% （2026-08-31 T2.7 对端验收实证：binary:uppercase undef 只有真机 500
%%% 才暴露，空转测试全绿）。勿再引入列表形态。

-define(WS_ID, 800001).
-define(UID, 900001).

%% ===================================================================
%% generate_invite_code/0：字符集 / 长度性质
%% ===================================================================

generate_invite_code_charset_and_length_test_() ->
    ?TEST_SIMPLE(fun() ->
        Codes = [workspace_invite_repo:generate_invite_code() || _ <- lists:seq(1, 200)],
        lists:foreach(
            fun(Code) ->
                ?assertEqual(8, byte_size(Code)),
                %% 仅 A-Z（去 I/O）与 2-9（排除 0/O/1/I 混淆字符）
                ?assertMatch({match, _}, re:run(Code, <<"^[A-HJ-NP-Z2-9]{8}$">>))
            end,
            Codes
        )
    end).

generate_invite_code_excludes_confusable_chars_test_() ->
    ?TEST_SIMPLE(fun() ->
        Codes = [workspace_invite_repo:generate_invite_code() || _ <- lists:seq(1, 200)],
        lists:foreach(
            fun(Code) ->
                ?assertEqual(nomatch, binary:match(Code, [<<"0">>, <<"1">>, <<"O">>, <<"I">>]))
            end,
            Codes
        )
    end).

generate_invite_code_boundary_position_test_() ->
    %% rand:uniform(32) 恒返回 32（末位下标）→ 每位取字符集最后一个字符 '9'
    CharsetSize = byte_size(<<"ABCDEFGHJKLMNPQRSTUVWXYZ23456789">>),
    ?WITH_MECKS(
        [
            {rand, [
                {'uniform', 1, fun(Arg) ->
                    ?assertEqual(CharsetSize, Arg),
                    CharsetSize
                end}
            ]}
        ],
        fun() ->
            %% last charset position picked per char
            begin
                ?assertEqual(<<"99999999">>, workspace_invite_repo:generate_invite_code()),
                ?assertEqual(8, meck:num_calls(rand, uniform, 1)),
                ok
            end
        end
    ).

%% ===================================================================
%% tablename/0
%% ===================================================================

tablename_returns_workspace_invite_table_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(<<"workspace_invite">>, workspace_invite_repo:tablename())
    end).

%% ===================================================================
%% add_tx/5：全参数化 + RETURNING + 23505 归一 code_conflict
%% ===================================================================

add_tx_inserts_parameterized_row_with_returning_test_() ->
    ?WITH_MECKS(
        [
            {elib_tsid, [
                {'generate', 1, fun(workspace_invite) -> 902001 end}
            ]},
            {elib_dt, [
                {'now', 0, fun() -> <<"2036-01-01T00:00:00Z">> end}
            ]},
            {elib_pg, [
                {'query', 3, fun(_Conn, Sql, Params) ->
                    SqlBin = iolist_to_binary(Sql),
                    ?assert(re:run(SqlBin, <<"INSERT INTO workspace_invite">>) =/= nomatch),
                    ?assert(re:run(SqlBin, <<"RETURNING id, workspace_id, code">>) =/= nomatch),
                    %% created_at/updated_at 用 $6（now），不复用 $5（expires_at）
                    ?assert(re:run(SqlBin, <<"\\$5, 'active', \\$6, \\$6">>) =/= nomatch),
                    ?assertEqual(6, length(Params)),
                    ?assertEqual(
                        [
                            902001,
                            ?WS_ID,
                            <<"ABCD2345">>,
                            ?UID,
                            <<"2036-01-01">>,
                            <<"2036-01-01T00:00:00Z">>
                        ],
                        Params
                    ),
                    {ok, [#{<<"id">> => 902001, <<"code">> => <<"ABCD2345">>}]}
                end}
            ]}
        ],
        fun() ->
            %% insert returns new row (parameterized $1..$6, 审计列=now)
            begin
                ?assertMatch(
                    {ok, #{<<"id">> := 902001, <<"code">> := <<"ABCD2345">>}},
                    workspace_invite_repo:add_tx(
                        fake_conn, ?WS_ID, <<"ABCD2345">>, ?UID, <<"2036-01-01">>
                    )
                ),
                ok
            end
        end
    ).

add_tx_normalizes_unique_violation_test_() ->
    ?WITH_MECKS(
        [
            {elib_tsid, [
                {'generate', 1, fun(workspace_invite) -> 902002 end}
            ]},
            {elib_dt, [
                {'now', 0, fun() -> <<"2036-01-01T00:00:00Z">> end}
            ]},
            {elib_pg, [
                {'query', 3, fun(_Conn, _Sql, _Params) ->
                    {error, {pgsql_error, #{code => <<"23505">>}}}
                end}
            ]}
        ],
        fun() ->
            %% unique violation normalized to code_conflict
            begin
                ?assertEqual(
                    {error, code_conflict},
                    workspace_invite_repo:add_tx(
                        fake_conn, ?WS_ID, <<"ABCD2345">>, ?UID, <<"2036-01-01">>
                    )
                ),
                ok
            end
        end
    ).

%% ===================================================================
%% find_active_by_code_tx/2：not_found | {ok, Map} | {error, _}
%% ===================================================================

find_active_by_code_tx_selects_active_with_expired_flag_test_() ->
    %% 哨兵经进程字典传递（eunit generator 与用例执行异进程，Self 消息
    %% 收不到；mock fun 与用例体同在执行进程，put/get 可靠）
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 3, fun(_Conn, Sql, [Code]) ->
                    SqlBin = iolist_to_binary(Sql),
                    ?assert(
                        re:run(SqlBin, <<"WHERE code = \\$1 AND status = 'active'">>) =/= nomatch
                    ),
                    ?assert(re:run(SqlBin, <<"expires_at < CURRENT_TIMESTAMP">>) =/= nomatch),
                    put(t_find_code, Code),
                    {ok, [
                        #{
                            <<"id">> => 902003,
                            <<"workspace_id">> => ?WS_ID,
                            <<"code">> => Code,
                            <<"expired">> => false
                        }
                    ]}
                end}
            ]}
        ],
        fun() ->
            %% active code row carries expired flag + code param
            begin
                ?assertMatch(
                    {ok, #{<<"workspace_id">> := ?WS_ID, <<"expired">> := false}},
                    workspace_invite_repo:find_active_by_code_tx(fake_conn, <<"ABCD2345">>)
                ),
                ?assertEqual(<<"ABCD2345">>, erase(t_find_code)),
                ok
            end
        end
    ).

find_active_by_code_tx_not_found_test_() ->
    ?WITH_MECKS(
        [
            {elib_pg, [
                {'query', 3, fun(_Conn, _Sql, _Params) -> {ok, []} end}
            ]}
        ],
        fun() ->
            %% no active row is not_found
            begin
                ?assertEqual(
                    not_found,
                    workspace_invite_repo:find_active_by_code_tx(fake_conn, <<"ZZZZ9999">>)
                ),
                ok
            end
        end
    ).

%% ===================================================================
%% revoke_tx/3：条件更新（workspace_id + code + status=active）
%% ===================================================================

revoke_tx_updates_active_only_test_() ->
    ?WITH_MECKS(
        [
            {elib_dt, [
                {'now', 0, fun() -> <<"2036-01-01T00:00:00Z">> end}
            ]},
            {elib_pg, [
                {'execute', 3, fun(_Conn, Sql, Params) ->
                    SqlBin = iolist_to_binary(Sql),
                    ?assert(re:run(SqlBin, <<"UPDATE workspace_invite">>) =/= nomatch),
                    ?assert(
                        re:run(
                            SqlBin,
                            <<"WHERE workspace_id = \\$2 AND code = \\$3 AND status = 'active'">>
                        ) =/= nomatch
                    ),
                    ?assertEqual([<<"2036-01-01T00:00:00Z">>, ?WS_ID, <<"ABCD2345">>], Params),
                    {ok, 1}
                end}
            ]}
        ],
        fun() ->
            %% revoke updates active code only (parameterized)
            begin
                ?assertEqual(
                    ok,
                    workspace_invite_repo:revoke_tx(fake_conn, ?WS_ID, <<"ABCD2345">>)
                ),
                ok
            end
        end
    ).

revoke_tx_not_active_is_idempotent_error_test_() ->
    ?WITH_MECKS(
        [
            {elib_dt, [
                {'now', 0, fun() -> <<"2036-01-01T00:00:00Z">> end}
            ]},
            {elib_pg, [
                {'execute', 3, fun(_Conn, _Sql, _Params) -> {ok, 0} end}
            ]}
        ],
        fun() ->
            %% revoked/unknown code returns not_active
            begin
                ?assertEqual(
                    {error, not_active},
                    workspace_invite_repo:revoke_tx(fake_conn, ?WS_ID, <<"ABCD2345">>)
                ),
                ok
            end
        end
    ).

%% ===================================================================
%% revoke_active_by_ws_tx/2：按工作区撤全部 active 码
%% （generate 前置撤旧 + Owner 主动撤销端点共用；幂等 {ok, 0}）
%% ===================================================================

revoke_active_by_ws_tx_test_() ->
    [
        ?WITH_MECKS(
            [
                {elib_dt, [
                    {'now', 0, fun() -> <<"2036-01-01T00:00:00Z">> end}
                ]},
                {elib_pg, [
                    {'execute', 3, fun(_Conn, Sql, Params) ->
                        SqlBin = iolist_to_binary(Sql),
                        ?assert(re:run(SqlBin, <<"UPDATE workspace_invite">>) =/= nomatch),
                        ?assert(
                            re:run(
                                SqlBin,
                                <<"WHERE workspace_id = \\$2 AND status = 'active'">>
                            ) =/= nomatch
                        ),
                        ?assertEqual([<<"2036-01-01T00:00:00Z">>, ?WS_ID], Params),
                        {ok, 1}
                    end}
                ]}
            ],
            fun() ->
                %% revokes all active codes of workspace (parameterized)
                ?assertEqual(
                    {ok, 1},
                    workspace_invite_repo:revoke_active_by_ws_tx(fake_conn, ?WS_ID)
                ),
                ok
            end
        ),
        ?WITH_MECKS(
            [
                {elib_dt, [
                    {'now', 0, fun() -> <<"2036-01-01T00:00:00Z">> end}
                ]},
                {elib_pg, [
                    {'execute', 3, fun(_Conn, _Sql, _Params) -> {ok, 0} end}
                ]}
            ],
            fun() ->
                %% no active codes is idempotent {ok, 0}
                ?assertEqual(
                    {ok, 0},
                    workspace_invite_repo:revoke_active_by_ws_tx(fake_conn, ?WS_ID)
                ),
                ok
            end
        )
    ].
