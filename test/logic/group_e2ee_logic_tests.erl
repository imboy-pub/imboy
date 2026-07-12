-module(group_e2ee_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("group_role.hrl").

%%%===================================================================
%%% @doc 群级 E2EE 开关与 fail-closed 门测试（P0-B B4）
%%%
%%% 覆盖：仅群主可开 / 0→1 单向 / e2ee_mode=1 拒明文 /
%%% 群查询失败拒发（fail-closed）/ 非内容动作放行不查库。
%%%===================================================================

encrypted_e2ee() ->
    #{<<"iv">> => <<"aXY=">>, <<"ct">> => <<"Y3Q=">>}.

%% ===================================================================
%% set_e2ee_mode：权限与单向性
%% ===================================================================

set_e2ee_mode_rejects_disable_test_() ->
    ?TEST_SIMPLE(fun() ->
        %% 0→1 单向：任何非 1 的目标模式（含关闭）一律拒绝
        ?assertMatch({error, _}, group_logic:set_e2ee_mode(1001, 42, 0)),
        ?assertMatch({error, _}, group_logic:set_e2ee_mode(1001, 42, 2))
    end).

set_e2ee_mode_owner_only_test_() ->
    ?WITH_MECKS(
        [
            {group_member_ds, [
                {'find_by_gid_and_uid', 3, fun(42, 1001, <<"role">>) ->
                    %% 副群主也不行（edit 通道允许副群主，本开关更严格）
                    #{<<"role">> => ?ROLE_VICE_OWNER}
                end}
            ]}
        ],
        fun() ->
            ?assertMatch({error, _}, group_logic:set_e2ee_mode(1001, 42, 1))
        end
    ).

set_e2ee_mode_owner_enables_and_broadcasts_test_() ->
    ?WITH_MECKS(
        [
            {group_member_ds, [
                {'find_by_gid_and_uid', 3, fun(42, 1001, <<"role">>) ->
                    #{<<"role">> => ?ROLE_OWNER}
                end}
            ]},
            {group_ds, [
                {'update_by_id', 2, fun(42, Data) ->
                    ?assertEqual(1, maps:get(e2ee_mode, Data)),
                    {ok, 1}
                end},
                {'flush_e2ee_mode', 1, fun(42) -> ok end},
                {'member_uids', 1, fun(42) -> [1001, 1002] end}
            ]},
            {msg_s2c_ds, [
                {'send', 7, fun(1001, [1001, 1002], <<"group_e2ee_mode">>, _, _, Payload, save) ->
                    ?assertEqual(1, maps:get(<<"e2ee_mode">>, Payload)),
                    ok
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(ok, group_logic:set_e2ee_mode(1001, 42, 1)),
            %% 开关落库后必须清缓存 + 广播
            ?assertEqual(1, meck:num_calls(group_ds, flush_e2ee_mode, 1)),
            ?assertEqual(1, meck:num_calls(msg_s2c_ds, send, 7))
        end
    ).

%% ===================================================================
%% group_e2ee_gate：fail-closed 门
%% ===================================================================

gate_rejects_plaintext_when_required_test_() ->
    ?WITH_MECKS(
        [
            {group_ds, [
                {'e2ee_mode', 1, fun(42) -> {ok, 1} end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"encrypted_message_required">>},
                msg_c2g_logic:group_e2ee_gate(42, <<"text">>, <<>>, null, <<"{\"plain\":1}">>)
            ),
            %% message_edit 同为内容动作，同样拦截
            ?assertEqual(
                {error, <<"encrypted_message_required">>},
                msg_c2g_logic:group_e2ee_gate(
                    42, <<"text">>, <<"message_edit">>, null, <<"{\"plain\":1}">>
                )
            )
        end
    ).

gate_allows_encrypted_when_required_test_() ->
    ?WITH_MECKS(
        [
            {group_ds, [
                {'e2ee_mode', 1, fun(42) -> {ok, 1} end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                ok,
                msg_c2g_logic:group_e2ee_gate(
                    42, <<"text">>, <<>>, encrypted_e2ee(), <<"{\"e2ee\":true}">>
                )
            )
        end
    ).

gate_allows_plaintext_when_off_test_() ->
    ?WITH_MECKS(
        [
            {group_ds, [
                {'e2ee_mode', 1, fun(42) -> {ok, 0} end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                ok, msg_c2g_logic:group_e2ee_gate(42, <<"text">>, <<>>, null, <<"{\"plain\":1}">>)
            )
        end
    ).

%% fail-closed：群配置查询失败一律拒发，不降级放行
gate_fails_closed_on_query_error_test_() ->
    ?WITH_MECKS(
        [
            {group_ds, [
                {'e2ee_mode', 1, fun(42) -> {error, pg_down} end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, <<"group_e2ee_check_failed">>},
                msg_c2g_logic:group_e2ee_gate(42, <<"text">>, <<>>, null, <<"{\"plain\":1}">>)
            )
        end
    ).

%% 非内容动作（撤回/已读等）放行且零查库（热路径保护）
gate_skips_non_content_actions_test_() ->
    ?WITH_MECKS(
        [
            {group_ds, [
                {'e2ee_mode', 1, fun(_) -> {ok, 1} end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                ok,
                msg_c2g_logic:group_e2ee_gate(42, <<>>, <<"msg_revoke">>, null, <<"{}">>)
            ),
            ?assertEqual(0, meck:num_calls(group_ds, e2ee_mode, 1))
        end
    ).
