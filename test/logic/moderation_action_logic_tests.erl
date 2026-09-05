-module(moderation_action_logic_tests).

%% R-02：处置动作 logic 单测。mock 边界：report_ticket_ds（case 存在性）、
%% moderation_action_repo（审计行持久化）、group_member_logic（群处置
%% primitives）、message_ds/elib_id/elib_retry_config（warning 通知）、
%% elib_pg:with_tx（透传 mock 连接）。jsone/elib_dt/ec_cnv 用真实现。

-include_lib("eunit/include/eunit.hrl").

-define(ADM, 9001).
-define(CASE, 510001).

%%%===================================================================
%%% Test fixtures
%%%===================================================================

setup_mocks(Opts) ->
    meck:new(report_ticket_ds, [passthrough, no_link]),
    meck:new(moderation_action_repo, [no_link]),
    meck:new(group_member_logic, [no_link]),
    meck:new(message_ds, [no_link]),
    meck:new(elib_id, [no_link]),
    meck:new(elib_retry_config, [no_link]),
    meck:new(elib_pg, [no_link]),
    meck:expect(elib_pg, with_tx, fun(F) -> F(mock_conn) end),
    meck:expect(elib_id, gen, fun(_Prefix) -> 1799000123456789 end),
    meck:expect(elib_retry_config, intervals, fun(<<"notice">>) -> [60] end),
    meck:expect(message_ds, assemble_msg, fun(MsType, _F, Uid, Payload, MsgId, _E, Action, _E2) ->
        #{ms_type => MsType, to => Uid, payload => Payload, id => MsgId, action => Action}
    end),
    meck:expect(message_ds, send_next, fun(_Uid, _MsgId, _Msg2, _MsLi, _DiLi, _Include) ->
        ok
    end),
    case lists:member(case_row, Opts) of
        true ->
            meck:expect(report_ticket_ds, find_by_id, fun(_Id) ->
                #{<<"id">> => ?CASE, <<"status">> => 2, <<"target_type">> => <<"group">>}
            end);
        false ->
            meck:expect(report_ticket_ds, find_by_id, fun(_Id) -> #{} end)
    end,
    ok.

teardown_mocks(_) ->
    meck:unload(report_ticket_ds),
    meck:unload(moderation_action_repo),
    meck:unload(group_member_logic),
    meck:unload(message_ds),
    meck:unload(elib_id),
    meck:unload(elib_retry_config),
    meck:unload(elib_pg),
    ok.

insert_expect(Status) ->
    meck:expect(moderation_action_repo, insert_tx, fun(_Conn, A) ->
        ?assertEqual(Status, maps:get(status, A)),
        {ok, maps:put(<<"id">>, 690001, A)}
    end).

%%%===================================================================
%%% Tests
%%%===================================================================

case_not_found_test_() ->
    {"case 不存在 → 举报单不存在（fail-closed）", fun() ->
        setup_mocks([]),
        meck:expect(moderation_action_repo, insert_tx, fun(_C, _A) -> {ok, #{}} end),
        R = moderation_action_logic:execute(?ADM, ?CASE, <<"warning">>, 77, #{reason => <<"r">>}),
        ?assertMatch({error, _}, R),
        ?assert(meck:called(report_ticket_ds, find_by_id, [?CASE])),
        teardown_mocks(ok)
    end}.

unsupported_action_test_() ->
    {"content_removal 当前 primitives 不支持 → 显式 unsupported 且不落审计行", fun() ->
        setup_mocks([case_row]),
        insert_expect(<<"executed">>),
        R = moderation_action_logic:execute(
            ?ADM,
            ?CASE,
            <<"content_removal">>,
            77,
            #{reason => <<"r">>}
        ),
        ?assertMatch({error, _}, R),
        ?assertNot(meck:called(moderation_action_repo, insert_tx, ['_', '_'])),
        teardown_mocks(ok)
    end}.

duplicate_action_rejected_test_() ->
    {"同 case 同 action 已 executed → 重复执行被拒绝且不再落行", fun() ->
        setup_mocks([case_row]),
        meck:expect(
            moderation_action_repo,
            has_executed_same_action,
            fun(_C, _A, _U) -> {ok, true} end
        ),
        meck:expect(moderation_action_repo, insert_tx, fun(_C, _A) -> {ok, #{}} end),
        R = moderation_action_logic:execute(?ADM, ?CASE, <<"warning">>, 77, #{reason => <<"r">>}),
        ?assertMatch({error, _}, R),
        ?assertNot(meck:called(moderation_action_repo, insert_tx, ['_', '_'])),
        teardown_mocks(ok)
    end}.

warning_executed_test_() ->
    {"warning：通知送达 + executed 审计行", fun() ->
        setup_mocks([case_row]),
        meck:expect(
            moderation_action_repo,
            has_executed_same_action,
            fun(_C, _A, _U) -> {ok, false} end
        ),
        insert_expect(<<"executed">>),
        R = moderation_action_logic:execute(?ADM, ?CASE, <<"warning">>, 77, #{
            reason => <<"违规警告"/utf8>>
        }),
        ?assertMatch({ok, _}, R),
        ?assert(meck:called(message_ds, send_next, ['_', '_', '_', '_', '_', '_'])),
        teardown_mocks(ok)
    end}.

group_mute_primitive_failure_leaves_failed_row_test_() ->
    {"group_mute primitive 拒绝 → failed 审计行落库（case truthful）+ 显式错误", fun() ->
        setup_mocks([case_row]),
        meck:expect(
            moderation_action_repo,
            has_executed_same_action,
            fun(_C, _A, _U) -> {ok, false} end
        ),
        meck:expect(group_member_logic, mute, fun(_Adm, _Gid, _Uid, _Sec) ->
            {error, <<"no permission">>}
        end),
        meck:expect(moderation_action_repo, insert_tx, fun(_C, A) ->
            ?assertEqual(<<"failed">>, maps:get(status, A)),
            ?assertNotEqual(<<>>, maps:get(fail_reason, A)),
            {ok, maps:put(<<"id">>, 690002, A)}
        end),
        R = moderation_action_logic:execute(
            ?ADM,
            ?CASE,
            <<"group_mute">>,
            77,
            #{
                reason => <<"r">>,
                gid => 42,
                duration_minutes => 60
            }
        ),
        ?assertMatch({error, _}, R),
        teardown_mocks(ok)
    end}.

group_mute_executed_test_() ->
    {"group_mute：primitives 调用（分钟→秒）+ executed 行含 end_at", fun() ->
        setup_mocks([case_row]),
        meck:expect(
            moderation_action_repo,
            has_executed_same_action,
            fun(_C, _A, _U) -> {ok, false} end
        ),
        meck:expect(group_member_logic, mute, fun(Adm, Gid, Uid, Sec) ->
            ?assertEqual(?ADM, Adm),
            ?assertEqual(42, Gid),
            ?assertEqual(77, Uid),
            ?assertEqual(3600, Sec),
            ok
        end),
        meck:expect(moderation_action_repo, insert_tx, fun(_C, A) ->
            ?assertEqual(<<"executed">>, maps:get(status, A)),
            ?assert(maps:get(end_at, A) =/= null),
            {ok, maps:put(<<"id">>, 690003, A)}
        end),
        R = moderation_action_logic:execute(
            ?ADM,
            ?CASE,
            <<"group_mute">>,
            77,
            #{
                reason => <<"r">>,
                gid => 42,
                duration_minutes => 60
            }
        ),
        ?assertMatch({ok, _}, R),
        teardown_mocks(ok)
    end}.

group_kick_executed_test_() ->
    {"group_kick：admin_kick 语义（Uid, Gid, AdminUid）+ executed 行", fun() ->
        setup_mocks([case_row]),
        meck:expect(
            moderation_action_repo,
            has_executed_same_action,
            fun(_C, _A, _U) -> {ok, false} end
        ),
        meck:expect(group_member_logic, admin_kick, fun(Uid, Gid, Adm) ->
            ?assertEqual(77, Uid),
            ?assertEqual(42, Gid),
            ?assertEqual(?ADM, Adm),
            ok
        end),
        insert_expect(<<"executed">>),
        R = moderation_action_logic:execute(
            ?ADM,
            ?CASE,
            <<"group_kick">>,
            77,
            #{reason => <<"r">>, gid => 42}
        ),
        ?assertMatch({ok, _}, R),
        teardown_mocks(ok)
    end}.

reject_is_explicit_no_action_test_() ->
    {"reject：显式不处置结论，executed 行、零 primitive 副作用", fun() ->
        setup_mocks([case_row]),
        meck:expect(
            moderation_action_repo,
            has_executed_same_action,
            fun(_C, _A, _U) -> {ok, false} end
        ),
        insert_expect(<<"executed">>),
        R = moderation_action_logic:execute(?ADM, ?CASE, <<"reject">>, 0, #{
            reason => <<"证据不足"/utf8>>
        }),
        ?assertMatch({ok, _}, R),
        ?assertNot(meck:called(group_member_logic, mute, ['_', '_', '_', '_'])),
        teardown_mocks(ok)
    end}.

reverse_happy_test_() ->
    {"reverse：executed → mark_reversed 返回 1 → 状态翻转", fun() ->
        setup_mocks([case_row]),
        meck:expect(moderation_action_repo, find_by_id, fun(690003) ->
            {ok, #{
                <<"id">> => 690003,
                <<"action">> => <<"warning">>,
                <<"status">> => <<"executed">>,
                <<"target_uid">> => 77
            }}
        end),
        meck:expect(
            moderation_action_repo,
            mark_reversed,
            fun(_Id, _Adm, _Reason) -> {ok, 1} end
        ),
        R = moderation_action_logic:reverse(?ADM, 690003, <<"误判"/utf8>>),
        ?assertMatch({ok, _}, R),
        teardown_mocks(ok)
    end}.

reverse_rejects_non_executed_test_() ->
    {"reverse：failed/reversed 行拒绝撤销", fun() ->
        setup_mocks([case_row]),
        meck:expect(moderation_action_repo, find_by_id, fun(690004) ->
            {ok, #{
                <<"id">> => 690004,
                <<"action">> => <<"reject">>,
                <<"status">> => <<"failed">>,
                <<"target_uid">> => 0
            }}
        end),
        R = moderation_action_logic:reverse(?ADM, 690004, <<"r">>),
        ?assertMatch({error, _}, R),
        teardown_mocks(ok)
    end}.
