-module(moya_assignment_submission_payload_tests).
%%%
% moya_assignment_logic:submission_created/6 响应契约单元测试
% 2026-09-11：响应补 submitted_at（Rfc3339；moya 此前兜底空串）。
% A1-D12（2026-09-18）：重放 ai_status 反映 AI 草稿现值（此前恒 queued）。
%%%

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

created_response_contains_rfc3339_submitted_at_test() ->
    Payload = moya_assignment_logic:submission_created(
        1001, 2002, 3003, 1, true, #{<<"submitted_at">> => <<"2026-09-11T08:30:00.000Z">>}
    ),
    ?assertEqual(<<"1001">>, maps:get(<<"submission_id">>, Payload)),
    ?assertEqual(<<"2002">>, maps:get(<<"assignment_id">>, Payload)),
    ?assertEqual(<<"3003">>, maps:get(<<"learner_id">>, Payload)),
    ?assertEqual(1, maps:get(<<"attempt_no">>, Payload)),
    ?assertEqual(
        <<"2026-09-11T08:30:00.000Z">>, maps:get(<<"submitted_at">>, Payload)
    ),
    ?assertEqual(false, maps:get(<<"idempotent_replayed">>, Payload)).

idempotent_replay_carries_same_submitted_at_test() ->
    Payload = moya_assignment_logic:submission_created(
        1001, 2002, 3003, 2, false, #{<<"submitted_at">> => <<"2026-09-11T08:30:00.000Z">>}
    ),
    ?assertEqual(
        <<"2026-09-11T08:30:00.000Z">>, maps:get(<<"submitted_at">>, Payload)
    ),
    ?assertEqual(true, maps:get(<<"idempotent_replayed">>, Payload)).

missing_submitted_at_falls_back_to_null_test() ->
    %% 行内无 submitted_at（异常输入）→ null，不编造时间
    Payload = moya_assignment_logic:submission_created(1001, 2002, 3003, 1, true, #{}),
    ?assertEqual(null, maps:get(<<"submitted_at">>, Payload)).

integer_ms_submitted_at_converts_to_rfc3339_test() ->
    %% 防御分支：若上游改为整型毫秒，elib_dt:rfc3339_or_null 走 elib_dt:to_rfc3339
    Payload = moya_assignment_logic:submission_created(
        1001, 2002, 3003, 1, true, #{<<"submitted_at">> => 1788121800000}
    ),
    SubmittedAt = maps:get(<<"submitted_at">>, Payload),
    ?assert(is_binary(SubmittedAt)),
    ?assertMatch(<<"2026-", _/binary>>, SubmittedAt),
    ok.

%%%===================================================================
%%% A1-D12：重放 ai_status 反映 AI 草稿现值（此前恒 queued）
%%%===================================================================

replay_ai_status_reflects_current_draft_test() ->
    %% 重放时 finish_create 注入 crd 现值 → 响应透传（不再恒 queued）
    Payload = moya_assignment_logic:submission_created(
        1001,
        2002,
        3003,
        1,
        false,
        #{
            <<"submitted_at">> => <<"2026-09-11T08:30:00.000Z">>,
            <<"ai_status">> => <<"succeeded">>
        }
    ),
    ?assertEqual(<<"succeeded">>, maps:get(<<"ai_status">>, Payload)),
    ?assertEqual(true, maps:get(<<"idempotent_replayed">>, Payload)).

replay_ai_status_none_without_active_draft_test() ->
    %% 无草稿行（finish_create 查得 undefined → none；teacher 队列同口径）
    Payload = moya_assignment_logic:submission_created(
        1001, 2002, 3003, 1, false, #{<<"ai_status">> => <<"none">>}
    ),
    ?assertEqual(<<"none">>, maps:get(<<"ai_status">>, Payload)).

replay_ai_status_defaults_queued_without_key_test() ->
    %% 行内无 ai_status 键（直测/兜底）→ 保持旧行为 queued
    Payload = moya_assignment_logic:submission_created(1001, 2002, 3003, 1, false, #{}),
    ?assertEqual(<<"queued">>, maps:get(<<"ai_status">>, Payload)).

created_ai_status_always_queued_test() ->
    %% 新建（本事务刚入队）恒 queued——即使行内意外携带 ai_status 也不泄漏
    Payload = moya_assignment_logic:submission_created(
        1001, 2002, 3003, 1, true, #{<<"ai_status">> => <<"succeeded">>}
    ),
    ?assertEqual(<<"queued">>, maps:get(<<"ai_status">>, Payload)).

%%%===================================================================
%%% A1-D12：重放路径全链（create_submission → finish_create 重放分支）
%%%===================================================================

replay_full_chain_reads_draft_current_status_test_() ->
    %% meck repo：重放行 created=false + AI 草稿现值 succeeded →
    %% 响应 ai_status=succeeded（RED：现实现硬编码 queued，客户端显示错态）
    ?WITH_MECKS(replay_mocks(<<"succeeded">>), fun() ->
        %% create_in_tx 入参行含 elib_tsid:generate()，需先初始化（幂等）
        _ = elib_tsid:init(#{dc_id => 1, node_id => 1}),
        {ok, Payload} = moya_assignment_logic:create_submission(
            978001, 979001, <<"idem-key-8c">>, replay_body()
        ),
        ?assertEqual(<<"succeeded">>, maps:get(<<"ai_status">>, Payload)),
        ?assertEqual(true, maps:get(<<"idempotent_replayed">>, Payload)),
        receive
            {ai_status_queried, 979501} -> ok
        after 0 -> ?assert(false, "replay must query draft status")
        end
    end).

replay_full_chain_no_draft_row_test_() ->
    %% 无草稿行（ai_draft_status_tx → undefined）→ none
    ?WITH_MECKS(replay_mocks(undefined), fun() ->
        _ = elib_tsid:init(#{dc_id => 1, node_id => 1}),
        {ok, Payload} = moya_assignment_logic:create_submission(
            978001, 979001, <<"idem-key-8c">>, replay_body()
        ),
        ?assertEqual(<<"none">>, maps:get(<<"ai_status">>, Payload))
    end).

replay_body() ->
    #{
        <<"learner_id">> => integer_to_binary(974001),
        <<"assets">> => [
            #{
                <<"attachment_id">> => <<"980001">>,
                <<"kind">> => <<"practice_video">>,
                <<"sort_order">> => 0
            }
        ]
    }.

replay_mocks(AiStatus) ->
    Scope = #{
        <<"learner_id">> => 974001,
        <<"task_status">> => 1,
        <<"task_deadline">> => <<"2099-01-01T00:00:00Z">>,
        <<"group_id">> => 973201
    },
    [
        {moya_context_repo, [
            {'assignment_scope', 1, fun(_Aid) -> {ok, Scope} end}
        ]},
        {moya_acl, [
            {'resolve_guardian', 3, fun(_Uid, _Lid, _Submit) -> {ok, #{}} end}
        ]},
        {moya_submission_repo, [
            {'validate_assets', 2, fun(_Uid, _Assets) -> {ok, []} end},
            {'lock_assignment_tx', 2, fun(_Conn, Aid) -> {ok, Aid} end},
            {'next_attempt_tx', 2, fun(_Conn, _Aid) -> {ok, 1} end},
            {'create_idempotent_tx', 2, fun(_Conn, _Row) ->
                {ok, #{
                    <<"id">> => 979501,
                    <<"attempt_no">> => 1,
                    <<"submitted_at">> => <<"2026-09-11T08:30:00.000Z">>,
                    <<"created">> => false
                }}
            end},
            {'ai_draft_status_tx', 2, fun(_Conn, Sid) ->
                self() ! {ai_status_queried, Sid},
                {ok, AiStatus}
            end}
        ]},
        {elib_pg, [
            {'with_tx', 2, fun(Tx, _Opts) -> Tx(fake_conn) end}
        ]}
    ].
