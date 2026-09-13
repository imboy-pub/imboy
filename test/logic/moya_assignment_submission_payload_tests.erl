-module(moya_assignment_submission_payload_tests).
%%%
% moya_assignment_logic:submission_created/6 响应契约单元测试
% 2026-09-11：响应补 submitted_at（Rfc3339；moya 此前兜底空串）。
%%%

-include_lib("eunit/include/eunit.hrl").

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
