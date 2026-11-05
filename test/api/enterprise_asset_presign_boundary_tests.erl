%%% Handler boundary only: transaction/provider are doubles, not HTTP acceptance.
%%% V3-14 S3-only：签发面 = attachment_storage:session_location +
%%% presign_put_snapshot（s3_put descriptor）；无 JWT 能力、无网关正文路径。
-module(enterprise_asset_presign_boundary_tests).
-include_lib("eunit/include/eunit.hrl").

postcommit_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun direct_put_descriptor/0,
        fun location_unavailable/0,
        fun presign_failure/0,
        fun legacy_cached_url/0,
        fun fresh_registration/0,
        fun registration_retry_signs_once/0,
        fun governance_null_times/0,
        fun governance_pg_times/0,
        fun purge_uses_retry_transaction/0,
        fun hold_keeps_ordinary_transaction/0,
        fun purge_digest_conflict_preserves_response/0,
        fun transaction_failure/0,
        fun confirmed_no_capability/0,
        fun terminal_conflict/0,
        fun expired_recovery/0,
        fun recovery_unavailable/0,
        fun authorization_revoked/0,
        fun recovery_commit_unknown/0
    ]}.
setup() ->
    _ = application:load(imboy),
    Prior = application:get_env(imboy, base_url),
    Modules = [
        cowboy_req,
        elib_param,
        elib_pg,
        enterprise_internal_idempotency,
        enterprise_internal_boundary,
        enterprise_asset_logic,
        enterprise_asset_retention_logic,
        attachment_storage
    ],
    try
        [meck:new(M, [no_link]) || M <- Modules],
        meck:expect(cowboy_req, method, fun(_) -> <<"POST">> end),
        meck:expect(cowboy_req, path, fun(_) -> <<"/api/internal/v1/files/presign">> end),
        meck:expect(cowboy_req, header, fun(_, _) -> <<"idempotency-key">> end),
        meck:expect(cowboy_req, reply, fun(Status, Headers, Body, Req) ->
            Req#{status => Status, headers => Headers, body => Body}
        end),
        meck:expect(elib_param, post, fun(_) -> #{} end),
        meck:expect(elib_pg, with_tx, fun(F) -> F(fixture_conn) end),
        meck:expect(elib_pg, with_physical_guard_retry_tx, fun(F) -> F(fixture_conn) end),
        meck:expect(enterprise_internal_idempotency, request_digest, fun(_, _, _) ->
            {ok, <<"digest">>}
        end),
        meck:expect(enterprise_internal_idempotency, replay_header, fun() ->
            {<<"replayed">>, <<"true">>}
        end),
        meck:expect(enterprise_internal_boundary, enforce, fun(_, _, <<"INT-07">>, undefined) ->
            ok
        end),
        meck:expect(enterprise_internal_boundary, error_code, fun(organization_boundary_violation) ->
            <<"organization_boundary_violation">>
        end),
        meck:expect(enterprise_asset_logic, presign_recovery_tx, fun(_, _, _) -> {ok, reg()} end),
        meck:expect(attachment_storage, presign_put_snapshot, fun(_I, _Mime, _Ttl) ->
            {ok, <<"http://s3.example.invalid/fixture/fixture/key?X-Amz-Signature=stub">>}
        end),
        application:set_env(imboy, base_url, <<"https://files.example.invalid/">>),
        {Modules, Prior}
    catch
        Class:Reason:Stack ->
            cleanup({Modules, Prior}),
            erlang:raise(Class, Reason, Stack)
    end.
cleanup({Modules, Prior}) ->
    [catch meck:unload(M) || M <- Modules],
    case Prior of
        undefined -> application:unset_env(imboy, base_url);
        {ok, V} -> application:set_env(imboy, base_url, V)
    end,
    erase(recovery_tx_seen),
    erase(stored_json_body),
    ok.
reg() ->
    #{
        <<"session_state">> => <<"registered">>,
        <<"client_upload_id">> => <<"00000000-0000-7000-8000-000000000001">>,
        <<"bucket">> => <<"fixture">>,
        <<"object_key">> => <<"fixture/key">>,
        <<"mime_type">> => <<"text/plain">>,
        <<"expires_at">> => 9999999999,
        <<"storage_snapshot">> => #{
            driver => garage,
            container => <<"fixture">>,
            key => <<"fixture/key">>,
            endpoint => <<"http://s3.example.invalid">>
        }
    }.
replay(Body) ->
    meck:expect(enterprise_internal_idempotency, begin_tx, fun(_, _, _, _, _) ->
        {ok, replay, #{response_code => 200, response_body => jsone:encode(Body)}}
    end),
    invoke().
invoke() ->
    {ok, R, _} = enterprise_asset_handler:init(#{}, #{action => presign, enterprise_internal => #{}}),
    R.
assert_error(Status, Code, R) ->
    ?assertEqual(Status, maps:get(status, R)),
    B = jsone:decode(maps:get(body, R)),
    ?assertEqual(Code, maps:get(<<"code">>, maps:get(<<"error">>, B))),
    ?assertNot(maps:is_key(<<"put_url">>, B)),
    ?assertNot(maps:is_key(<<"upload">>, B)),
    B.
direct_put_descriptor() ->
    R = replay(reg()),
    ?assertEqual(200, maps:get(status, R)),
    #{
        <<"put_url">> := Url,
        <<"upload">> := #{
            <<"transport">> := <<"s3_put">>,
            <<"method">> := <<"PUT">>,
            <<"url">> := Url,
            <<"required_headers">> := #{<<"Content-Type">> := <<"text/plain">>}
        }
    } = jsone:decode(maps:get(body, R)),
    %% S3-only：无受控网关正文路径、无 JWT upload 能力、凭据快照不泄漏
    Body = jsone:decode(maps:get(body, R)),
    ?assertNot(maps:is_key(<<"gateway">>, Body)),
    ?assertNot(maps:is_key(<<"upload_capability">>, Body)),
    ?assertNot(maps:is_key(<<"storage_snapshot">>, Body)),
    ?assertEqual(<<"no-store">>, maps:get(<<"cache-control">>, maps:get(headers, R))),
    ?assertEqual(1, meck:num_calls(enterprise_internal_boundary, enforce, '_')),
    ?assertEqual(1, meck:num_calls(enterprise_asset_logic, presign_recovery_tx, '_')),
    ?assertEqual(1, meck:num_calls(attachment_storage, presign_put_snapshot, '_')).
location_unavailable() ->
    %% 恢复视图未携带已验证位置快照（位置漂移/历史行）→ 无能力签发
    meck:expect(enterprise_asset_logic, presign_recovery_tx, fun(_, _, _) ->
        {ok, maps:remove(<<"storage_snapshot">>, reg())}
    end),
    assert_error(503, <<"security_gate_closed">>, replay(reg())),
    ?assertEqual(0, meck:num_calls(attachment_storage, presign_put_snapshot, '_')).
presign_failure() ->
    meck:expect(attachment_storage, presign_put_snapshot, fun(_I, _Mime, _Ttl) ->
        {error, signing_failed}
    end),
    assert_error(503, <<"security_gate_closed">>, replay(reg())),
    ?assertEqual(1, meck:num_calls(attachment_storage, presign_put_snapshot, '_')).
legacy_cached_url() ->
    assert_error(
        409, <<"resource_conflict">>, replay(#{<<"put_url">> => <<"https://old.invalid/unsafe">>})
    ),
    ?assertEqual(0, meck:num_calls(attachment_storage, presign_put_snapshot, '_')).
fresh_registration() ->
    %% 无 client_upload_id 的旧调用方也先登记 V5 稳定会话，不返回裸 PUT URL。
    inserted(),
    meck:expect(enterprise_asset_logic, presign_tx, fun(_, _, Input) ->
        ?assertNot(maps:is_key(client_upload_id, Input)),
        {ok, reg()}
    end),
    R = invoke(),
    ?assertEqual(200, maps:get(status, R)),
    ?assert(maps:is_key(<<"put_url">>, jsone:decode(maps:get(body, R)))),
    ?assertEqual(1, meck:num_calls(enterprise_asset_logic, presign_recovery_tx, '_')).

registration_retry_signs_once() ->
    inserted(),
    put(registration_attempt, 0),
    meck:expect(enterprise_asset_logic, presign_tx, fun(_, _, _) ->
        N = get(registration_attempt) + 1,
        put(registration_attempt, N),
        case N of
            1 -> throw({abort_tx, synthetic_guard_conflict});
            _ -> {ok, reg()}
        end
    end),
    meck:expect(elib_pg, with_physical_guard_retry_tx, fun(F) ->
        try
            F(fixture_conn)
        catch
            throw:{abort_tx, synthetic_guard_conflict} -> F(fixture_conn)
        end
    end),
    ?assertEqual(200, maps:get(status, invoke())),
    ?assertEqual(2, get(registration_attempt)),
    ?assertEqual(1, meck:num_calls(enterprise_internal_idempotency, must_complete_tx, '_')),
    ?assertEqual(1, meck:num_calls(attachment_storage, presign_put_snapshot, '_')),
    erase(registration_attempt).

inserted() ->
    meck:expect(enterprise_internal_idempotency, begin_tx, fun(_, _, _, _, _) ->
        {ok, inserted}
    end),
    meck:expect(
        enterprise_internal_idempotency,
        must_complete_tx,
        fun(_, _, _, _, _, 200, Body) ->
            put(stored_json_body, jsone:decode(Body)),
            ok
        end
    ).

governance_null_times() ->
    Result = governance_response(undefined, null, undefined),
    ?assertEqual(null, maps:get(<<"purged_at">>, Result)),
    ?assertEqual(null, maps:get(<<"extended_until">>, Result)),
    ?assertEqual(null, maps:get(<<"retention_until">>, Result)).

governance_pg_times() ->
    Time = {{2026, 10, 23}, {1, 2, 3.25}},
    Result = governance_response(Time, Time, <<"2026-10-24T00:00:00Z">>),
    ?assertEqual(<<"2026-10-23T01:02:03.250Z">>, maps:get(<<"purged_at">>, Result)),
    ?assertEqual(<<"2026-10-23T01:02:03.250Z">>, maps:get(<<"extended_until">>, Result)),
    ?assertEqual(<<"2026-10-24T00:00:00Z">>, maps:get(<<"retention_until">>, Result)).

purge_uses_retry_transaction() ->
    meck:expect(elib_param, post, fun(_) -> #{<<"op">> => <<"purge">>} end),
    meck:expect(elib_pg, with_deadlock_retry_tx, fun(F) -> F(fixture_conn) end),
    _ = governance_response(null, null, null),
    ?assertEqual(1, meck:num_calls(elib_pg, with_deadlock_retry_tx, '_')),
    ?assertEqual(0, meck:num_calls(elib_pg, with_tx, '_')),
    ?assertEqual(1, meck:num_calls(enterprise_internal_boundary, enforce, '_')),
    ?assertEqual(1, meck:num_calls(enterprise_internal_idempotency, must_complete_tx, '_')).

hold_keeps_ordinary_transaction() ->
    meck:expect(elib_param, post, fun(_) -> #{<<"op">> => <<"hold">>} end),
    _ = governance_response(null, null, null),
    ?assertEqual(1, meck:num_calls(elib_pg, with_tx, '_')),
    ?assertEqual(0, meck:num_calls(elib_pg, with_deadlock_retry_tx, '_')).

purge_digest_conflict_preserves_response() ->
    meck:expect(elib_param, post, fun(_) -> #{<<"op">> => <<"purge">>} end),
    meck:expect(elib_pg, with_deadlock_retry_tx, fun(F) -> F(fixture_conn) end),
    meck:expect(enterprise_internal_idempotency, begin_tx, fun(_, _, _, _, _) ->
        {error, digest_conflict}
    end),
    meck:expect(enterprise_internal_idempotency, conflict_code, fun() ->
        <<"idempotency_conflict">>
    end),
    {ok, R, _} = enterprise_asset_handler:init(
        #{}, #{action => governance, enterprise_internal => #{}}
    ),
    assert_error(409, <<"idempotency_conflict">>, R),
    ?assertEqual(0, meck:num_calls(enterprise_asset_retention_logic, governance_tx, '_')),
    ?assertEqual(0, meck:num_calls(enterprise_internal_idempotency, must_complete_tx, '_')).

governance_response(PurgedAt, ExtendedUntil, RetentionUntil) ->
    inserted(),
    meck:expect(enterprise_internal_boundary, enforce, fun(_, _, <<"INT-22">>, _) -> ok end),
    meck:expect(enterprise_asset_retention_logic, governance_tx, fun(_, _, _) ->
        {ok, #{
            <<"file_id">> => 42,
            <<"object_key">> => <<"fixture/key">>,
            <<"purged">> => false,
            <<"purged_at">> => PurgedAt,
            <<"extended_until">> => ExtendedUntil,
            <<"retention_until">> => RetentionUntil
        }}
    end),
    {ok, R, _} = enterprise_asset_handler:init(
        #{},
        #{action => governance, enterprise_internal => #{}}
    ),
    ?assertEqual(200, maps:get(status, R)),
    Decoded = jsone:decode(maps:get(body, R)),
    ?assertEqual(Decoded, erase(stored_json_body)),
    Decoded.
transaction_failure() ->
    meck:expect(elib_pg, with_physical_guard_retry_tx, fun(_) -> {rollback, fixture_failure} end),
    assert_error(500, <<"internal_error">>, invoke()),
    ?assertEqual(0, meck:num_calls(attachment_storage, presign_put_snapshot, '_')).
confirmed_no_capability() ->
    Confirmed = #{
        <<"file_id">> => 42,
        <<"object_key">> => <<"fixture/key">>,
        <<"session_state">> => <<"confirmed">>,
        <<"can_resume">> => false,
        <<"retry_action">> => <<"none">>,
        <<"lifecycle_state">> => <<"deleted">>
    },
    meck:expect(enterprise_asset_logic, presign_recovery_tx, fun(_, _, _) -> {ok, Confirmed} end),
    application:unset_env(imboy, base_url),
    R = replay(reg()),
    ?assertEqual(200, maps:get(status, R)),
    ?assertEqual(Confirmed, jsone:decode(maps:get(body, R))),
    ?assertEqual(<<"no-store">>, maps:get(<<"cache-control">>, maps:get(headers, R))),
    ?assertEqual(0, meck:num_calls(attachment_storage, presign_put_snapshot, '_')).
terminal_conflict() ->
    meck:expect(enterprise_asset_logic, presign_recovery_tx, fun(_, _, _) ->
        {error, {<<"resource_conflict">>, session_not_renewable}}
    end),
    assert_error(409, <<"resource_conflict">>, replay(reg())),
    ?assertEqual(0, meck:num_calls(attachment_storage, presign_put_snapshot, '_')).
expired_recovery() ->
    meck:expect(enterprise_asset_logic, presign_recovery_tx, fun(_, _, _) ->
        {error, {<<"gone">>, session_expired}}
    end),
    R = replay(reg()),
    B = assert_error(410, <<"resource_conflict">>, R),
    ?assertEqual(<<"reupload_required">>, maps:get(<<"retry_action">>, B)),
    ?assertEqual(false, maps:get(<<"can_resume">>, B)),
    ?assertEqual(<<"no-store">>, maps:get(<<"cache-control">>, maps:get(headers, R))),
    ?assertEqual(0, meck:num_calls(attachment_storage, presign_put_snapshot, '_')).
recovery_unavailable() ->
    meck:expect(enterprise_asset_logic, presign_recovery_tx, fun(_, _, _) ->
        {error, {<<"security_gate_closed">>, recovery_unavailable}}
    end),
    B = assert_error(503, <<"security_gate_closed">>, replay(reg())),
    ?assertEqual(<<"retry_same_id">>, maps:get(<<"retry_action">>, B)),
    ?assertEqual(0, meck:num_calls(attachment_storage, presign_put_snapshot, '_')).
authorization_revoked() ->
    meck:expect(enterprise_internal_boundary, enforce, fun(_, _, _, _) ->
        {error, organization_boundary_violation}
    end),
    assert_error(403, <<"organization_boundary_violation">>, replay(reg())),
    ?assertEqual(0, meck:num_calls(enterprise_asset_logic, presign_recovery_tx, '_')),
    ?assertEqual(0, meck:num_calls(attachment_storage, presign_put_snapshot, '_')).
recovery_commit_unknown() ->
    meck:expect(elib_pg, with_tx, fun(F) ->
        _ = F(fixture_conn),
        {rollback, recovery_commit_unknown}
    end),
    assert_error(503, <<"security_gate_closed">>, replay(reg())),
    ?assertEqual(1, meck:num_calls(attachment_storage, presign_put_snapshot, '_')).
