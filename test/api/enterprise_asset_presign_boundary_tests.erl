%%% Handler boundary only: transaction/provider are doubles, not HTTP acceptance.
-module(enterprise_asset_presign_boundary_tests).
-include_lib("eunit/include/eunit.hrl").

postcommit_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun absolute_gateway/0,
        fun missing_base/0,
        fun invalid_base/0,
        fun unknown_location/0,
        fun legacy_cached_url/0,
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
        imboy_jwt
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
        meck:expect(imboy_jwt, issue_upload_capability, fun(_, _, _) -> {ok, <<"cap">>} end),
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
    ok.
reg() ->
    #{
        <<"session_state">> => <<"registered">>,
        <<"client_upload_id">> => <<"00000000-0000-7000-8000-000000000001">>,
        <<"bucket">> => <<"fixture">>,
        <<"object_key">> => <<"fixture/key">>,
        <<"mime_type">> => <<"text/plain">>,
        <<"expires_at">> => 9999999999
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
    ?assertNot(maps:is_key(<<"upload_capability">>, B)),
    B.
absolute_gateway() ->
    R = replay(reg()),
    ?assertEqual(200, maps:get(status, R)),
    #{<<"put_url">> := Url} = jsone:decode(maps:get(body, R)),
    ?assertMatch(<<"https://files.example.invalid/api/v1/attachment/gateway/", _/binary>>, Url),
    ?assertEqual(<<"no-store">>, maps:get(<<"cache-control">>, maps:get(headers, R))),
    ?assertEqual(1, meck:num_calls(enterprise_internal_boundary, enforce, '_')),
    ?assertEqual(1, meck:num_calls(enterprise_asset_logic, presign_recovery_tx, '_')).
missing_base() ->
    application:unset_env(imboy, base_url),
    assert_error(503, <<"security_gate_closed">>, replay(reg())),
    ?assertEqual(0, meck:num_calls(imboy_jwt, issue_upload_capability, '_')).
invalid_base() ->
    lists:foreach(
        fun(Base) ->
            application:set_env(imboy, base_url, Base),
            assert_error(503, <<"security_gate_closed">>, replay(reg()))
        end,
        [<<"files.internal">>, <<"/files">>, <<"ftp://files.invalid">>]
    ),
    ?assertEqual(0, meck:num_calls(imboy_jwt, issue_upload_capability, '_')).
unknown_location() ->
    meck:expect(enterprise_asset_logic, presign_recovery_tx, fun(_, _, _) ->
        {error, {<<"resource_conflict">>, recovery_unverified}}
    end),
    assert_error(409, <<"resource_conflict">>, replay(reg())),
    ?assertEqual(0, meck:num_calls(imboy_jwt, issue_upload_capability, '_')).
legacy_cached_url() ->
    assert_error(
        409, <<"resource_conflict">>, replay(#{<<"put_url">> => <<"https://old.invalid/unsafe">>})
    ),
    ?assertEqual(0, meck:num_calls(imboy_jwt, issue_upload_capability, '_')).
transaction_failure() ->
    meck:expect(elib_pg, with_tx, fun(_) -> {rollback, fixture_failure} end),
    assert_error(500, <<"internal_error">>, invoke()),
    ?assertEqual(0, meck:num_calls(imboy_jwt, issue_upload_capability, '_')).
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
    ?assertEqual(0, meck:num_calls(imboy_jwt, issue_upload_capability, '_')).
terminal_conflict() ->
    meck:expect(enterprise_asset_logic, presign_recovery_tx, fun(_, _, _) ->
        {error, {<<"resource_conflict">>, session_not_renewable}}
    end),
    assert_error(409, <<"resource_conflict">>, replay(reg())),
    ?assertEqual(0, meck:num_calls(imboy_jwt, issue_upload_capability, '_')).
expired_recovery() ->
    meck:expect(enterprise_asset_logic, presign_recovery_tx, fun(_, _, _) ->
        {error, {<<"gone">>, session_expired}}
    end),
    R = replay(reg()),
    B = assert_error(410, <<"resource_conflict">>, R),
    ?assertEqual(<<"reupload_required">>, maps:get(<<"retry_action">>, B)),
    ?assertEqual(false, maps:get(<<"can_resume">>, B)),
    ?assertEqual(<<"no-store">>, maps:get(<<"cache-control">>, maps:get(headers, R))),
    ?assertEqual(0, meck:num_calls(imboy_jwt, issue_upload_capability, '_')).
recovery_unavailable() ->
    meck:expect(enterprise_asset_logic, presign_recovery_tx, fun(_, _, _) ->
        {error, {<<"security_gate_closed">>, recovery_unavailable}}
    end),
    B = assert_error(503, <<"security_gate_closed">>, replay(reg())),
    ?assertEqual(<<"retry_same_id">>, maps:get(<<"retry_action">>, B)),
    ?assertEqual(0, meck:num_calls(imboy_jwt, issue_upload_capability, '_')).
authorization_revoked() ->
    meck:expect(enterprise_internal_boundary, enforce, fun(_, _, _, _) ->
        {error, organization_boundary_violation}
    end),
    assert_error(403, <<"organization_boundary_violation">>, replay(reg())),
    ?assertEqual(0, meck:num_calls(enterprise_asset_logic, presign_recovery_tx, '_')),
    ?assertEqual(0, meck:num_calls(imboy_jwt, issue_upload_capability, '_')).
recovery_commit_unknown() ->
    meck:expect(elib_pg, with_tx, fun(F) ->
        case get(recovery_tx_seen) of
            undefined ->
                put(recovery_tx_seen, true),
                F(fixture_conn);
            true ->
                _ = F(fixture_conn),
                {rollback, recovery_commit_unknown}
        end
    end),
    assert_error(503, <<"security_gate_closed">>, replay(reg())),
    ?assertEqual(1, meck:num_calls(imboy_jwt, issue_upload_capability, '_')).
