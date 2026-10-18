%%% Unit coverage of authoritative recovery; HTTP coverage is in wiring tests.
-module(enterprise_asset_recovery_tests).
-include_lib("eunit/include/eunit.hrl").

recovery_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun registered_frozen_window/0, fun confirmed_original_result/0,
        fun terminal_states/0, fun foreign_subject/0, fun foreign_key/0,
        fun missing_or_unknown/0, fun missing_confirmation/0,
        fun expired_window/0, fun deletion_intent/0, fun unverified_location/0
    ]}.
setup() ->
    meck:new(enterprise_asset_repo, [passthrough, no_link]),
    meck:new(attachment_storage, [no_link]),
    meck:expect(attachment_storage, frozen_location, fun(_, _) -> {ok, #{driver => disk}} end),
    row_result({ok, row()}),
    ok.
cleanup(_) ->
    meck:unload(attachment_storage),
    meck:unload(enterprise_asset_repo).
ctx() -> #{organization_id => 1, application_id => 2}.
key() -> <<"eoa/1/2/fixture/file.txt">>.
cached() -> #{<<"object_key">> => key(), <<"client_upload_id">> => <<"fixture-id">>,
    <<"session_state">> => <<"registered">>, <<"expires_at">> => 1}.
row() ->
    #{<<"object_key">> => key(), <<"bucket">> => <<"fixture">>,
        <<"scope">> => <<"enterprise">>, <<"client_upload_id">> => <<"fixture-id">>,
        <<"session_state">> => <<"registered">>, <<"deadline">> => 9999999999,
        <<"expires_at">> => 9999999998, <<"deletion_pending">> => false,
        <<"upload_registration">> => #{
            <<"subject">> => #{<<"kind">> => <<"application">>, <<"principal">> => <<"2">>, <<"org_id">> => <<"1">>},
            <<"declaration">> => #{<<"mime">> => <<"text/plain">>}}}.
row_result(Result) ->
    meck:expect(enterprise_asset_repo, recovery_session_tx, fun(fixture_conn, _) -> Result end).
recover() -> enterprise_asset_logic:presign_recovery_tx(fixture_conn, ctx(), cached()).
assert_denied(Code) ->
    ?assertMatch({error, {Code, _}}, recover()),
    ?assertEqual(0, meck:num_calls(attachment_storage, frozen_location, '_')).
registered_frozen_window() ->
    {ok, R} = recover(),
    ?assertEqual(9999999998, maps:get(<<"expires_at">>, R)),
    ?assertEqual(9999999999, maps:get(<<"session_deadline">>, R)),
    ?assertEqual(key(), maps:get(<<"object_key">>, R)),
    ?assertEqual(<<"text/plain">>, maps:get(<<"mime_type">>, R)),
    ?assertNot(maps:is_key(<<"put_url">>, R)),
    ?assertEqual(1, meck:num_calls(attachment_storage, frozen_location, [key(), row()])).
confirmed_row(Life) ->
    (row())#{<<"session_state">> => <<"confirmed">>, <<"confirmed_attachment_id">> => 42,
        <<"asset_id">> => 42, <<"asset_scope">> => <<"enterprise">>,
        <<"asset_scope_ref">> => <<"org:1/app:2">>, <<"asset_size">> => 1024,
        <<"asset_mime_type">> => <<"text/plain">>, <<"asset_lifecycle_state">> => Life}.
confirmed_original_result() ->
    lists:foreach(fun(Life) ->
        row_result({ok, confirmed_row(Life)}),
        {ok, R} = recover(),
        ?assertEqual(42, maps:get(<<"file_id">>, R)),
        ?assertEqual(key(), maps:get(<<"object_key">>, R)),
        ?assertEqual(1024, maps:get(<<"size">>, R)),
        ?assertEqual(Life, maps:get(<<"lifecycle_state">>, R)),
        ?assertEqual(false, maps:get(<<"can_resume">>, R)),
        ?assertEqual(<<"none">>, maps:get(<<"retry_action">>, R)),
        ?assertNot(maps:is_key(<<"put_url">>, R)),
        ?assertNot(maps:is_key(<<"upload_capability">>, R))
    end, [<<"live">>, <<"delete_intent">>, <<"deleted">>]),
    ?assertEqual(0, meck:num_calls(attachment_storage, frozen_location, '_')).
terminal_states() ->
    lists:foreach(fun(State) ->
        row_result({ok, (row())#{<<"session_state">> => State}}),
        assert_denied(<<"resource_conflict">>)
    end, [<<"rejected">>, <<"delete_intent">>, <<"deleted">>]).
foreign_subject() ->
    lists:foreach(fun(R) -> row_result({ok, R}), assert_denied(<<"resource_not_found">>) end,
        [(row())#{<<"upload_registration">> => #{<<"subject">> => #{<<"principal">> => <<"3">>}}},
         (row())#{<<"client_upload_id">> => <<"different-id">>}, (row())#{<<"scope">> => <<"private">>}]).
foreign_key() ->
    ?assertMatch({error, {<<"resource_not_found">>, _}},
        enterprise_asset_logic:presign_recovery_tx(fixture_conn, ctx(),
            (cached())#{<<"object_key">> => <<"eoa/1/3/foreign">>})),
    ?assertEqual(0, meck:num_calls(enterprise_asset_repo, recovery_session_tx, '_')).
missing_or_unknown() ->
    row_result({error, not_found}), assert_denied(<<"resource_not_found">>),
    row_result({error, connection_lost}), assert_denied(<<"security_gate_closed">>),
    row_result({ok, (row())#{<<"session_state">> => <<"unknown">>}}),
    assert_denied(<<"security_gate_closed">>).
missing_confirmation() ->
    lists:foreach(fun(R) -> row_result({ok, R}), assert_denied(<<"security_gate_closed">>) end,
        [(confirmed_row(<<"live">>))#{<<"asset_id">> => null},
         (confirmed_row(<<"live">>))#{<<"asset_id">> => 43},
         (confirmed_row(<<"live">>))#{<<"asset_scope_ref">> => <<"org:1/app:3">>}]).
expired_window() ->
    row_result({ok, (row())#{<<"expires_at">> => 1}}), assert_denied(<<"gone">>),
    row_result({ok, (row())#{<<"deadline">> => 1}}), assert_denied(<<"gone">>),
    row_result({ok, (row())#{<<"deadline">> => null}}), assert_denied(<<"security_gate_closed">>).
deletion_intent() ->
    row_result({ok, (row())#{<<"deletion_pending">> => true}}),
    assert_denied(<<"resource_conflict">>).
unverified_location() ->
    meck:expect(attachment_storage, frozen_location, fun(_, _) -> {ok, legacy_unknown} end),
    ?assertMatch({error, {<<"resource_conflict">>, recovery_unverified}}, recover()),
    meck:expect(attachment_storage, frozen_location, fun(_, _) -> {error, location_changed} end),
    ?assertMatch({error, {<<"security_gate_closed">>, _}}, recover()).

recovery_repo_test_() ->
    {foreach, fun() -> meck:new(elib_pg, [no_link]) end,
        fun(_) -> meck:unload(elib_pg) end, [fun lock_before_read/0, fun lock_failure_no_read/0]}.
lock_before_read() ->
    meck:expect(elib_pg, query, fun(fixture_conn, Sql, [Key]) ->
        ?assertEqual(key(), Key),
        case Sql of
            <<"SELECT fn_v5_lock_ordinary_upload($1)">> -> {ok, []};
            _ ->
                ?assertEqual(1, meck:num_calls(elib_pg, query,
                    [fixture_conn, <<"SELECT fn_v5_lock_ordinary_upload($1)">>, [key()]])),
                ?assertNotEqual(nomatch, binary:match(Sql, <<"FROM attach_pending p LEFT JOIN attachment a">>)),
                {ok, [(row())#{<<"upload_registration">> => jsone:encode(maps:get(<<"upload_registration">>, row()))}]}
        end
    end),
    ?assertEqual({ok, row()}, enterprise_asset_repo:recovery_session_tx(fixture_conn, key())),
    ?assertEqual(2, meck:num_calls(elib_pg, query, '_')).
lock_failure_no_read() ->
    meck:expect(elib_pg, query, fun(fixture_conn, <<"SELECT fn_v5_lock_ordinary_upload($1)">>, [_]) ->
        {error, serialization_failure}
    end),
    ?assertEqual({error, serialization_failure}, enterprise_asset_repo:recovery_session_tx(fixture_conn, key())),
    ?assertEqual(1, meck:num_calls(elib_pg, query, '_')).
