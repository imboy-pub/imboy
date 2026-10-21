-module(attach_direct_confirm_tests).
-include_lib("eunit/include/eunit.hrl").

confirm_test_() ->
    Success = [
        {"group guard and binary time", fun() -> confirm_success(<<"group">>) end},
        {"channel guard and binary time", fun() -> confirm_success(<<"channel">>) end},
        {"private null ref and binary time", fun confirm_private/0},
        {"unknown reason formatting", fun reason_format/0}
    ],
    Missing = [
        {lists:flatten(io_lib:format("~s missing ref ~p", [Scope, Ref])), fun() ->
            confirm_missing_ref(Scope, Ref)
        end}
     || Scope <- [<<"group">>, <<"channel">>], Ref <- [undefined, null, <<>>]
    ],
    Denied = [
        {lists:flatten(io_lib:format("~s guard rejects ~p", [Scope, Denial])), fun() ->
            confirm_denied(Scope, Denial)
        end}
     || Scope <- [<<"group">>, <<"channel">>], Denial <- [nonmember, archived]
    ],
    {foreach, fun setup/0, fun cleanup/1, Success ++ Missing ++ Denied}.

mock_modules() ->
    [
        elib_pg,
        attach_direct_attempt_repo,
        attachment_storage_garage,
        attachment_repo,
        workspace_resolver,
        attachment_ds
    ].

setup() ->
    [meck:new(Module, [no_link]) || Module <- lists:droplast(mock_modules())],
    meck:new(attachment_ds, [no_link, passthrough]).

cleanup(_) ->
    [meck:unload(Module) || Module <- lists:reverse(mock_modules())].

fixture(Scope, Ref) ->
    Row = #{
        <<"direct_state">> => <<"FINALIZING">>,
        <<"direct_expected">> => #{<<"size_bytes">> => 10, <<"sha256">> => sha()},
        <<"creator_user_id">> => 123,
        <<"scope">> => Scope,
        <<"upload_registration">> => #{<<"declaration">> => #{<<"scope_ref">> => Ref}},
        <<"storage_driver">> => <<"garage">>,
        <<"storage_namespace">> => <<"fixture-ns">>,
        <<"storage_container">> => <<"fixture-bucket">>,
        <<"storage_locator_sha256">> => <<"fixture-locator">>
    },
    Attempt = #{
        <<"attempt_state">> => <<"CREATED">>,
        <<"attempt_no">> => 1,
        <<"source_key">> => <<"fixture-source">>,
        <<"target_key">> => target_key(),
        <<"target_namespace">> => <<"fixture-ns">>,
        <<"target_container">> => <<"fixture-bucket">>
    },
    meck:expect(elib_pg, query, fun(_Sql, [ObjectKey]) ->
        ?assertEqual(object_key(), ObjectKey),
        {ok, [Row]}
    end),
    meck:expect(elib_pg, with_tx, fun(Transaction) ->
        try
            Transaction(self())
        catch
            throw:{abort_tx, Reason} -> {error, Reason}
        end
    end),
    meck:expect(
        attach_direct_attempt_repo,
        get_tx,
        fun(_Conn, _Key, 1) -> {ok, Attempt} end
    ),
    meck:expect(
        attach_direct_attempt_repo,
        mark_dispatched_tx,
        fun(_Conn, _Key, 1) -> ok end
    ),
    meck:expect(
        attach_direct_attempt_repo,
        record_outcome_tx,
        fun(_Conn, _Key, 1, copy_completed, undefined) -> ok end
    ),
    meck:expect(
        attach_direct_attempt_repo,
        record_proof_tx,
        fun(_Conn, _Key, 1, _Sha, 10) -> ok end
    ),
    meck:expect(
        attachment_storage_garage,
        copy_dispatch,
        fun(_Ns, _Container, _Source, _Target) -> {ok, #{}} end
    ),
    meck:expect(
        attachment_storage_garage,
        head_bounded,
        fun(_Ns, _Container, _Target) -> {ok, #{size => 10}} end
    ).

guard_fixture(Status, Member) ->
    meck:expect(
        attachment_repo,
        lock_upload_organization_tx,
        fun(_Conn, _Target, 123) -> ok end
    ),
    meck:expect(workspace_resolver, resolve_workspace, fun(_Target) -> {ok, 77} end),
    meck:expect(elib_pg, query, fun(Conn, Sql, [77]) ->
        ?assertEqual(self(), Conn),
        ?assertEqual(<<"SELECT status FROM workspace WHERE id = $1 FOR UPDATE">>, Sql),
        {ok, [#{<<"status">> => Status}]}
    end),
    meck:expect(
        attachment_repo,
        authorize_upload_scope_tx,
        fun(_Conn, _Kind, 42, 123) -> Member end
    ).

save_fixture(Scope, ExpectedRef) ->
    meck:expect(attachment_ds, save_confirmed_tx, fun(Conn, CreatedAt, 123, [Attach]) ->
        ?assertEqual(self(), Conn),
        ?assert(is_binary(CreatedAt)),
        ?assert(is_integer(calendar:rfc3339_to_system_time(binary_to_list(CreatedAt)))),
        ?assertEqual(Scope, maps:get(<<"scope">>, Attach)),
        ?assertEqual(ExpectedRef, maps:get(<<"scope_ref">>, Attach)),
        ?assertEqual(target_key(), maps:get(<<"storage_physical_key">>, Attach)),
        ?assertEqual(sha(), maps:get(<<"file_hash256">>, Attach)),
        case Scope of
            <<"group">> ->
                assert_guard_called(group);
            <<"channel">> ->
                assert_guard_called(channel);
            <<"private">> ->
                ?assertEqual(
                    0,
                    meck:num_calls(
                        attachment_ds, ensure_upload_scope_tx, '_'
                    )
                )
        end,
        ok
    end),
    meck:expect(
        attach_direct_attempt_repo,
        winner_pin_tx,
        fun(Conn, Key, 1, Target, Sha) ->
            ?assertEqual({self(), object_key(), target_key(), sha()}, {Conn, Key, Target, Sha}),
            ?assertEqual(1, meck:num_calls(attachment_ds, save_confirmed_tx, '_')),
            ok
        end
    ).

assert_guard_called(Kind) ->
    ?assertEqual(
        1,
        meck:num_calls(
            attachment_ds,
            ensure_upload_scope_tx,
            [self(), {Kind, <<"42">>}, 123]
        )
    ),
    ?assertEqual(
        1,
        meck:num_calls(
            attachment_repo,
            lock_upload_organization_tx,
            [self(), {Kind, 42}, 123]
        )
    ),
    ?assertEqual(1, meck:num_calls(workspace_resolver, resolve_workspace, [{Kind, 42}])),
    ?assertEqual(
        1,
        meck:num_calls(
            attachment_repo,
            authorize_upload_scope_tx,
            [self(), Kind, 42, 123]
        )
    ).

confirm_success(Scope) ->
    fixture(Scope, <<"42">>),
    guard_fixture(<<"active">>, true),
    save_fixture(Scope, <<"42">>),
    assert_confirmed().

confirm_private() ->
    fixture(<<"private">>, null),
    save_fixture(<<"private">>, null),
    assert_confirmed().

assert_confirmed() ->
    ?assertEqual(
        {ok, #{
            state => <<"CONFIRMED">>,
            attempt_id => 1,
            object_key => object_key(),
            size => 10
        }},
        attach_direct_logic:finalize(object_key(), 1)
    ),
    ?assertEqual(1, meck:num_calls(attach_direct_attempt_repo, winner_pin_tx, '_')),
    assert_mocks_valid().

confirm_missing_ref(Scope, Ref) ->
    fixture(Scope, Ref),
    ?assertEqual({error, missing_scope_ref}, attach_direct_logic:finalize(object_key(), 1)),
    ?assertEqual(0, meck:num_calls(attachment_ds, ensure_upload_scope_tx, '_')),
    ?assertEqual(0, meck:num_calls(attachment_repo, lock_upload_organization_tx, '_')),
    assert_no_confirmation().

confirm_denied(Scope, Denial) ->
    fixture(Scope, <<"42">>),
    Expected =
        case Denial of
            nonmember ->
                guard_fixture(<<"active">>, false),
                {error, forbidden};
            archived ->
                guard_fixture(<<"archived">>, true),
                workspace_guard:archived_error()
        end,
    ?assertEqual(Expected, attach_direct_logic:finalize(object_key(), 1)),
    ?assertEqual(1, meck:num_calls(attachment_repo, lock_upload_organization_tx, '_')),
    ExpectedAuthCalls =
        case Denial of
            nonmember -> 1;
            archived -> 0
        end,
    ?assertEqual(
        ExpectedAuthCalls,
        meck:num_calls(
            attachment_repo, authorize_upload_scope_tx, '_'
        )
    ),
    assert_no_confirmation().

assert_no_confirmation() ->
    ?assertEqual(0, meck:num_calls(attachment_ds, save_confirmed_tx, '_')),
    ?assertEqual(0, meck:num_calls(attach_direct_attempt_repo, winner_pin_tx, '_')),
    assert_mocks_valid().

reason_format() ->
    ?assertEqual(<<"{unexpected,42}">>, attach_direct_logic:reason_bin({unexpected, 42})),
    ?assertEqual(<<"copy_timeout">>, attach_direct_logic:reason_bin(copy_timeout)),
    ?assertEqual(<<"storage_status_503">>, attach_direct_logic:reason_bin({storage_status, 503})),
    ?assertEqual(<<"fixture">>, attach_direct_logic:reason_bin(<<"fixture">>)),
    assert_mocks_valid().

assert_mocks_valid() ->
    [?assert(meck:validate(Module)) || Module <- mock_modules()].

object_key() -> <<"direct-fixture.bin">>.
target_key() -> <<"direct-c/fixture">>.
sha() -> binary:copy(<<"a">>, 64).
