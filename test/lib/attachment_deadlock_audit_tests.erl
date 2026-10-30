-module(attachment_deadlock_audit_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

audit_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun(_) -> ?_test(v5_unwraps_only_deadlock()) end,
        fun(_) -> ?_test(legacy_preserves_audit_wrapper()) end,
        fun(_) -> ?_test(v5_preserves_non_deadlock_wrapper()) end
    ]}.

setup() ->
    meck:new([attachment_repo, enterprise_audit_event_repo], [no_link, non_strict]),
    meck:expect(attachment_repo, confirmation_row_tx, fun(_, _) ->
        case get(audit_lookup) of
            undefined ->
                put(audit_lookup, called),
                {ok, not_found};
            called ->
                {ok, row()}
        end
    end),
    meck:expect(attachment_repo, confirm_if_absent_tx, fun(_, _, _, _) -> {ok, 3, true} end),
    meck:expect(
        enterprise_audit_event_repo,
        append_scope_file_tx,
        fun(_, _, _, _) -> {error, {audit_failed, get(audit_error)}} end
    ),
    ok.
cleanup(_) ->
    meck:unload(attachment_repo),
    meck:unload(enterprise_audit_event_repo).

row() ->
    #{
        <<"creator_user_id">> => 2,
        <<"scope">> => <<"group">>,
        <<"scope_ref">> => <<"1">>,
        <<"cipher">> => null,
        <<"id">> => 3
    }.
attach() ->
    #{
        <<"path">> => <<"synthetic">>,
        <<"scope">> => <<"group">>,
        <<"scope_ref">> => <<"1">>,
        <<"cipher">> => null
    }.
err(Code) ->
    #error{
        severity = error,
        code = Code,
        codename = unknown,
        message = <<"synthetic">>,
        extra = []
    }.

v5_unwraps_only_deadlock() ->
    E = err(<<"40P01">>),
    put(audit_error, E),
    erase(audit_lookup),
    ?assertThrow(
        {abort_tx, E}, attachment_ds:save_confirmed_tx_retryable(fake, <<"now">>, 2, [attach()])
    ).
legacy_preserves_audit_wrapper() ->
    E = err(<<"40P01">>),
    put(audit_error, E),
    erase(audit_lookup),
    ?assertThrow(
        {abort_tx, {audit_failed, E}},
        attachment_ds:save_confirmed_tx(fake, <<"now">>, 2, [attach()])
    ).
v5_preserves_non_deadlock_wrapper() ->
    E = err(<<"23505">>),
    put(audit_error, E),
    erase(audit_lookup),
    ?assertThrow(
        {abort_tx, {audit_failed, E}},
        attachment_ds:save_confirmed_tx_retryable(fake, <<"now">>, 2, [attach()])
    ).
