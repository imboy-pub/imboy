-module(attach_presign_registration_scope_tests).
-include_lib("eunit/include/eunit.hrl").

scope_registration_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun scope_ref_is_frozen_before_signing/0,
        fun revoked_group_never_registers/0,
        fun restored_identity_is_signed/0,
        fun confirmed_session_never_resigns/0,
        fun expired_session_never_resigns/0,
        fun old_client_gets_absolute_gateway_url/0,
        fun historical_unknown_location_never_resigns/0,
        fun recovery_rechecks_original_scope/0,
        fun confirm_cannot_replace_original_group/0,
        fun revoked_group_cannot_confirm/0
    ]}.

setup() ->
    lists:foreach(
        fun(M) -> meck:new(M, [non_strict, no_link]) end,
        [elib_oss, attach_pending_repo, group_member_ds, imboy_jwt]
    ),
    meck:expect(elib_oss, validate_file_type, fun(_) -> true end),
    meck:expect(elib_oss, build_object_key, fun(_, _, _, _) -> <<"new-proposed-key">> end),
    meck:expect(elib_oss, get_bucket, fun(_) -> <<"fixture">> end).

cleanup(_) ->
    lists:foreach(
        fun meck:unload/1,
        [elib_oss, attach_pending_repo, group_member_ds, imboy_jwt]
    ).

scope_ref_is_frozen_before_signing() ->
    meck:expect(group_member_ds, is_member, fun(11, 7) -> true end),
    meck:expect(attach_pending_repo, register_v5, fun(_, _, _, _, _, Registration) ->
        ?assertEqual(<<"11">>, maps:get(<<"scope_ref">>, Registration)),
        {error, snapshot_mismatch}
    end),
    ?assertEqual(
        {error, registration_failed},
        attach_logic:presign(
            7, <<"a.png">>, <<"image/png">>, <<"group">>, <<"11">>, cid()
        )
    ),
    ?assertEqual(0, meck:num_calls(elib_oss, presign_put_for_key, '_')).

revoked_group_never_registers() ->
    meck:expect(group_member_ds, is_member, fun(11, 7) -> false end),
    ?assertEqual(
        {error, forbidden},
        attach_logic:presign(
            7, <<"a.png">>, <<"image/png">>, <<"group">>, <<"11">>, cid()
        )
    ),
    ?assertEqual(0, meck:num_calls(attach_pending_repo, register_v5, '_')).

session(State, Expiry) ->
    #{
        object_key => <<"original-key">>,
        client_upload_id => cid(),
        session_state => State,
        storage_driver => <<"disk">>,
        storage_namespace => <<"fixture-ns">>,
        storage_container => <<"fixture">>,
        storage_locator_sha256 => binary:copy(<<"a">>, 64),
        expires_at => Expiry,
        deadline => Expiry + 600
    }.

presign(Session) ->
    meck:expect(attach_pending_repo, register_v5, fun(_, _, _, _, _, _) -> {ok, Session} end),
    attach_logic:presign(7, <<"a.png">>, <<"image/png">>, <<"private">>, undefined, cid()).

restored_identity_is_signed() ->
    Expiry = erlang:system_time(second) + 60,
    meck:expect(imboy_jwt, issue_upload_capability, fun(<<"original-key">>, <<"image/png">>, E) ->
        ?assertEqual(Expiry, E),
        {ok, <<"fixture-capability">>}
    end),
    {ok, Data} = presign(session(<<"registered">>, Expiry)),
    ?assertEqual(<<"original-key">>, maps:get(<<"object_key">>, Data)),
    ?assertEqual(Expiry, maps:get(<<"expires_at">>, Data)),
    ?assertEqual(1, meck:num_calls(imboy_jwt, issue_upload_capability, '_')).

confirmed_session_never_resigns() ->
    {ok, Data} = presign(session(<<"confirmed">>, erlang:system_time(second) + 60)),
    ?assertEqual(false, maps:is_key(<<"put_url">>, Data)),
    ?assertEqual(0, meck:num_calls(imboy_jwt, issue_upload_capability, '_')).

expired_session_never_resigns() ->
    ?assertEqual(
        {error, session_expired},
        presign(session(<<"registered">>, erlang:system_time(second) - 1))
    ),
    ?assertEqual(0, meck:num_calls(imboy_jwt, issue_upload_capability, '_')).

old_client_gets_absolute_gateway_url() ->
    Prior = application:get_env(imboy, base_url),
    application:set_env(imboy, base_url, <<"https://fixture.invalid/">>),
    Expiry = erlang:system_time(second) + 60,
    meck:expect(imboy_jwt, issue_upload_capability, fun(_, _, _) ->
        {ok, <<"fixture-capability">>}
    end),
    meck:expect(attach_pending_repo, register_v5, fun(_, _, _, _, undefined, _) ->
        {ok, session(<<"registered">>, Expiry)}
    end),
    try
        {ok, Data} = attach_logic:presign(
            7, <<"a.png">>, <<"image/png">>, <<"private">>, undefined
        ),
        ?assertMatch(
            <<"https://fixture.invalid/api/v1/attachment/gateway/", _/binary>>,
            maps:get(<<"put_url">>, Data)
        ),
        ?assertEqual(0, meck:num_calls(elib_oss, presign_put_for_key, '_'))
    after
        case Prior of
            {ok, V} -> application:set_env(imboy, base_url, V);
            undefined -> application:unset_env(imboy, base_url)
        end
    end.

historical_unknown_location_never_resigns() ->
    Unknown = maps:without(
        [storage_driver, storage_namespace, storage_container, storage_locator_sha256],
        session(<<"registered">>, erlang:system_time(second) + 60)
    ),
    ?assertEqual({error, historical_location_unverified}, presign(Unknown)),
    ?assertEqual(0, meck:num_calls(imboy_jwt, issue_upload_capability, '_')).

recovery_rechecks_original_scope() ->
    Row = #{
        <<"creator_user_id">> => 7,
        <<"scope">> => <<"group">>,
        <<"upload_registration">> => #{
            <<"subject">> => #{<<"kind">> => <<"user">>, <<"principal">> => <<"7">>},
            <<"declaration">> => #{<<"scope">> => <<"group">>, <<"scope_ref">> => <<"11">>}
        }
    },
    meck:expect(group_member_ds, is_member, fun(11, 7) -> true end),
    ?assertEqual(ok, attach_logic:reauthorize_upload_session(7, Row)),
    meck:expect(group_member_ds, is_member, fun(11, 7) -> false end),
    ?assertEqual({error, forbidden}, attach_logic:reauthorize_upload_session(7, Row)),
    ?assertEqual({error, forbidden}, attach_logic:reauthorize_upload_session(8, Row)).

confirm_cannot_replace_original_group() ->
    meck:expect(elib_oss, owner_of_key, fun(<<"original-key">>) -> {ok, 7} end),
    meck:expect(group_member_ds, is_member, fun(22, 7) -> true end),
    meck:expect(attach_pending_repo, status_lookup, fun(<<"original-key">>) ->
        {ok, #{
            <<"creator_user_id">> => 7,
            <<"scope">> => <<"group">>,
            <<"session_state">> => <<"registered">>,
            <<"client_upload_id">> => cid(),
            <<"upload_registration">> => #{
                <<"subject">> => #{<<"kind">> => <<"user">>, <<"principal">> => <<"7">>},
                <<"declaration">> => #{<<"scope">> => <<"group">>, <<"scope_ref">> => <<"11">>}
            }
        }}
    end),
    ?assertEqual(
        {error, scope_conflict},
        attach_logic:confirm(
            7, <<"original-key">>, <<"group">>, <<"22">>, #{}
        )
    ),
    ?assertEqual(0, meck:num_calls(elib_oss, object_sha256, '_')).

revoked_group_cannot_confirm() ->
    meck:expect(elib_oss, owner_of_key, fun(<<"original-key">>) -> {ok, 7} end),
    meck:expect(group_member_ds, is_member, fun(11, 7) -> false end),
    ?assertEqual(
        {error, forbidden},
        attach_logic:confirm(
            7, <<"original-key">>, <<"group">>, <<"11">>, #{}
        )
    ),
    ?assertEqual(0, meck:num_calls(attach_pending_repo, status_lookup, '_')).

cid() -> <<"a1910000-0000-4000-8000-000000000003">>.
