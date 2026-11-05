-module(ai_agent_avatar_binding_tests).
-include_lib("eunit/include/eunit.hrl").

avatar_binding_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun(_) -> fun update_binds_and_saves_in_same_transaction/0 end,
        fun(_) -> fun update_rejects_foreign_session/0 end,
        fun(_) -> fun update_rejects_profile_write_failure/0 end,
        fun(_) -> fun create_saves_and_binds_uploaded_avatar/0 end,
        fun(_) -> fun create_rejects_unbound_local_avatar/0 end,
        fun(_) -> fun reject_untrusted_admin_actor/0 end,
        fun(_) -> fun preserve_existing_external_avatar_only/0 end,
        fun(_) -> fun bind_alias_atomically/0 end
    ]}.

setup() ->
    Mods = [elib_pg, ai_agent_repo, user_repo, elib_tsid, elib_oss, attachment_avatar_alias_repo],
    [meck:new(M, [passthrough, no_link]) || M <- Mods],
    meck:expect(elib_pg, with_tx, fun(F) ->
        try
            F(fake_conn)
        catch
            throw:{abort_tx, Reason} -> {error, Reason}
        end
    end),
    meck:expect(elib_tsid, generate, fun(_) -> 123 end),
    meck:expect(ai_agent_repo, patch_tx, fun(fake_conn, 123, _) ->
        {ok, [#{<<"user_id">> => 123}]}
    end),
    meck:expect(ai_agent_repo, upsert_tx, fun(fake_conn, _) -> {ok, [#{<<"user_id">> => 123}]} end),
    meck:expect(user_repo, create_tx, fun(fake_conn, _) -> {ok, 123} end),
    meck:expect(user_repo, update_tx, fun(fake_conn, 123, _) -> {ok, 1} end),
    meck:expect(elib_oss, public_base_url, fun() -> <<"https://public.example">> end),
    meck:expect(elib_oss, public_url_for_key, fun(Key) ->
        <<"https://public.example/", Key/binary>>
    end),
    Mods.

cleanup(Mods) ->
    [meck:unload(M) || M <- Mods],
    ok.

config() ->
    #{
        admin_actor_id => 81,
        <<"nickname">> => <<"agent">>,
        <<"provider">> => <<"provider">>,
        <<"avatar">> => <<"https://public.example/app123/avatar/test">>,
        <<"avatar_object_key">> => <<"app123/avatar/test">>,
        <<"avatar_client_upload_id">> => <<"5f0e9c1e-1111-4222-8333-444455556666">>
    }.

update_binds_and_saves_in_same_transaction() ->
    meck:expect(ai_agent_repo, bind_avatar_tx, fun(fake_conn, _, _, 123, false, 81) -> ok end),
    ?assertMatch({ok, _}, ai_agent_ds:update(123, config())),
    ?assert(
        meck:called(user_repo, update_tx, [
            fake_conn,
            123,
            #{nickname => <<"agent">>, avatar => <<"https://public.example/app123/avatar/test">>}
        ])
    ).

update_rejects_foreign_session() ->
    meck:expect(ai_agent_repo, bind_avatar_tx, fun(_, _, _, _, _, _) ->
        {error, avatar_session_mismatch}
    end),
    ?assertMatch({error, _}, ai_agent_ds:update(123, config())),
    ?assertEqual(0, meck:num_calls(user_repo, update_tx, '_')).

update_rejects_profile_write_failure() ->
    meck:expect(ai_agent_repo, bind_avatar_tx, fun(_, _, _, _, _, _) -> ok end),
    meck:expect(user_repo, update_tx, fun(_, _, _) -> {error, failed} end),
    ?assertMatch({error, _}, ai_agent_ds:update(123, config())).

create_saves_and_binds_uploaded_avatar() ->
    meck:expect(ai_agent_repo, bind_avatar_tx, fun(fake_conn, _, _, 123, true, 81) -> ok end),
    ?assertMatch({ok, _}, ai_agent_ds:create(config())),
    ?assert(
        meck:called(user_repo, update_tx, [
            fake_conn,
            123,
            #{avatar => <<"https://public.example/app123/avatar/test">>}
        ])
    ).

create_rejects_unbound_local_avatar() ->
    Cfg = maps:without([<<"avatar_object_key">>, <<"avatar_client_upload_id">>], config()),
    ?assertMatch({error, _}, ai_agent_ds:create(Cfg)).

reject_untrusted_admin_actor() ->
    Config = (maps:remove(admin_actor_id, config()))#{<<"admin_actor_id">> => 81},
    ?assertMatch({error, _}, ai_agent_ds:update(123, Config)),
    ?assertEqual(0, meck:num_calls(user_repo, update_tx, '_')).

preserve_existing_external_avatar_only() ->
    Existing = <<"https://external.example/old.png">>,
    meck:expect(elib_pg, query, fun(fake_conn, _, [123, Avatar]) ->
        case Avatar of
            Existing -> {ok, [#{<<"present">> => 1}]};
            _ -> {ok, []}
        end
    end),
    Legacy = maps:without([<<"avatar_object_key">>, <<"avatar_client_upload_id">>], config()),
    ?assertMatch({ok, _}, ai_agent_ds:update(123, Legacy#{<<"avatar">> => Existing})),
    [
        ?assertMatch({error, _}, ai_agent_ds:update(123, Legacy#{<<"avatar">> => New}))
     || New <- [
            <<"https://PUBLIC.example/app123/avatar/test">>,
            <<"https://public.example/%61pp123/avatar/test">>,
            <<"https://external.example/new.png">>
        ]
    ],
    ?assertMatch({error, _}, ai_agent_ds:create(Legacy#{<<"avatar">> => Existing})).

bind_alias_atomically() ->
    Alias = <<"11111111-2222-4333-8444-555555555555">>,
    Url = <<"https://public.example/_avatar/", Alias/binary>>,
    meck:expect(attachment_avatar_alias_repo, bind_tx, fun(fake_conn, _, _, 123, false, 81) ->
        {ok, Alias}
    end),
    meck:expect(attachment_avatar_alias_repo, public_url, fun(Alias) -> {ok, Url} end),
    meck:expect(attachment_avatar_alias_repo, revoke_other_tx, fun(fake_conn, 123, _) -> ok end),
    ?assertMatch({ok, _}, ai_agent_ds:update(123, (config())#{<<"avatar">> => Url})),
    ?assert(meck:called(attachment_avatar_alias_repo, revoke_other_tx, '_')),
    ?assertMatch(
        {error, _}, ai_agent_ds:update(123, (config())#{<<"avatar">> => <<"https://evil.invalid">>})
    ).
