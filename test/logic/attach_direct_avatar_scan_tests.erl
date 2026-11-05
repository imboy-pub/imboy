-module(attach_direct_avatar_scan_tests).
-include_lib("eunit/include/eunit.hrl").

avatar_scan_test_() ->
    {setup,
        fun() ->
            Mods = [
                elib_pg,
                attachment_avatar_alias_repo,
                attachment_edge_redirect,
                attachment_storage,
                attachment_worker_port,
                attachment_storage_garage
            ],
            [meck:new(M, [passthrough, no_link]) || M <- Mods],
            Sha = binary:copy(<<"a">>, 64),
            Row = #{
                <<"status">> => 1,
                <<"lifecycle_state">> => <<"live">>,
                <<"mime_type">> => <<"image/png">>,
                <<"file_hash256">> => Sha,
                <<"size">> => 10
            },
            meck:expect(elib_pg, query, fun(_, _) ->
                {ok, [
                    #{
                        <<"direct_state">> => <<"CONFIRMED">>,
                        <<"upload_registration">> => #{
                            <<"subject">> => #{
                                <<"kind">> => <<"application">>,
                                <<"org_id">> => <<"0">>,
                                <<"principal">> => <<"agent">>
                            }
                        }
                    }
                ]}
            end),
            meck:expect(attachment_edge_redirect, edge_ready, fun() -> true end),
            meck:expect(attachment_avatar_alias_repo, reserve, fun(_) ->
                {ok, Row, #{<<"state">> => <<"unpublished">>}}
            end),
            meck:expect(attachment_avatar_alias_repo, publish, fun(_, _) ->
                {ok, <<"11111111-2222-4333-8444-555555555555">>}
            end),
            meck:expect(attachment_avatar_alias_repo, public_url, fun(_) ->
                {ok, <<"https://s3.example/_avatar/11111111-2222-4333-8444-555555555555">>}
            end),
            Cfg = #{
                endpoint => <<"http://localhost">>, access_key => <<"a">>, secret_key => <<"b">>
            },
            meck:expect(attachment_storage, frozen_location, fun(_, _) ->
                {ok, #{
                    namespace => <<"ns">>,
                    container => <<"private">>,
                    key => <<"winner">>,
                    garage_config => Cfg
                }}
            end),
            meck:expect(attachment_storage_garage, copy_cross, fun(_, _, _, _, _) ->
                error(unexpected_copy)
            end),
            {Mods, Sha}
        end,
        fun({Mods, _}) -> [meck:unload(M) || M <- Mods] end, fun({_, Sha}) ->
            [
                ?_test(begin
                    meck:expect(attachment_worker_port, scan_external, fun(_) -> Result end),
                    meck:reset(attachment_avatar_alias_repo),
                    Actual = attach_direct_logic:publish_avatar(
                        <<"appagent/avatar/logical">>, <<"agent">>, <<>>
                    ),
                    case Expected of
                        success -> ?assertMatch({ok, _}, Actual);
                        _ -> ?assertEqual(Expected, Actual)
                    end,
                    ?assertEqual(Calls, meck:num_calls(attachment_avatar_alias_repo, publish, '_')),
                    ?assertEqual(0, meck:num_calls(attachment_storage_garage, copy_cross, '_'))
                end)
             || {Result, Expected, Calls} <- [
                    {{ok, #{verified => true, sha256 => Sha, size => 10}}, success, 1},
                    {
                        {ok, #{verified => true, sha256 => Sha, size => 11}},
                        {error, avatar_proof_mismatch},
                        0
                    },
                    {{error, timeout}, {error, {avatar_scan_failed, timeout}}, 0}
                ]
            ]
        end}.

avatar_subject_bound_test_() ->
    {setup, fun() -> meck:new(elib_pg, [passthrough, no_link]) end,
        fun(_) -> meck:unload(elib_pg) end, fun(_) ->
            ?_test(begin
                Subject = #{
                    <<"kind">> => <<"application">>,
                    <<"org_id">> => <<"0">>,
                    <<"principal">> => <<"123">>
                },
                Build = fun(S) ->
                    #{
                        <<"direct_state">> => <<"CONFIRMED">>,
                        <<"upload_registration">> => #{<<"subject">> => S}
                    }
                end,
                meck:expect(elib_pg, query, fun(_, _) -> {ok, [Build(Subject)]} end),
                ?assertEqual(
                    ok, attach_direct_logic:require_application_session(<<"app123/avatar/a">>)
                ),
                ?assertEqual(
                    {error, not_application_session},
                    attach_direct_logic:require_application_session(<<"app456/avatar/a">>)
                ),
                ?assertEqual(
                    {error, not_application_session},
                    attach_direct_logic:publish_avatar(<<"app123/avatar/a">>, <<"456">>, <<>>)
                ),
                ?assertEqual(
                    {error, not_application_session},
                    attach_direct_logic:publish_avatar(<<"app123/avatar/a">>, <<>>, <<>>)
                ),
                meck:expect(elib_pg, query, fun(_, _) ->
                    {ok, [Build(Subject#{<<"org_id">> => <<"42">>})]}
                end),
                ?assertEqual(
                    {error, not_application_session},
                    attach_direct_logic:require_application_session(<<"app123/avatar/a">>)
                )
            end)
        end}.
