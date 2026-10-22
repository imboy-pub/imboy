-module(talk_record_results_relay_tests).
-include_lib("eunit/include/eunit.hrl").

results_security_test_() ->
    {timeout, 120,
        {setup, fun setup/0, fun talk_record_t19_support:teardown_all/1, fun(S) ->
            ?_test(run(S))
        end}}.
setup() ->
    S = talk_record_t19_support:setup_all(),
    talk_record_setup_fixture:prepare(
        S,
        fun() ->
            talk_record_t19_support:seed(S),
            S
        end,
        fun talk_record_t19_support:teardown_all/1
    ).

run(S) ->
    C = maps:get(conn, S),
    {ok, Rid} = talk_record_t15_support:create_org_record(700001, 710001, 0, 941),
    ok = talk_record_t15_support:add_grant(C, Rid, 700002, 700001, <<"viewer">>),
    {ok, P} = talk_record_transfer_candidates:for_record(700001, Rid, <<>>, undefined, 100),
    Users = [maps:get(<<"user_id">>, R) || R <- maps:get(<<"list">>, P)],
    ?assert(lists:member(700002, Users)),
    ?assertNot(lists:member(700001, Users)),
    ?assertNot(lists:member(700004, Users)),
    ?assertEqual(
        {error, forbidden},
        talk_record_transfer_candidates:for_record(700002, Rid, <<>>, undefined, 20)
    ),
    ?assertEqual(
        {error, not_found},
        talk_record_transfer_candidates:for_record(700004, Rid, <<>>, undefined, 20)
    ),
    workspace_candidates(C),
    complete_replay(C, Rid).

workspace_candidates(C) ->
    {ok, Rid} = talk_record_t15_support:create_org_record(700001, 710001, 720001, 942),
    ok = talk_record_t15_support:add_grant(C, Rid, 700002, 700001, <<"viewer">>),
    {ok, P} = talk_record_transfer_candidates:for_record(700001, Rid, <<>>, undefined, 100),
    ?assertNot(
        lists:any(fun(R) -> maps:get(<<"user_id">>, R) =:= 700002 end, maps:get(<<"list">>, P))
    ),
    {ok, _} = elib_pg:execute(
        C,
        <<
            "INSERT INTO workspace_member(workspace_id,user_id,status)"
            " VALUES(720001,700002,'active')"
        >>,
        []
    ),
    {ok, P2} = talk_record_transfer_candidates:for_record(700001, Rid, <<>>, undefined, 100),
    ?assert(
        lists:any(fun(R) -> maps:get(<<"user_id">>, R) =:= 700002 end, maps:get(<<"list">>, P2))
    ).

%% Synthetic historical success snapshot isolates the idempotency authorization
%% boundary. It is not evidence that fresh upload/assembly completed.
complete_replay(C, Rid) ->
    Cid = elib_tsid:generate(attachment),
    Key = <<"complete-security-replay">>,
    {ok, _} = elib_pg:execute(
        C,
        <<
            "INSERT INTO talk_record_capture(id,record_id,device_session_id,"
            "device_id,started_by,status) VALUES($1,$2,'11111111-2222-4333-8444-555555555555',"
            "'results-replay',700001,'ended')"
        >>,
        [Cid, Rid]
    ),
    Body = #{<<"captures">> => [#{<<"capture_id">> => Cid, <<"last_seq">> => 0}]},
    Resource = <<"/api/v1/talk-records/", (integer_to_binary(Rid))/binary, "/complete">>,
    {ok, Digest} = talk_record_logic:request_digest(<<"POST">>, Resource, Body),
    {ok, inserted} = elib_pg:with_tx(fun(X) ->
        talk_record_idempotency_repo:record_tx(
            X,
            700001,
            <<"POST">>,
            Resource,
            Key,
            Digest,
            elib_dt:add(elib_dt:now(), {86400, second})
        )
    end),
    ok = elib_pg:with_tx(fun(X) ->
        talk_record_idempotency_repo:complete_tx(
            X,
            700001,
            <<"POST">>,
            Resource,
            Key,
            jsone:encode(#{<<"payload">> => #{<<"job_id">> => 123}})
        )
    end),
    ?assertMatch({ok, replay, _}, talk_record_job_logic:complete(700001, Rid, <<"1">>, Key, Body)),
    {ok, _} = elib_pg:execute(
        C, <<"UPDATE talk_record_capture SET started_by=700002 WHERE id=$1">>, [Cid]
    ),
    ?assertEqual(
        {error, capture_not_found}, talk_record_job_logic:complete(700001, Rid, <<"1">>, Key, Body)
    ),
    {ok, _} = elib_pg:execute(
        C, <<"UPDATE talk_record_capture SET started_by=700001 WHERE id=$1">>, [Cid]
    ),
    {ok, _} = elib_pg:execute(C, <<"UPDATE talk_record SET revision=2 WHERE id=$1">>, [Rid]),
    ?assertMatch({ok, replay, _}, talk_record_job_logic:complete(700001, Rid, <<"1">>, Key, Body)),
    ?assertMatch({ok, _}, organization_owner_transfer:transfer(700001, 710001, 700003)),
    ?assertMatch({ok, _}, talk_record_ds:detail(700001, Rid)),
    {ok, _} = elib_pg:execute(
        C,
        <<
            "UPDATE organization_member SET status='suspended'"
            " WHERE organization_id=710001 AND user_id=700001"
        >>,
        []
    ),
    ?assertEqual(
        {error, not_found}, talk_record_job_logic:complete(700001, Rid, <<"1">>, Key, Body)
    ),
    {ok, _} = elib_pg:execute(
        C,
        <<
            "UPDATE organization_member SET status='active'"
            " WHERE organization_id=710001 AND user_id=700001"
        >>,
        []
    ),
    {ok, _} = elib_pg:execute(C, <<"UPDATE talk_record SET owner_user_id=700002 WHERE id=$1">>, [
        Rid
    ]),
    ?assertEqual(
        {error, not_found}, talk_record_job_logic:complete(700001, Rid, <<"1">>, Key, Body)
    ),
    {ok, _} = elib_pg:execute(
        C, <<"UPDATE talk_record SET owner_user_id=700001,deleted_at=now() WHERE id=$1">>, [Rid]
    ),
    ?assertEqual(
        {error, not_found}, talk_record_job_logic:complete(700001, Rid, <<"1">>, Key, Body)
    ).
