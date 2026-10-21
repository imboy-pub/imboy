-module(micro_community_binding_parameters_pg_tests).
-include_lib("eunit/include/eunit.hrl").

%% Real PG parses both bind parameters in every profile. Only profile inputs are
%% mocked; SQL parsing, discussion locks and entitlement rows are real.
binding_parameters_test_() ->
    {timeout, 900,
        {setup, fun setup/0, fun cleanup/1, fun(S) ->
            C = maps:get(conn, S),
            [
                {"phase1 unbound eligibility retains both parameters", fun() ->
                    profile(phase1_delivery),
                    ?assert(micro_community_access_repo:eligible_tx(C, 99001012, 99001003))
                end},
                {"phase1 bound discussion remains denied", fun() ->
                    profile(phase1_delivery),
                    ?assertNot(micro_community_access_repo:eligible_tx(C, 99001011, 99001001))
                end},
                {"enabled owner remains entitled", fun() ->
                    profile(standard),
                    ?assert(micro_community_access_repo:eligible_tx(C, 99001011, 99001001))
                end},
                {"enabled unrelated person remains denied", fun() ->
                    profile(standard),
                    ?assertNot(micro_community_access_repo:eligible_tx(C, 99001011, 99001003))
                end},
                {"enabled subscriber and revoked subscriber differ", fun() ->
                    profile(standard),
                    ?assert(micro_community_access_repo:eligible_tx(C, 99001011, 99001002)),
                    execute(C, <<"UPDATE public.channel_subscription SET status=0 WHERE id=$1">>, [
                        99001041
                    ]),
                    ?assertNot(micro_community_access_repo:eligible_tx(C, 99001011, 99001002))
                end}
            ]
        end}}.

setup() ->
    S = inttest_marker_db:provision(#{env_prefix => <<"MICRO_BIND_ARITY_INTTEST">>}),
    try
        setup_rows(S)
    catch
        Class:Reason:Stack ->
            inttest_marker_db:release(S),
            erlang:raise(Class, Reason, Stack)
    end.

setup_rows(S) ->
    C = maps:get(conn, S),
    {ok, _, _} = epgsql:squery(C, <<"BEGIN">>),
    execute(
        C,
        <<"INSERT INTO public.\"user\"(id,account,password,reg_ip,reg_cosv) VALUES ($1,'bind-owner','synthetic-disabled','127.0.0.1','synthetic'),($2,'bind-subscriber','synthetic-disabled','127.0.0.1','synthetic'),($3,'bind-other','synthetic-disabled','127.0.0.1','synthetic')">>,
        [99001001, 99001002, 99001003]
    ),
    execute(
        C,
        <<"INSERT INTO public.\"group\"(id,owner_uid,creator_uid,title,type,join_limit,status,scope,e2ee_mode) VALUES ($1,$2,$2,'binding parameter fixture',2,3,1,'personal',0),($3,$4,$4,'binding parameter fixture unbound',2,3,1,'personal',0)">>,
        [99001011, 99001001, 99001012, 99001003]
    ),
    execute(
        C,
        <<"INSERT INTO public.channel(id,name,creator_uid,scope,status,discussion_group_id,access_type) VALUES ($1,'binding parameter fixture',$2,'personal',1,$3,0)">>,
        [99001021, 99001001, 99001011]
    ),
    execute(
        C,
        <<"INSERT INTO public.channel_subscription(id,channel_id,user_id,status) VALUES ($1,$2,$3,1)">>,
        [99001041, 99001021, 99001002]
    ),
    ok = meck:new(imboy_policy, [no_link]),
    ok = meck:new(imboy_feature, [no_link]),
    ok = meck:expect(imboy_feature, enabled, fun(channel) -> true end),
    S.

profile(Value) -> meck:expect(imboy_policy, current_profile, fun() -> Value end).

execute(C, Sql, Params) ->
    {ok, _} = epgsql:equery(C, Sql, Params),
    ok.

cleanup(S) ->
    try
        cleanup_mocks(),
        {ok, _, _} = epgsql:squery(maps:get(conn, S), <<"ROLLBACK">>)
    after
        inttest_marker_db:release(S)
    end.

cleanup_mocks() ->
    [meck:unload(M) || M <- [imboy_policy, imboy_feature], lists:member(M, meck:mocked())],
    ok.
